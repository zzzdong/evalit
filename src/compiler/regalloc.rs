use std::{
    collections::{HashMap, HashSet},
    fmt,
    hash::Hash,
};

use log::trace;
use petgraph::Direction::Outgoing;

use super::ir::instruction::{Block, BlockId, ControlFlowGraph, Instruction, Value};
use crate::bytecode::{MIN_REQUIRED_REGISTER, Register};

#[derive(Debug, Clone)]
pub struct LiveRange {
    start: usize,
    end: usize,
}

#[derive(Debug, Clone)]
pub struct LiveInterval {
    var: Value,
    start: usize,
    end: usize,
    ranges: Vec<LiveRange>,
    reg: Option<Register>,
    stack: Option<usize>,
}

impl LiveInterval {
    pub fn new(var: Value) -> Self {
        LiveInterval {
            var,
            start: usize::MAX,
            end: 0,
            ranges: Vec::new(),
            reg: None,
            stack: None,
        }
    }

    pub fn active(&mut self, index: usize, at_block_start: bool) {
        match self.ranges.last_mut() {
            Some(last) => {
                if at_block_start {
                    self.ranges.push(LiveRange {
                        start: index,
                        end: index,
                    });
                } else if last.end == index - 1 {
                    last.end = last.end.max(index);
                } else {
                    self.ranges.push(LiveRange {
                        start: index,
                        end: index,
                    });
                }
            }
            None => {
                self.ranges.push(LiveRange {
                    start: index,
                    end: index,
                });
            }
        }
        self.start = self.start.min(index);
        self.end = self.end.max(index);
    }

    pub fn update_end(&mut self, index: usize) {
        self.end = self.end.max(index);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Liveness {
    intervals: HashMap<Value, LiveInterval>,
}

impl Liveness {
    fn new() -> Self {
        Liveness {
            intervals: HashMap::new(),
        }
    }

    fn intervals(&self) -> Vec<LiveInterval> {
        self.intervals.values().cloned().collect()
    }

    fn set_register(&mut self, var: Value, reg: Register) {
        self.intervals.get_mut(&var).unwrap().reg.replace(reg);
    }

    fn set_stack(&mut self, var: Value, stack: usize) {
        self.intervals.get_mut(&var).unwrap().stack.replace(stack);
    }

    fn stack_size(&self) -> usize {
        let stacks = self
            .intervals
            .values()
            .filter(|interval| interval.stack.is_some());

        if stacks.clone().count() == 0 {
            return 0;
        }

        stacks
            .map(|interval| interval.stack.unwrap())
            .max()
            .unwrap_or(0)
            + 1
    }
}

pub struct LiveIntervalAnalyzer {}

impl LiveIntervalAnalyzer {
    pub fn scan(cfg: &ControlFlowGraph) -> Liveness {
        // 1. 遍历一次控制流图，生成基本存活区间
        let mut liveness = Self::build_basic_intervals(cfg);

        // 第二遍：计算live_in和live_out集合
        let (_live_in_sets, live_out_sets) = Self::compute_liveness_sets(cfg);

        // 第三遍：更新变量存活周期
        Self::update_intervals(cfg, &live_out_sets, &mut liveness);

        liveness
    }

    /// 第一遍：扫描所有指令，建立基本的LiveInterval
    fn build_basic_intervals(cfg: &ControlFlowGraph) -> Liveness {
        let mut liveness = Liveness::new();
        let mut index = 0;

        for block in cfg.blocks.iter() {
            let block_start = index;

            for inst in block.instructions.iter() {
                Self::process_instruction(&mut liveness, index, inst, index == block_start);
                index += 1;
            }

            // let block_end = index - 1;
            // Self::leave_block(&mut liveness, block_start, block_end);
        }

        liveness
    }

    /// 处理指令中的变量，更新LiveInterval
    fn process_instruction(
        liveness: &mut Liveness,
        index: usize,
        inst: &Instruction,
        at_block_start: bool,
    ) {
        let (defined, used) = inst.defined_and_used_vars();

        // 处理使用的变量（在定义之前处理使用）
        for var in used {
            if matches!(var, Value::Variable(_)) {
                liveness
                    .intervals
                    .entry(var)
                    .or_insert(LiveInterval::new(var))
                    .active(index, at_block_start);
            }
        }

        // 处理定义的变量
        for var in defined {
            if matches!(var, Value::Variable(_)) {
                liveness
                    .intervals
                    .entry(var)
                    .or_insert(LiveInterval::new(var))
                    .active(index, at_block_start);
            }
        }
    }

    /// 第二遍：计算每个块的live_in和live_out集合
    fn compute_liveness_sets(
        cfg: &ControlFlowGraph,
    ) -> (
        HashMap<BlockId, HashSet<Value>>,
        HashMap<BlockId, HashSet<Value>>,
    ) {
        let mut live_in_sets = HashMap::new();
        let mut live_out_sets = HashMap::new();
        let mut changed = true;

        // 初始化每个块的live_in和live_out为空集合
        for block in cfg.blocks.iter() {
            live_in_sets.insert(block.id, HashSet::new());
            live_out_sets.insert(block.id, HashSet::new());
        }

        while changed {
            changed = false;

            // 从后向前遍历基本块
            for block in cfg.blocks.iter().rev() {
                // 计算当前块的live_out（从后继块的live_in获取）
                let mut new_live_out = Self::compute_block_liveness(cfg, block, &live_in_sets);

                // 从后向前扫描指令
                for inst in block.instructions.iter().rev() {
                    let (defined, used) = inst.defined_and_used_vars();

                    // 先从live_out中移除定义的变量
                    for var in defined {
                        if matches!(var, Value::Variable(_)) {
                            new_live_out.remove(&var);
                        }
                    }

                    // 添加使用的变量到live_out（这些变量在此指令之前必须是活跃的）
                    for var in used {
                        if matches!(var, Value::Variable(_)) {
                            new_live_out.insert(var);
                        }
                    }
                }

                // live_in就是扫描完所有指令后的live_out
                let new_live_in: HashSet<Value> = new_live_out.clone();

                // 检查是否有变化
                let old_live_in = live_in_sets.get(&block.id).unwrap();
                let old_live_out = live_out_sets.get(&block.id).unwrap();

                if &new_live_in != old_live_in || &new_live_out != old_live_out {
                    changed = true;
                    live_in_sets.insert(block.id, new_live_in);
                    live_out_sets.insert(block.id, new_live_out);
                }
            }
        }

        (live_in_sets, live_out_sets)
    }

    /// 计算单个基本块的live_out（从后继块的live_in获取）
    fn compute_block_liveness(
        cfg: &ControlFlowGraph,
        block: &Block,
        live_in_sets: &HashMap<BlockId, HashSet<Value>>,
    ) -> HashSet<Value> {
        let mut live_out = HashSet::new();

        // 获取当前块在图中的节点索引
        if let Some(&node_index) = cfg.block_node_map.get(&block.id) {
            // 获取所有后继块
            let successors = cfg
                .graph
                .neighbors_directed(node_index, petgraph::Direction::Outgoing);

            // 遍历所有后继块
            for succ_node in successors {
                // 获取后继块的BlockId
                let succ_block_id = cfg.graph[succ_node];

                // 将后继块的live_in中的所有变量添加到当前块的live_out中
                if let Some(succ_live_in) = live_in_sets.get(&succ_block_id) {
                    live_out.extend(succ_live_in.iter().cloned());
                }
            }
        }

        live_out
    }

    /// 第三遍：更新变量的存活周期
    fn update_intervals(
        cfg: &ControlFlowGraph,
        live_out_sets: &HashMap<BlockId, HashSet<Value>>,
        liveness: &mut Liveness,
    ) {
        // 计算每个块的起始和结束索引
        let mut block_starts = Vec::with_capacity(cfg.blocks.len());
        let mut current_index = 0;

        for block in cfg.blocks.iter() {
            block_starts.push(current_index);
            current_index += block.instructions.len();
        }

        // 遍历所有块，更新变量的存活周期
        for (block_id, block) in cfg.blocks.iter().enumerate() {
            let block_start = block_starts[block_id];
            let block_end = block_start + block.instructions.len();

            // 获取当前块的live_out集合
            if let Some(live_out) = live_out_sets.get(&block.id) {
                // 更新live_out中变量的存活周期
                for &var in live_out {
                    if let Some(interval) = liveness.intervals.get_mut(&var) {
                        // 检查块是否在循环中
                        if let Some(&node_index) = cfg.block_node_map.get(&block.id) {
                            let successors = cfg.graph.neighbors_directed(node_index, Outgoing);

                            let is_loop = successors.clone().any(|succ_node| {
                                let succ_block_id = cfg.graph[succ_node];
                                // 检查是否有后继指向自己或之前的块
                                if let Some(succ_block) =
                                    cfg.blocks.iter().position(|b| b.id == succ_block_id)
                                {
                                    succ_block <= block_id
                                } else {
                                    false
                                }
                            });

                            if is_loop {
                                // 在循环中，确保变量在整个循环范围内都是活跃的
                                interval.update_end(block_end);

                                // 对于循环中的每个后继块，延长变量的活跃期
                                for succ_node in successors {
                                    let succ_block_id = cfg.graph[succ_node];
                                    if let Some(succ_block) =
                                        cfg.blocks.iter().position(|b| b.id == succ_block_id)
                                        && succ_block <= block_id
                                    {
                                        interval.update_end(block_starts[succ_block]);
                                    }
                                }
                            } else {
                                interval.update_end(block_end);
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct RegAlloc {
    liveness: Liveness,
    pub(super) reg_set: RegisterSet,
}

impl RegAlloc {
    pub fn new(registers: &[Register]) -> Self {
        if registers.len() <= MIN_REQUIRED_REGISTER {
            panic!("Not enough registers");
        }
        Self {
            liveness: Liveness::new(),
            reg_set: RegisterSet::new(registers),
        }
    }

    pub fn load_arg(&mut self, arg: usize) -> isize {
        0 - ((arg as isize) + 1)
    }

    pub fn in_use_registers(&self) -> Vec<Register> {
        self.reg_set
            .registers
            .iter()
            .filter(|reg| reg.variable.is_some())
            .map(|reg| reg.register)
            .collect()
    }

    pub fn stack_size(&self) -> usize {
        self.liveness.stack_size()
    }

    pub fn alloc(&mut self, value: Value, index: usize) -> (Register, Option<Action>) {
        trace!("allocating {value}");

        let interval = self.liveness.intervals.get(&value).unwrap();

        match self.reg_set.find(value) {
            Some(register) => (register, None),
            None => match interval.reg {
                Some(register) => {
                    self.reg_set.use_register(register, value, true);
                    (register, None)
                }
                None => {
                    let reg = self.reg_set.must_alloc(value);

                    if interval
                        .ranges
                        .iter()
                        .any(|range| range.start == index && interval.start != index)
                    {
                        let spill = Action::Restore {
                            stack: interval.stack.unwrap(),
                            register: reg,
                        };

                        return (reg, Some(spill));
                    }

                    (reg, None)
                }
            },
        }
    }

    pub fn release(&mut self, value: Value, index: usize) -> Option<Action> {
        trace!("releasing {value}");

        if !matches!(value, Value::Variable(_)) {
            return None;
        }

        let interval = self.liveness.intervals.get(&value).unwrap();
        if interval.ranges.iter().any(|range| range.end == index)
            && let Some(stack) = interval.stack
            && let Some(register) = self.reg_set.release(value)
        {
            let spill = Action::Spill { register, stack };
            return Some(spill);
        }

        None
    }

    pub fn arrange(&mut self, cfg: &ControlFlowGraph) {
        self.liveness = LiveIntervalAnalyzer::scan(cfg);

        let registers: Vec<Register> = self
            .reg_set
            .registers
            .iter()
            .map(|reg| reg.register)
            .collect();

        let mut intervals = self.liveness.intervals();

        // 按开始时间排序，优先处理长区间
        intervals.sort_by(|a, b| {
            let a_len = a.end - a.start;
            let b_len = b.end - b.start;
            a.start.cmp(&b.start).then(b_len.cmp(&a_len))
        });

        // 1. 分组
        let mut groups: Vec<Vec<LiveInterval>> = Vec::new();
        for interval in intervals {
            let mut placed = false;
            for group in groups.iter_mut() {
                if Self::can_join_group(&interval, group) {
                    group.push(interval.clone());
                    placed = true;
                    break;
                }
            }
            if !placed {
                groups.push(vec![interval.clone()]);
            }
        }

        // 2. 分配寄存器
        // 2.1 如果组数量不多于可用寄存器数量，则直接分配
        if groups.len() <= registers.len() {
            for (group, reg) in groups.into_iter().zip(registers.iter()) {
                for interval in group {
                    self.liveness.set_register(interval.var, *reg);
                }
            }
            return;
        }

        // 2.2 保留3个临时寄存器，其他的进行优先级分配
        let (_temp_regs, fixed_regs) = registers.split_at(3);

        // 排序
        groups.sort_by(|a, b| {
            // let a_len: usize = a.iter().map(|interval| interval.end - interval.start).sum();
            // let b_len: usize = b.iter().map(|interval| interval.end - interval.start).sum();
            // b_len.cmp(&a_len)

            let a_len: usize = a.iter().map(|interval| interval.ranges.len()).sum();
            let b_len: usize = b.iter().map(|interval| interval.ranges.len()).sum();
            b_len.cmp(&a_len)
        });

        for (i, group) in groups.iter().enumerate() {
            trace!("Group[{i}]: {group:?}");
            // let vars = group.iter().map(|interval| interval.var).collect::<Vec<_>>();
            // trace!("Group[{i}]: {vars:?}");
        }

        // 2.2.1 分配固定寄存器
        let (fixed_group, temp_group) = groups.split_at(fixed_regs.len());
        for (group, reg) in fixed_group.iter().zip(fixed_regs) {
            for interval in group {
                self.liveness.set_register(interval.var, *reg);
            }
        }

        // 2.2.2 分配临时寄存器，只分配栈上空间，不分配寄存器
        for (i, group) in temp_group.iter().enumerate() {
            for interval in group {
                self.liveness.set_stack(interval.var, i);
            }
        }
    }

    fn can_join_group(interval: &LiveInterval, group: &[LiveInterval]) -> bool {
        group
            .iter()
            .all(|existing| !Self::has_overlap(interval, existing))
    }

    fn has_overlap(interval_a: &LiveInterval, interval_b: &LiveInterval) -> bool {
        // 直接使用interval的start和end来判断重叠
        interval_a.start <= interval_b.end && interval_b.start <= interval_a.end
    }
}

#[derive(Debug, Clone)]
pub(super) struct RegisterSet {
    registers: Vec<RegisterHold>,
}

impl RegisterSet {
    fn new(registers: &[Register]) -> Self {
        let registers = registers
            .iter()
            .map(|addr| RegisterHold::new(*addr))
            .collect();
        Self { registers }
    }

    fn must_alloc(&mut self, value: Value) -> Register {
        let reg = self
            .registers
            .iter_mut()
            .find(|reg| reg.variable.is_none())
            .unwrap();
        reg.variable = Some(value);
        reg.register
    }

    fn release(&mut self, value: Value) -> Option<Register> {
        match self
            .registers
            .iter_mut()
            .find(|reg| reg.variable == Some(value))
        {
            Some(reg) => {
                reg.variable = None;
                Some(reg.register)
            }
            None => None,
        }
    }

    fn use_register(&mut self, register: Register, variable: Value, is_fixed: bool) {
        for reg in self.registers.iter_mut() {
            if reg.register == register {
                reg.variable = Some(variable);
                reg.is_fixed = is_fixed;
                return;
            }
        }
    }

    fn find(&self, variable: Value) -> Option<Register> {
        self.registers
            .iter()
            .find(|reg| reg.variable == Some(variable))
            .map(|reg| reg.register)
    }
}

impl fmt::Display for RegisterSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for register in self.registers.iter() {
            match register.variable {
                Some(var) => write!(f, "{var}"),
                None => write!(f, "-"),
            }?;
            write!(f, "\t|")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct RegisterHold {
    register: Register,
    variable: Option<Value>,
    is_fixed: bool,
}

impl RegisterHold {
    fn new(register: Register) -> Self {
        Self {
            register,
            variable: None,
            is_fixed: false,
        }
    }
}

pub enum Action {
    Restore { stack: usize, register: Register },
    Spill { register: Register, stack: usize },
}
