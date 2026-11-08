//! 寄存器分配：精确活跃区间分组着色 + 固定栈槽交接。
//!
//! 跨块正确性依赖这几条不变量：
//! 1. 每个变量一个**固定**栈槽，槽位一经分配永不变更；栈槽是跨块的权威值。
//! 2. 寄存器只是块内的加速副本：块入口清空 `reg_set`，块出口把绑定的值
//!    全部写回栈槽；块参数（phi）由每条边写槽交接。单趟、按布局顺序的
//!    codegen 无法知道真正执行的是哪条前驱路径，编译期 `reg_set` 在块入口
//!    不可信，所以块边界只经内存交接。
//! 3. `alloc` 返回按序执行的动作列表（先 Spill 被驱逐者，再 Restore 目标），
//!    调用方必须全部发射；旧实现只取一个动作，被驱逐的值会被静默丢掉。
//! 4. 同一条 IR 指令内已生成的操作数寄存器被锁定，驱逐时不得覆盖。
//!
//! 预留：AOT 阶段应把「寄存器 / 栈槽」抽象成 `Location`，这里分配的 R0..R15
//! 是虚拟机寄存器，重定位到物理寄存器的入口留在本模块；异常边一旦引入，
//! handler 入口同样走「清空 reg_set + 从栈槽 Restore」这条路径。

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

use log::trace;

use super::ir::{BlockId, ControlFlowGraph};
use crate::{
    bytecode::{MIN_REQUIRED_REGISTER, RETURN_REGISTER, Register},
    compiler::ir::{cfg::BlockLayout, instruction::Variable},
};

#[derive(Debug, Clone, Copy)]
pub struct LiveRange {
    start: usize,
    end: usize,
}

#[derive(Debug, Clone)]
pub struct LiveInterval {
    var: Variable,
    start: usize,
    end: usize,
    ranges: Vec<LiveRange>,
    reg: Option<Register>,
    stack: Option<usize>,
}

impl LiveInterval {
    fn new(var: Variable) -> Self {
        LiveInterval {
            var,
            start: usize::MAX,
            end: 0,
            ranges: Vec::new(),
            reg: None,
            stack: None,
        }
    }

    /// 用精确活跃索引重建区间。旧实现只在 def/use 打点、靠 `update_end`
    /// 拉长末端，跨块 live-through 的索引全是空洞；这里逐索引记录后合并。
    fn rebuild(&mut self, indices: &BTreeSet<usize>) {
        self.ranges.clear();
        self.start = usize::MAX;
        self.end = 0;

        for &index in indices {
            match self.ranges.last_mut() {
                Some(last) if last.end + 1 == index => last.end = index,
                _ => self.ranges.push(LiveRange {
                    start: index,
                    end: index,
                }),
            }
        }

        if let Some(first) = self.ranges.first() {
            self.start = first.start;
        }
        if let Some(last) = self.ranges.last() {
            self.end = last.end;
        }
    }

    /// 精确重叠判定：任一活跃区间相交即冲突，不再只比整体 [start, end]。
    fn overlaps(&self, other: &LiveInterval) -> bool {
        self.ranges.iter().any(|a| {
            other
                .ranges
                .iter()
                .any(|b| a.start <= b.end && b.start <= a.end)
        })
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct Liveness {
    intervals: BTreeMap<Variable, LiveInterval>,
    next_stack: usize,
}

impl Liveness {
    fn interval_mut(&mut self, var: Variable) -> &mut LiveInterval {
        self.intervals
            .entry(var)
            .or_insert_with(|| LiveInterval::new(var))
    }

    fn vars(&self) -> Vec<Variable> {
        self.intervals.keys().copied().collect()
    }

    fn intervals(&self) -> Vec<LiveInterval> {
        self.intervals.values().cloned().collect()
    }

    fn set_register(&mut self, var: Variable, reg: Register) {
        self.interval_mut(var).reg = Some(reg);
    }

    fn stack(&self, var: Variable) -> Option<usize> {
        self.intervals.get(&var).and_then(|interval| interval.stack)
    }

    /// 槽位一经分配永不变更：同一条指令流里读写必须落在同一个槽上。
    fn ensure_stack(&mut self, var: Variable) -> usize {
        if let Some(slot) = self.stack(var) {
            return slot;
        }
        let slot = self.next_stack;
        self.next_stack += 1;
        self.interval_mut(var).stack = Some(slot);
        slot
    }

    fn stack_size(&self) -> usize {
        self.next_stack
    }
}

/// 块索引区间 + 每个块的 live_out 集合。
type LivenessSets = (
    BTreeMap<BlockId, (usize, usize)>,
    BTreeMap<BlockId, BTreeSet<Variable>>,
);

pub struct LiveIntervalAnalyzer;

impl LiveIntervalAnalyzer {
    pub fn scan(cfg: &ControlFlowGraph, layout: &BlockLayout) -> Liveness {
        let (block_ranges, live_out) = Self::compute_liveness_sets(cfg, layout);
        Self::build_intervals(cfg, layout, &block_ranges, &live_out)
    }

    /// 标准块级数据流：`live_out[b] = ∪ live_in[succ]`，
    /// `live_in[b] = (live_out[b] - defs) ∪ uses`。块参数在入口定义，不穿透。
    ///
    /// 旧实现把反向扫描后的集合同时写进 `live_in` 和 `live_out`，导致
    /// 「live_out」里存的其实是 live_in；这里分开保存。
    fn compute_liveness_sets(cfg: &ControlFlowGraph, layout: &BlockLayout) -> LivenessSets {
        let mut block_ranges = BTreeMap::new();
        let mut index = 0;
        for block in layout.iter(cfg) {
            let start = index;
            index += block.instructions().len();
            block_ranges.insert(block.id(), (start, index));
        }

        let mut live_in: BTreeMap<BlockId, BTreeSet<Variable>> = BTreeMap::new();
        let mut live_out: BTreeMap<BlockId, BTreeSet<Variable>> = BTreeMap::new();
        for block in layout.iter(cfg) {
            live_in.insert(block.id(), BTreeSet::new());
            live_out.insert(block.id(), BTreeSet::new());
        }

        loop {
            let mut changed = false;
            for block in layout.iter_rev(cfg) {
                let mut out = BTreeSet::new();
                for &succ in cfg.get_successors(block.id()) {
                    if let Some(set) = live_in.get(&succ) {
                        out.extend(set.iter().copied());
                    }
                }
                for param in block.params() {
                    out.remove(param);
                }

                let mut cur = out.clone();
                for inst in block.instructions().iter().rev() {
                    let (defined, used) = inst.defined_and_used_vars();
                    for var in defined {
                        cur.remove(&var);
                    }
                    for var in used {
                        cur.insert(var);
                    }
                }
                for param in block.params() {
                    cur.remove(param);
                }

                if live_in.get(&block.id()) != Some(&cur) {
                    live_in.insert(block.id(), cur);
                    changed = true;
                }
                if live_out.get(&block.id()) != Some(&out) {
                    live_out.insert(block.id(), out);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }

        (block_ranges, live_out)
    }

    /// 逐块后向存活扫描：以 `live_out` 为种子，把变量在**真正活跃的每条
    /// 指令索引**上都记录下来，再合并成连续区间。
    fn build_intervals(
        cfg: &ControlFlowGraph,
        layout: &BlockLayout,
        block_ranges: &BTreeMap<BlockId, (usize, usize)>,
        live_out: &BTreeMap<BlockId, BTreeSet<Variable>>,
    ) -> Liveness {
        let mut active: BTreeMap<Variable, BTreeSet<usize>> = BTreeMap::new();

        for block in layout.iter(cfg) {
            let (start, _) = block_ranges[&block.id()];
            let mut live = live_out.get(&block.id()).cloned().unwrap_or_default();

            for (i, inst) in block.instructions().iter().enumerate().rev() {
                let (defined, used) = inst.defined_and_used_vars();
                for var in defined {
                    live.remove(&var);
                }
                for var in used {
                    live.insert(var);
                }
                for var in &live {
                    active.entry(*var).or_default().insert(start + i);
                }
            }

            // 块参数与入口活跃值在块首即活跃（空块也要有区间）。
            for param in block.params() {
                active.entry(*param).or_default().insert(start);
            }
            for var in live {
                active.entry(var).or_default().insert(start);
            }
        }

        let mut liveness = Liveness::default();
        for (var, indices) in active {
            liveness.interval_mut(var).rebuild(&indices);
        }
        liveness
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
            liveness: Liveness::default(),
            reg_set: RegisterSet::new(registers),
        }
    }

    pub fn load_arg(&mut self, arg: usize) -> isize {
        0 - ((arg as isize) + 1)
    }

    /// 调用点需要备份的寄存器。返回值寄存器是 caller-clobbered：调用前由
    /// `spill_return_register` 落栈，调用后不能再 Pop 回来覆盖返回值。
    pub fn in_use_registers(&self) -> Vec<Register> {
        self.reg_set
            .registers
            .iter()
            .filter(|reg| reg.variable.is_some() && reg.register != RETURN_REGISTER)
            .map(|reg| reg.register)
            .collect()
    }

    pub fn stack_size(&self) -> usize {
        self.liveness.stack_size()
    }

    /// 变量的固定栈槽；缺失时按需分配，绝不 unwrap。
    pub fn stack_slot(&mut self, var: Variable) -> usize {
        self.liveness.ensure_stack(var)
    }

    /// 块入口：编译期寄存器状态跨块不可信，全部解绑。
    pub fn begin_block(&mut self) {
        self.reg_set.clear();
    }

    /// 块参数（phi）已经由某条边写入栈槽，寄存器里的旧副本作废。
    pub fn invalidate(&mut self, var: Variable) {
        self.reg_set.release(var);
    }

    /// 调用前：返回值寄存器会被被调函数覆写。若它当前绑定了变量，先把值
    /// 写回固定栈槽并解绑，调用方之后从槽 Restore，不会丢值。
    pub fn spill_return_register(&mut self) -> Option<Action> {
        let var = self.reg_set.holder(RETURN_REGISTER)?;
        let stack = self.stack_slot(var);
        self.reg_set.release(var);
        Some(Action::Spill {
            register: RETURN_REGISTER,
            stack,
        })
    }

    /// 块出口：把当前绑定在寄存器中的变量全部写回各自的固定栈槽。
    /// 这同时保证终止符里的跳转实参读到的是最新值。
    pub fn spill_all_bound(&mut self) -> Vec<Action> {
        let holds = self.reg_set.registers.clone();
        let mut actions = Vec::new();
        for hold in holds {
            if let Some(var) = hold.variable {
                let stack = self.stack_slot(var);
                actions.push(Action::Spill {
                    register: hold.register,
                    stack,
                });
            }
        }
        actions
    }

    /// 分配寄存器，返回**按序**执行的动作：先 Spill 被驱逐者，再 Restore 目标。
    /// `locked` 是本条 IR 指令已经生成的操作数寄存器，驱逐时必须跳过。
    pub fn alloc(&mut self, var: Variable, locked: &[Register]) -> (Register, Vec<Action>) {
        self.alloc_inner(var, locked, true)
    }

    /// 定义位置：值马上会被覆盖，只分配位置与必要的驱逐动作，不发 Restore。
    /// 调用返回值的目标必须走这里，否则 Restore 会抢先覆写 R0 里的返回值。
    pub fn alloc_def(&mut self, var: Variable, locked: &[Register]) -> (Register, Vec<Action>) {
        self.alloc_inner(var, locked, false)
    }

    fn alloc_inner(
        &mut self,
        var: Variable,
        locked: &[Register],
        restore: bool,
    ) -> (Register, Vec<Action>) {
        if let Some(register) = self.reg_set.find(var) {
            return (register, Vec::new());
        }

        let stack = self.stack_slot(var);
        // 静态分配的固定寄存器若正被本条指令的某个操作数占着，不能抢占：
        // 否则第二个 gen_operand 会把第一个操作数的值覆盖掉。此时退化为
        // 动态分配（free/evictable 都会跳过 locked）。
        let fixed = self
            .liveness
            .intervals
            .get(&var)
            .and_then(|i| i.reg)
            .filter(|reg| {
                !locked.contains(reg)
                    || self.reg_set.holder(*reg).is_none()
                    || self.reg_set.holder(*reg) == Some(var)
            });
        let mut actions = Vec::new();

        let register = match fixed {
            Some(reg) => {
                if let Some(holder) = self.reg_set.holder(reg)
                    && holder != var
                {
                    let holder_stack = self.stack_slot(holder);
                    actions.push(Action::Spill {
                        register: reg,
                        stack: holder_stack,
                    });
                    self.reg_set.release(holder);
                }
                reg
            }
            None => match self.reg_set.free(locked) {
                Some(reg) => reg,
                None => {
                    // 找不到空闲寄存器就驱逐一个非锁定、优先非固定的占用者。
                    // 一条 IR 指令最多锁定 3 个操作数，16 个通用寄存器下
                    // evictable 不会为空；这里仍给一个确定性兜底，绝不 panic。
                    match self.reg_set.evictable(locked) {
                        Some(victim) => {
                            if let Some(holder) = victim.variable {
                                let holder_stack = self.stack_slot(holder);
                                actions.push(Action::Spill {
                                    register: victim.register,
                                    stack: holder_stack,
                                });
                                self.reg_set.release(holder);
                            }
                            victim.register
                        }
                        None => self.reg_set.registers[0].register,
                    }
                }
            },
        };

        self.reg_set.bind(register, var, fixed.is_some());
        if restore {
            // 栈槽是权威值：绑定建立后从槽恢复；定义位置跳过，避免覆盖
            // 即将写入的返回值（多出的 Mov 由 golden 快照记录）。
            actions.push(Action::Restore { stack, register });
        }
        (register, actions)
    }

    pub fn arrange(&mut self, cfg: &ControlFlowGraph, block_layout: &BlockLayout) {
        self.liveness = LiveIntervalAnalyzer::scan(cfg, block_layout);

        // 1. 每个变量一个固定栈槽，按 Variable id 升序，确定性。
        for var in self.liveness.vars() {
            self.liveness.ensure_stack(var);
        }

        // 2. 精确区间分组着色。
        let registers: Vec<Register> = self
            .reg_set
            .registers
            .iter()
            .map(|reg| reg.register)
            .collect();

        let mut intervals = self.liveness.intervals();
        intervals.retain(|interval| !interval.ranges.is_empty());
        intervals.sort_by_key(|interval| (interval.start, interval.var.id()));

        let mut groups: Vec<Vec<LiveInterval>> = Vec::new();
        for interval in intervals {
            match groups
                .iter_mut()
                .find(|group| group.iter().all(|existing| !interval.overlaps(existing)))
            {
                Some(group) => group.push(interval),
                None => groups.push(vec![interval]),
            }
        }

        // 寄存器够用时全部分配；不够时保留前 MIN_REQUIRED_REGISTER 个作临时
        // 寄存器池，其余按活跃区间密度优先分配，剩下的组只走栈槽。
        let fixed_regs: &[Register] = if groups.len() <= registers.len() {
            &registers
        } else {
            groups.sort_by(|a, b| {
                let a_ranges: usize = a.iter().map(|interval| interval.ranges.len()).sum();
                let b_ranges: usize = b.iter().map(|interval| interval.ranges.len()).sum();
                (b_ranges, a[0].var.id()).cmp(&(a_ranges, b[0].var.id()))
            });
            &registers[MIN_REQUIRED_REGISTER..]
        };

        for (i, group) in groups.iter().enumerate() {
            trace!("Group[{i}]: {group:?}");
            if let Some(&reg) = fixed_regs.get(i) {
                for interval in group {
                    self.liveness.set_register(interval.var, reg);
                }
            }
        }

        self.reg_set.clear();
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

    fn clear(&mut self) {
        for hold in &mut self.registers {
            hold.variable = None;
            hold.is_fixed = false;
        }
    }

    fn find(&self, variable: Variable) -> Option<Register> {
        self.registers
            .iter()
            .find(|hold| hold.variable == Some(variable))
            .map(|hold| hold.register)
    }

    fn holder(&self, register: Register) -> Option<Variable> {
        self.registers
            .iter()
            .find(|hold| hold.register == register)
            .and_then(|hold| hold.variable)
    }

    fn bind(&mut self, register: Register, variable: Variable, is_fixed: bool) {
        for hold in &mut self.registers {
            if hold.register == register {
                hold.variable = Some(variable);
                hold.is_fixed = is_fixed;
                return;
            }
        }
    }

    fn release(&mut self, variable: Variable) -> Option<Register> {
        match self
            .registers
            .iter_mut()
            .find(|hold| hold.variable == Some(variable))
        {
            Some(hold) => {
                hold.variable = None;
                hold.is_fixed = false;
                Some(hold.register)
            }
            None => None,
        }
    }

    fn free(&self, locked: &[Register]) -> Option<Register> {
        self.registers
            .iter()
            .find(|hold| hold.variable.is_none() && !locked.contains(&hold.register))
            .map(|hold| hold.register)
    }

    /// 优先驱逐临时（非固定）寄存器，顺序按寄存器编号，保证确定性。
    fn evictable(&self, locked: &[Register]) -> Option<RegisterHold> {
        self.registers
            .iter()
            .filter(|hold| hold.variable.is_some() && !locked.contains(&hold.register))
            .min_by_key(|hold| (hold.is_fixed, hold.register.as_usize()))
            .copied()
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
    variable: Option<Variable>,
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

#[derive(Debug, Clone, Copy)]
pub enum Action {
    Restore { stack: usize, register: Register },
    Spill { register: Register, stack: usize },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rebuilt_intervals_have_no_holes() {
        let mut interval = LiveInterval::new(Variable::new(0));
        interval.rebuild(&[0, 1, 2, 5, 6].into_iter().collect());
        assert_eq!(interval.ranges.len(), 2);
        assert_eq!((interval.start, interval.end), (0, 6));
        assert_eq!((interval.ranges[0].start, interval.ranges[0].end), (0, 2));
        assert_eq!((interval.ranges[1].start, interval.ranges[1].end), (5, 6));

        // 空洞 [3,4] 不与任何精确区间相交；旧的整体 [0,6] 判定会误报冲突。
        let mut hole = LiveInterval::new(Variable::new(1));
        hole.rebuild(&[3, 4].into_iter().collect());
        assert!(!interval.overlaps(&hole));

        let mut crossing = LiveInterval::new(Variable::new(2));
        crossing.rebuild(&[2, 3, 4].into_iter().collect());
        assert!(interval.overlaps(&crossing));
    }

    #[test]
    fn alloc_emits_spill_before_restore() {
        let mut ra = RegAlloc::new(&Register::general()[..4]);
        let v0 = Variable::new(0);
        let v1 = Variable::new(1);
        ra.stack_slot(v0);
        ra.stack_slot(v1);
        ra.liveness.set_register(v0, Register::R0);
        ra.liveness.set_register(v1, Register::R0);

        let (reg0, first) = ra.alloc(v0, &[]);
        assert_eq!(reg0, Register::R0);
        assert!(matches!(first[..], [Action::Restore { .. }]));

        let (reg1, second) = ra.alloc(v1, &[]);
        assert_eq!(reg1, Register::R0);
        assert_eq!(second.len(), 2);
        assert!(matches!(second[0], Action::Spill { .. }));
        assert!(matches!(second[1], Action::Restore { .. }));
    }

    #[test]
    fn fixed_register_does_not_steal_a_locked_operand() {
        let mut ra = RegAlloc::new(&Register::general()[..4]);
        let v0 = Variable::new(0);
        let v1 = Variable::new(1);
        ra.stack_slot(v0);
        ra.stack_slot(v1);
        ra.liveness.set_register(v0, Register::R0);
        ra.liveness.set_register(v1, Register::R0);

        let (reg0, _) = ra.alloc(v0, &[]);
        let (reg1, _) = ra.alloc(v1, &[reg0]);
        assert_eq!(reg0, Register::R0);
        assert_ne!(reg1, reg0);
        assert_eq!(ra.reg_set.find(v0), Some(Register::R0));
    }

    #[test]
    fn spill_return_register_saves_the_live_value() {
        let mut ra = RegAlloc::new(&Register::general()[..4]);
        let v0 = Variable::new(0);
        ra.stack_slot(v0);
        ra.liveness.set_register(v0, RETURN_REGISTER);

        let (reg, _) = ra.alloc(v0, &[]);
        assert_eq!(reg, RETURN_REGISTER);

        match ra.spill_return_register() {
            Some(Action::Spill { register, .. }) => assert_eq!(register, RETURN_REGISTER),
            _ => panic!("expected a spill of the live value in R0"),
        }
        assert!(ra.reg_set.find(v0).is_none());
        assert!(ra.spill_return_register().is_none());
    }

    #[test]
    fn alloc_def_does_not_restore() {
        let mut ra = RegAlloc::new(&Register::general()[..4]);
        let v0 = Variable::new(0);
        ra.stack_slot(v0);
        let (_, actions) = ra.alloc(v0, &[]);
        assert!(
            actions
                .iter()
                .any(|action| matches!(action, Action::Restore { .. }))
        );

        let v1 = Variable::new(1);
        ra.stack_slot(v1);
        let (_, actions) = ra.alloc_def(v1, &[]);
        assert!(
            !actions
                .iter()
                .any(|action| matches!(action, Action::Restore { .. }))
        );
    }
}
