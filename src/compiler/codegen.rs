use std::collections::HashMap;

use log::{debug, trace};
use petgraph::visit::DfsPostOrder;

use crate::{
    bytecode::{Bytecode, Opcode, Operand, Register},
    ir::instruction::{Block, ControlFlowGraph, Instruction, Value},
};

use super::regalloc::{Action, RegAlloc};

type PatchFn = Box<dyn Fn(&mut Codegen)>;

pub struct Codegen {
    reg_alloc: RegAlloc,
    codes: Vec<Bytecode>,
    block_map: HashMap<isize, isize>,
    inst_index: usize,
}

impl Codegen {
    pub fn new(registers: &[Register]) -> Self {
        Self {
            reg_alloc: RegAlloc::new(registers),
            codes: Vec::new(),
            block_map: HashMap::new(),
            inst_index: 0,
        }
    }

    pub fn generate_code(&mut self, cfg: ControlFlowGraph) -> &[Bytecode] {
        let mut cfg = cfg;

        Self::resort_blocks(&mut cfg);

        debug!("===IR===");
        let mut pos = 0;
        for block in &cfg.blocks {
            debug!("block({}):", block.id.as_usize());
            for inst in &block.instructions {
                debug!("{pos}\t{inst}");
                pos += 1;
            }
        }
        debug!("===IR===");

        self.reg_alloc.arrange(&cfg);

        let mut patchs: Vec<PatchFn> = Vec::new();

        // alloc stack frame, need rewrite with actual stack size
        // rsp = rsp + stack_size
        let pos = self.codes.len();
        patchs.push(Box::new(move |this: &mut Self| {
            this.codes[pos].operands[2] = Operand::new_immd(this.reg_alloc.stack_size() as isize)
        }));
        // placeholder
        self.codes.push(Bytecode::triple(
            Opcode::AddC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(0),
        ));

        for block in cfg.blocks.iter() {
            self.block_map
                .insert(block.id.as_usize() as isize, self.codes.len() as isize);

            for inst in &block.instructions {
                debug!("inst[{}]: {inst:?}", self.inst_index);
                debug!("register: {}", self.reg_alloc.reg_set);

                match inst.clone() {
                    // Function Call Instructions
                    Instruction::Call { func, args, result } => {
                        self.gen_call(func, &args, result);
                    }
                    Instruction::CallEx {
                        callable,
                        args,
                        result,
                    } => {
                        self.gen_call_ex(callable, &args, result);
                    }
                    Instruction::CallNative { func, args, result } => {
                        self.gen_call_native(func, &args, result);
                    }
                    Instruction::PropertyCall {
                        object,
                        property,
                        args,
                        result,
                    } => {
                        self.gen_prop_call(object, property, &args, result);
                    }

                    // Load and Move Instructions
                    Instruction::LoadArg { dst, index } => {
                        let dst = self.gen_operand(dst);
                        let stack = self.reg_alloc.load_arg(index);
                        self.codes.push(Bytecode::double(
                            Opcode::Mov,
                            dst,
                            Operand::new_stack(stack),
                        ));
                    }
                    Instruction::LoadConst { dst, const_id } => {
                        let dst = self.gen_operand(dst);

                        self.codes.push(Bytecode::double(
                            Opcode::LoadConst,
                            dst,
                            const_id.to_operand(),
                        ));
                    }
                    Instruction::LoadEnv { dst, name } => {
                        let dst = self.gen_operand(dst);
                        self.codes
                            .push(Bytecode::double(Opcode::LoadEnv, dst, name.to_operand()));
                    }
                    Instruction::Move { dst, src } => {
                        let src = self.gen_operand(src);
                        let dst = self.gen_operand(dst);
                        self.codes.push(Bytecode::double(Opcode::Mov, dst, src));
                    }

                    // Unary and Binary Operators
                    Instruction::UnaryOp { op, dst, src } => {
                        let src = self.gen_operand(src);
                        let dst = self.gen_operand(dst);

                        self.codes.push(Bytecode::double(op, dst, src));
                    }
                    Instruction::BinaryOp { op, dst, lhs, rhs } => {
                        let src1 = self.gen_operand(lhs);
                        let src2 = self.gen_operand(rhs);
                        let dst = self.gen_operand(dst);
                        self.codes.push(Bytecode::triple(op, dst, src1, src2));
                    }

                    // Range Instructions
                    Instruction::MakeRange {
                        op,
                        begin,
                        end,
                        result,
                    } => match (begin, end) {
                        (Some(begin), Some(end)) => {
                            let src1 = self.gen_operand(begin);
                            let src2 = self.gen_operand(end);
                            let dst = self.gen_operand(result);
                            self.codes.push(Bytecode::triple(op, dst, src1, src2));
                        }
                        (Some(begin), None) => {
                            let src1 = self.gen_operand(begin);
                            let dst = self.gen_operand(result);
                            self.codes
                                .push(Bytecode::double(Opcode::RangeFrom, dst, src1));
                        }
                        (None, Some(end)) => {
                            let src1 = self.gen_operand(end);
                            let dst = self.gen_operand(result);
                            match op {
                                Opcode::RangeInclusive => {
                                    self.codes.push(Bytecode::double(
                                        Opcode::RangeToInclusive,
                                        dst,
                                        src1,
                                    ));
                                }
                                Opcode::Range => {
                                    self.codes
                                        .push(Bytecode::double(Opcode::RangeTo, dst, src1));
                                }
                                _ => unreachable!("invalid op"),
                            }
                        }
                        (None, None) => {
                            let dst = self.gen_operand(result);
                            self.codes.push(Bytecode::single(Opcode::RangeFull, dst));
                        }
                    },

                    // Collection / Structural Operations
                    Instruction::MakeArray { dst } => {
                        let dst = self.gen_operand(dst);
                        self.codes.push(Bytecode::single(Opcode::MakeArray, dst));
                    }
                    Instruction::ArrayPush { array, value } => {
                        let array = self.gen_operand(array);
                        let value = self.gen_operand(value);
                        self.codes
                            .push(Bytecode::double(Opcode::ArrayPush, array, value));
                    }
                    Instruction::MakeMap { dst } => {
                        let dst = self.gen_operand(dst);
                        self.codes.push(Bytecode::single(Opcode::MakeMap, dst));
                    }
                    Instruction::IndexSet {
                        object,
                        index: idx,
                        value,
                    } => {
                        let object = self.gen_operand(object);
                        let idx = self.gen_operand(idx);
                        let value = self.gen_operand(value);
                        self.codes
                            .push(Bytecode::triple(Opcode::IndexSet, object, idx, value));
                    }
                    Instruction::IndexGet {
                        dst,
                        object,
                        index: idx,
                    } => {
                        let dst = self.gen_operand(dst);
                        let object = self.gen_operand(object);
                        let idx = self.gen_operand(idx);
                        self.codes
                            .push(Bytecode::triple(Opcode::IndexGet, dst, object, idx));
                    }
                    Instruction::MakeSlice { dst, object, range } => {
                        let dst = self.gen_operand(dst);
                        let object = self.gen_operand(object);
                        let range = self.gen_operand(range);
                        self.codes
                            .push(Bytecode::triple(Opcode::MakeSlice, dst, object, range));
                    }
                    Instruction::PropertyGet {
                        dst,
                        object,
                        property,
                    } => {
                        let dst = self.gen_operand(dst);
                        let object = self.gen_operand(object);
                        let property = self.gen_operand(property);
                        self.codes
                            .push(Bytecode::triple(Opcode::PropGet, dst, object, property));
                    }
                    Instruction::PropertySet {
                        object,
                        property,
                        value,
                    } => {
                        let object = self.gen_operand(object);
                        let property = self.gen_operand(property);
                        let value = self.gen_operand(value);
                        self.codes
                            .push(Bytecode::triple(Opcode::PropSet, object, property, value));
                    }

                    // Iteration Instructions
                    Instruction::MakeIterator {
                        src: iter,
                        dst: result,
                    } => {
                        let src = self.gen_operand(iter);
                        let dst = self.gen_operand(result);
                        self.codes
                            .push(Bytecode::double(Opcode::MakeIter, dst, src));
                    }
                    Instruction::IterateNext { iter, dst: next } => {
                        let src = self.gen_operand(iter);
                        let dst = self.gen_operand(next);
                        self.codes
                            .push(Bytecode::double(Opcode::IterNext, dst, src));
                    }

                    // Control Flow Instructions
                    Instruction::Return { value } => {
                        if let Some(v) = value {
                            let ret = self.gen_operand(v);
                            self.codes.push(Bytecode::double(
                                Opcode::Mov,
                                Operand::new_register(Register::Rv),
                                ret,
                            ));
                        }

                        self.codes.push(Bytecode::empty(Opcode::Ret));
                    }
                    Instruction::Br { dst } => {
                        let dst = self.gen_operand(dst);

                        let pos = self.codes.len();
                        patchs.push(Box::new(move |this: &mut Self| {
                            let dst = this.codes[pos].operands[0].as_immd();
                            this.codes[pos].operands[0] =
                                Operand::new_immd(this.block_map[&dst] - pos as isize);
                        }));

                        self.codes.push(Bytecode::single(Opcode::Br, dst));
                    }
                    Instruction::BrIf {
                        condition,
                        true_blk,
                        false_blk,
                    } => {
                        let condition = self.gen_operand(condition);
                        let true_blk = self.gen_operand(true_blk);
                        let false_blk = self.gen_operand(false_blk);

                        let pos = self.codes.len();
                        patchs.push(Box::new(move |this: &mut Self| {
                            let true_blk = this.codes[pos].operands[1].as_immd();
                            this.codes[pos].operands[1] =
                                Operand::new_immd(this.block_map[&true_blk] - pos as isize);
                            let false_blk = this.codes[pos].operands[2].as_immd();
                            this.codes[pos].operands[2] =
                                Operand::new_immd(this.block_map[&false_blk] - pos as isize);
                        }));

                        self.codes.push(Bytecode::triple(
                            Opcode::BrIf,
                            condition,
                            true_blk,
                            false_blk,
                        ));
                    }
                    Instruction::Halt => {
                        self.codes.push(Bytecode::empty(Opcode::Halt));
                    }

                    // Async Support
                    Instruction::Await { promise, dst } => {
                        let promise = self.gen_operand(promise);
                        let dst = self.gen_operand(dst);
                        self.codes
                            .push(Bytecode::double(Opcode::Await, dst, promise));
                    }
                }

                let (defined, used) = inst.defined_and_used_vars();
                for var in defined {
                    if matches!(var, Value::Variable(_)) {
                        if let Some(Action::Spill { stack, register }) =
                            self.reg_alloc.release(var, self.inst_index)
                        {
                            trace!("spilling({var}) {register} -> [rbp+{stack}]");
                            self.codes.push(Bytecode::double(
                                Opcode::Mov,
                                Operand::Stack(stack as isize),
                                register.into(),
                            ));
                        }
                    }
                }

                for var in used {
                    if matches!(var, Value::Variable(_)) {
                        if let Some(Action::Spill { stack, register }) =
                            self.reg_alloc.release(var, self.inst_index)
                        {
                            trace!("spilling({var}) {register} -> [rbp+{stack}]");
                            self.codes.push(Bytecode::double(
                                Opcode::Mov,
                                Operand::Stack(stack as isize),
                                register.into(),
                            ));
                        }
                    }
                }

                self.inst_index += 1;
            }
        }

        for patch in patchs {
            patch(self);
        }

        &self.codes
    }

    fn gen_call(&mut self, func: Value, args: &[Value], result: Value) {
        // 1. Backup used registers
        let in_use_registers = self.reg_alloc.in_use_registers();
        for reg in in_use_registers.iter().copied() {
            self.codes.push(Bytecode::single(Opcode::Push, reg.into()));
        }

        // 2. Push arguments onto the stack
        self.store_args(args, self.inst_index);

        // 3. Set up new stack frame
        self.codes.push(Bytecode::single(
            Opcode::PushC,
            Operand::new_register(Register::Rbp),
        ));
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rbp),
            Operand::new_register(Register::Rsp),
        ));

        // 4. Call the function
        self.codes
            .push(Bytecode::single(Opcode::Call, func.to_operand()));

        // 5. Restore stack pointer (reset to current base pointer)
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rsp),
            Operand::new_register(Register::Rbp),
        ));

        // 6. Pop the saved base pointer
        self.codes
            .push(Bytecode::single(Opcode::PopC, Register::Rbp.into()));

        // 7. Clean up arguments from the stack
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 8. Restore backed-up registers
        for reg in in_use_registers.iter().rev().copied() {
            self.codes.push(Bytecode::single(Opcode::Pop, reg.into()));
        }

        // 9. Move return value to destination register
        let result_reg = self.gen_operand(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            Operand::new_register(Register::Rv),
        ));
    }

    fn gen_call_ex(&mut self, func: Value, args: &[Value], result: Value) {
        let callable = self.gen_operand(func);

        // 1. Backup used registers
        let in_use_registers = self.reg_alloc.in_use_registers();
        for reg in in_use_registers.iter().copied() {
            self.codes.push(Bytecode::single(Opcode::Push, reg.into()));
        }

        // 2. Push arguments onto the stack
        self.store_args(args, self.inst_index);

        // 3. Set up new stack frame
        self.codes.push(Bytecode::single(
            Opcode::PushC,
            Operand::new_register(Register::Rbp),
        ));
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rbp),
            Operand::new_register(Register::Rsp),
        ));

        // 4. Call the function (CallEx)
        self.codes.push(Bytecode::single(Opcode::CallEx, callable));

        // 5. Restore stack pointer to current base pointer
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rsp),
            Operand::new_register(Register::Rbp),
        ));

        // 6. Pop saved base pointer
        self.codes
            .push(Bytecode::single(Opcode::PopC, Register::Rbp.into()));

        // 7. Clean up arguments from the stack
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 8. Restore backed-up registers
        for reg in in_use_registers.iter().rev().copied() {
            self.codes.push(Bytecode::single(Opcode::Pop, reg.into()));
        }

        // 9. Move return value to destination register
        let result_reg = self.gen_operand(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            Operand::new_register(Register::Rv),
        ));
    }
    fn gen_call_native(&mut self, func: Value, args: &[Value], result: Value) {
        let callable = self.gen_operand(func);

        // 1. Push arguments onto the stack
        self.store_args(args, self.inst_index);

        // 2. Set up new stack frame
        self.codes.push(Bytecode::single(
            Opcode::PushC,
            Operand::new_register(Register::Rbp),
        ));
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rbp),
            Operand::new_register(Register::Rsp),
        ));

        // 3. Call the native function
        self.codes.push(Bytecode::double(
            Opcode::CallNative,
            callable,
            Operand::new_immd(args.len() as isize),
        ));

        // 4. Restore stack pointer to current base pointer
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rsp),
            Operand::new_register(Register::Rbp),
        ));

        // 5. Pop saved base pointer
        self.codes
            .push(Bytecode::single(Opcode::PopC, Register::Rbp.into()));

        // 6. Clean up arguments from the stack
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 7. Move return value to destination register
        let result_reg = self.gen_operand(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            Operand::new_register(Register::Rv),
        ));
    }

    fn gen_prop_call(&mut self, object: Value, property: Value, args: &[Value], result: Value) {
        let callable = self.gen_operand(object);

        // 1. Push arguments onto the stack
        self.store_args(args, self.inst_index);

        // 2. Set up new stack frame
        self.codes.push(Bytecode::single(
            Opcode::PushC,
            Operand::new_register(Register::Rbp),
        ));
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rbp),
            Operand::new_register(Register::Rsp),
        ));

        let prop = self.gen_operand(property);
        // 3. Call method on the object
        self.codes.push(Bytecode::triple(
            Opcode::MethodCall,
            callable,
            prop,
            Operand::new_immd(args.len() as isize),
        ));

        // 4. Restore stack pointer to current base pointer
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rsp),
            Operand::new_register(Register::Rbp),
        ));

        // 5. Pop saved base pointer
        self.codes
            .push(Bytecode::single(Opcode::PopC, Register::Rbp.into()));

        // 6. Clean up arguments from the stack
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 7. Move return value to destination register
        let result_reg = self.gen_operand(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            Operand::new_register(Register::Rv),
        ));
    }

    fn store_args(&mut self, args: &[Value], index: usize) {
        for arg in args.iter().rev() {
            let op = self.gen_operand(*arg);
            self.codes.push(Bytecode::single(Opcode::Push, op));
            if let Some(action) = self.reg_alloc.release(*arg, index) {
                match action {
                    Action::Spill { stack, register } => {
                        trace!("spilling({arg}) {register} -> [rbp+{stack}]");
                        self.codes.push(Bytecode::double(
                            Opcode::Mov,
                            Operand::Stack(stack as isize),
                            register.into(),
                        ));
                    }
                    _ => unreachable!("action must be spill"),
                }
            }
        }
    }

    fn gen_operand(&mut self, value: Value) -> Operand {
        match value {
            Value::Primitive(v) => Operand::new_primitive(v),
            Value::Constant(id) => Operand::new_immd(id.as_usize() as isize),
            Value::Function(id) => Operand::new_symbol(id.as_usize() as u32),
            Value::Block(id) => Operand::new_immd(id.as_usize() as isize),
            Value::Variable(_) => {
                let (register, unspill) = self.reg_alloc.alloc(value, self.inst_index);

                if let Some(Action::Restore { stack, register }) = unspill {
                    trace!("unspilling({value}) [rbp+{stack}] -> {register}");
                    self.codes.push(Bytecode::double(
                        Opcode::Mov,
                        register.into(),
                        Operand::Stack(stack as isize),
                    ));
                }

                Operand::new_register(register)
            }
        }
    }

    fn resort_blocks(control_flow_graph: &mut ControlFlowGraph) {
        // sort blocks by post order
        let mut sorted = sort_graph_blocks(control_flow_graph);

        let mut iter = sorted.iter_mut().peekable();

        while let Some(block) = iter.next() {
            // remove instructions after return
            if let Some(i) = block
                .instructions
                .iter()
                .rposition(|item| matches!(item, Instruction::Return { .. }))
            {
                if i + 1 < block.instructions.len() {
                    block.instructions.drain(i + 1..);
                }
            }

            let next_block_id = iter.peek().map(|b| b.id);
            if let Some(Instruction::Br { dst }) = block.instructions.last() {
                if next_block_id == dst.as_block() {
                    block.instructions.pop();
                }
            }
        }

        control_flow_graph.blocks = sorted;
    }
}

fn sort_graph_blocks(control_flow_graph: &ControlFlowGraph) -> Vec<Block> {
    let graph = &control_flow_graph.graph;
    let entry = control_flow_graph.entry().expect("no entry block");
    let start = control_flow_graph.block_node_map[&entry];

    let mut dfs = DfsPostOrder::new(graph, start);

    let mut sorted = Vec::new();

    while let Some(node) = dfs.next(graph) {
        let block_id = graph[node];
        sorted.push(block_id);
    }

    sorted.reverse();

    sorted
        .into_iter()
        .map(|block_id| {
            let block = control_flow_graph
                .get_block(block_id)
                .expect("no such block");
            block.clone()
        })
        .collect()
}
