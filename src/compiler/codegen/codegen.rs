use std::collections::HashMap;

use log::{debug, trace};
use petgraph::visit::DfsPostOrder;

use crate::{
    bytecode::{Bytecode, Opcode, Operand, Register},
    compiler::codegen::regalloc::Action,
    ir::instruction::{Block, ControlFlowGraph, Instruction, Value},
};

use super::regalloc::RegAlloc;

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
                // preload
                // if !inst.is_call() {
                //     for action in self.reg_alloc.pre_allocate(index) {
                //         match action {
                //             Action::UnSpill { stack, register } => {
                //                 self.codes.push(Bytecode::double(
                //                     Opcode::Mov,
                //                     register.into(),
                //                     Operand::Stack(stack as isize),
                //                 ));
                //             }
                //             _ => unreachable!(),
                //         }
                //     }
                // }

                debug!("inst[{}]: {inst:?}", self.inst_index);
                debug!("register: {}", self.reg_alloc.reg_set);

                match inst.clone() {
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
                    Instruction::PropertyCall {
                        object,
                        property,
                        args,
                        result,
                    } => {
                        self.gen_prop_call(object, property, &args, result);
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
        // 0. result register
        // let result_reg = self.gen_operand( result);

        // 1. backup registers
        let in_use_registers = self.reg_alloc.in_use_registers();
        for reg in in_use_registers.iter().copied() {
            self.codes.push(Bytecode::single(Opcode::Push, reg.into()));
        }

        // 2. push arguments
        self.store_args(args, self.inst_index);

        // 3. call function
        self.codes.push(Bytecode::single(
            Opcode::PushC,
            Operand::new_register(Register::Rbp),
        ));
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rbp),
            Operand::new_register(Register::Rsp),
        ));

        // call
        self.codes
            .push(Bytecode::single(Opcode::Call, func.to_operand()));

        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rsp),
            Operand::new_register(Register::Rbp),
        ));

        self.codes
            .push(Bytecode::single(Opcode::PopC, Register::Rbp.into()));

        // 4. pop arguments
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 5. restore registers
        for reg in in_use_registers.iter().rev().copied() {
            self.codes.push(Bytecode::single(Opcode::Pop, reg.into()));
        }

        // 3. move result
        let result_reg = self.gen_operand(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            Operand::new_register(Register::Rv),
        ));
    }

    fn gen_call_ex(&mut self, func: Value, args: &[Value], result: Value) {
        let callable = self.gen_operand(func);

        // 1. backup registers
        let in_use_registers = self.reg_alloc.in_use_registers();
        for reg in in_use_registers.iter().copied() {
            self.codes.push(Bytecode::single(Opcode::Push, reg.into()));
        }

        // 2. push arguments
        self.store_args(args, self.inst_index);

        // 3. call function
        self.codes.push(Bytecode::single(
            Opcode::PushC,
            Operand::new_register(Register::Rbp),
        ));
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rbp),
            Operand::new_register(Register::Rsp),
        ));

        // call_ex
        self.codes.push(Bytecode::single(Opcode::CallEx, callable));

        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rsp),
            Operand::new_register(Register::Rbp),
        ));

        self.codes
            .push(Bytecode::single(Opcode::PopC, Register::Rbp.into()));

        // 4. pop arguments
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));
        // 5. restore registers
        for reg in in_use_registers.iter().rev().copied() {
            self.codes.push(Bytecode::single(Opcode::Pop, reg.into()));
        }

        // 3. move result
        let result_reg = self.gen_operand(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            Operand::new_register(Register::Rv),
        ));
    }

    fn gen_call_native(&mut self, func: Value, args: &[Value], result: Value) {
        let callable = self.gen_operand(func);

        // 1. push arguments
        self.store_args(args, self.inst_index);

        // 3. call function
        self.codes.push(Bytecode::single(
            Opcode::PushC,
            Operand::new_register(Register::Rbp),
        ));
        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rbp),
            Operand::new_register(Register::Rsp),
        ));

        // call_native
        self.codes.push(Bytecode::double(
            Opcode::CallNative,
            callable,
            Operand::new_immd(args.len() as isize),
        ));

        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rsp),
            Operand::new_register(Register::Rbp),
        ));

        self.codes
            .push(Bytecode::single(Opcode::PopC, Register::Rbp.into()));

        // 4. pop arguments
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 6. move result
        let result_reg = self.gen_operand(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            Operand::new_register(Register::Rv),
        ));
    }

    fn gen_prop_call(&mut self, object: Value, property: Value, args: &[Value], result: Value) {
        let callable = self.gen_operand(object);

        // 1. push arguments
        self.store_args(args, self.inst_index);

        // 3. call function
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
        // method_call
        self.codes.push(Bytecode::triple(
            Opcode::MethodCall,
            callable,
            prop,
            Operand::new_immd(args.len() as isize),
        ));

        self.codes.push(Bytecode::double(
            Opcode::MovC,
            Operand::new_register(Register::Rsp),
            Operand::new_register(Register::Rbp),
        ));

        self.codes
            .push(Bytecode::single(Opcode::PopC, Register::Rbp.into()));

        // 4. pop arguments
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 6. move result
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
            // // remove instructions after return
            // if let Some(i) = block
            //     .instructions
            //     .iter()
            //     .rposition(|item| matches!(item, Instruction::Return { .. }))
            // {
            //     if i + 1 < block.instructions.len() {
            //         block.instructions.drain(i + 1..);
            //     }
            // }

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

#[cfg(test)]
mod tests {

    use crate::{
        bytecode::{Opcode, Primitive},
        ir::instruction::VariableId,
    };

    use super::*;

    #[test]
    fn test_register_allocator() {
        // move %0, 3.141592653589793
        // move %1, 2.718281828459045
        // %2 = mul %0, %1
        // return %2
        let instructions = vec![
            Instruction::Move {
                dst: Value::Variable(VariableId::new(0)),
                src: Value::Primitive(Primitive::from(3.141592653589793)),
            },
            Instruction::Move {
                dst: Value::Variable(VariableId::new(1)),
                src: Value::Primitive(Primitive::from(718281828459045)),
            },
            Instruction::BinaryOp {
                op: Opcode::Mulx,
                dst: Value::Variable(VariableId::new(2)),
                lhs: Value::Variable(VariableId::new(0)),
                rhs: Value::Variable(VariableId::new(1)),
            },
            Instruction::Return {
                value: Some(Value::Variable(VariableId::new(2))),
            },
        ];

        let mut cfg = ControlFlowGraph::new();
        let entry = cfg.create_block_with_instructions("test", instructions);
        cfg.set_entry(entry);

        let mut codegen = Codegen::new(&Register::small_general());

        let bytecodes = codegen.generate_code(cfg);

        for bytecode in bytecodes {
            println!("{bytecode};");
        }
    }

    #[test]
    fn test_register_allocator2() {
        // %0 = add i1, i1;
        // %1 = mul i2, i2;
        // %2 = add %0, %1;
        // return %2;
        let instructions = vec![
            Instruction::BinaryOp {
                op: Opcode::Addx,
                dst: Value::Variable(VariableId::new(0)),
                lhs: Value::Primitive(Primitive::from(1)),
                rhs: Value::Primitive(Primitive::from(1)),
            },
            Instruction::BinaryOp {
                op: Opcode::Mulx,
                dst: Value::Variable(VariableId::new(1)),
                lhs: Value::Primitive(Primitive::from(2)),
                rhs: Value::Primitive(Primitive::from(2)),
            },
            Instruction::BinaryOp {
                op: Opcode::Addx,
                dst: Value::Variable(VariableId::new(2)),
                lhs: Value::Variable(VariableId::new(0)),
                rhs: Value::Variable(VariableId::new(1)),
            },
            Instruction::Return {
                value: Some(Value::Variable(VariableId::new(2))),
            },
        ];

        let mut cfg = ControlFlowGraph::new();
        let entry = cfg.create_block_with_instructions("test", instructions);
        cfg.set_entry(entry);

        let mut codegen = Codegen::new(&Register::small_general());

        let bytecodes = codegen.generate_code(cfg);

        for bytecode in bytecodes {
            println!("{bytecode};");
        }
    }

    #[test]
    fn test_codegen() {
        /*
        block(1):
        0       %0 = move 0
        block(2):
        1       %1 = make_range 0..=2
        2       %2 = make_iterator %1
        block(3):
        3       %3 = iterator_has_next %2
        4       br_if %3, @block(4), @block(5)
        block(5):
        5       return %0
        block(4):
        6       %4 = iterate_next %2
        7       %5 = move %4
        8       %6 = add %0, %5
        9       %0 = move %6
        10      br @block(3)
        */

        let mut cfg = ControlFlowGraph::new();

        let block1 = cfg.create_block(None);
        let block2 = cfg.create_block(None);
        let block3 = cfg.create_block(None);
        let block4 = cfg.create_block(None);
        let block5 = cfg.create_block(None);

        cfg.set_entry(block1);

        // block(1)
        cfg.switch_to_block(block1);
        cfg.emit(Instruction::Move {
            dst: Value::Variable(VariableId::new(0)),
            src: Value::Primitive(Primitive::from(0)),
        });

        // block(2)
        cfg.switch_to_block(block2);
        cfg.emit(Instruction::MakeRange {
            op: Opcode::RangeInclusive,
            result: Value::Variable(VariableId::new(1)),
            begin: Some(Value::Primitive(Primitive::from(0))),
            end: Some(Value::Primitive(Primitive::from(2))),
        });
        cfg.emit(Instruction::MakeIterator {
            src: Value::Variable(VariableId::new(1)),
            dst: Value::Variable(VariableId::new(2)),
        });

        // block(3)
        cfg.switch_to_block(block3);
        cfg.emit(Instruction::BrIf {
            condition: Value::Variable(VariableId::new(3)),
            true_blk: Value::Block(block4),
            false_blk: Value::Block(block5),
        });

        // block(4)
        cfg.switch_to_block(block4);
        cfg.emit(Instruction::IterateNext {
            iter: Value::Variable(VariableId::new(2)),
            dst: Value::Variable(VariableId::new(4)),
        });
        cfg.emit(Instruction::Move {
            dst: Value::Variable(VariableId::new(5)),
            src: Value::Variable(VariableId::new(4)),
        });
        cfg.emit(Instruction::BinaryOp {
            op: Opcode::Addx,
            dst: Value::Variable(VariableId::new(6)),
            lhs: Value::Variable(VariableId::new(0)),
            rhs: Value::Variable(VariableId::new(5)),
        });
        cfg.emit(Instruction::Move {
            dst: Value::Variable(VariableId::new(0)),
            src: Value::Variable(VariableId::new(6)),
        });
        cfg.emit(Instruction::Br {
            dst: Value::Block(block3),
        });

        // block(5)
        cfg.switch_to_block(block5);
        cfg.emit(Instruction::Return {
            value: Some(Value::Variable(VariableId::new(0))),
        });

        let mut codegen = Codegen::new(&Register::small_general());
        let bytecodes = codegen.generate_code(cfg);

        for bytecode in bytecodes {
            println!("{bytecode};");
        }
    }
}
