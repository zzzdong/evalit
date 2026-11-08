use std::collections::{BTreeMap, HashMap};

use log::{debug, trace};

use super::ir::{ControlFlowGraph, Instruction, Value, instruction::Variable};
use crate::bytecode::{Bytecode, Opcode, Operand, Primitive, RETURN_REGISTER, Register};

use super::regalloc::{Action, RegAlloc};

type PatchFn = Box<dyn Fn(&mut Codegen)>;

pub struct Codegen {
    reg_alloc: RegAlloc,
    codes: Vec<Bytecode>,
    block_map: HashMap<isize, isize>,
    inst_index: usize,
    insts: BTreeMap<usize, Instruction>,
    /// 本条 IR 指令已经生成的操作数寄存器，分配器驱逐时必须跳过。
    locked: Vec<Register>,
}

impl Codegen {
    pub fn new(registers: &[Register]) -> Self {
        Self {
            reg_alloc: RegAlloc::new(registers),
            codes: Vec::new(),
            block_map: HashMap::new(),
            inst_index: 0,
            insts: BTreeMap::new(),
            locked: Vec::new(),
        }
    }

    pub fn generate_code(&mut self, cfg: ControlFlowGraph) -> &[Bytecode] {
        // debug ir
        let block_layout = cfg.loop_root_reverse_postorder_layout2();

        self.reg_alloc.arrange(&cfg, &block_layout);

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

        for block in block_layout.iter(&cfg) {
            self.block_map
                .insert(block.id().as_usize() as isize, self.codes.len() as isize);

            // 块入口只经内存交接：编译期 reg_set 在块入口不可信。
            self.reg_alloc.begin_block();

            let block_len = block.instructions().len();
            for (inst_pos, inst) in block.instructions().iter().enumerate() {
                self.begin_inst();
                // 块出口把绑定值全部写回固定栈槽，终止符里的跳转实参
                // 以及下一块的 Restore 才能拿到最新值。
                if inst_pos + 1 == block_len {
                    let actions = self.reg_alloc.spill_all_bound();
                    self.emit_actions(&actions);
                }

                debug!("inst[{}]: {inst:?}", self.inst_index);
                debug!("register: {}", self.reg_alloc.reg_set);

                self.insts.insert(self.codes.len(), inst.clone());

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
                    Instruction::MakeStruct { dst } => {
                        let dst = self.gen_operand(dst);
                        self.codes.push(Bytecode::single(Opcode::MakeStruct, dst));
                    }
                    Instruction::MakeStructField {
                        object,
                        field,
                        value,
                    } => {
                        let object = self.gen_operand(object);
                        let field = self.gen_operand(field);
                        let value = self.gen_operand(value);
                        self.codes.push(Bytecode::triple(
                            Opcode::MakeStructField,
                            object,
                            field,
                            value,
                        ));
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
                    Instruction::IterateNext {
                        iter,
                        item,
                        has_next,
                    } => {
                        let src = self.gen_operand(iter);
                        let dst = self.gen_operand(item);
                        let has_next = self.gen_operand(has_next);
                        self.codes
                            .push(Bytecode::triple(Opcode::IterNext, dst, has_next, src));
                    }

                    // Control Flow Instructions
                    Instruction::Return { value } => {
                        // 调用约定：返回值写 R0；无值返回时显式置 null，
                        // 避免调用方读到上一次调用的残留。
                        let ret = match value {
                            Some(v) => self.gen_operand(v),
                            None => Operand::new_primitive(Primitive::Null),
                        };
                        self.codes
                            .push(Bytecode::double(Opcode::Mov, RETURN_REGISTER.into(), ret));
                        self.codes.push(Bytecode::empty(Opcode::Ret));
                    }
                    Instruction::Jump { dst, args } => {
                        let params = cfg
                            .get_block(dst.to_block())
                            .map(|b| b.params().to_vec())
                            .unwrap_or_default();
                        self.store_jump_args(&params, &args);

                        let dst = self.gen_operand(dst);

                        let pos = self.codes.len();
                        patchs.push(Box::new(move |this: &mut Self| {
                            let dst = this.codes[pos].operands[0].as_immd();
                            this.codes[pos].operands[0] =
                                Operand::new_immd(this.block_map[&dst] - pos as isize);
                        }));

                        self.codes.push(Bytecode::single(Opcode::Jump, dst));
                    }
                    Instruction::BrIf {
                        condition,
                        true_blk,
                        false_blk,
                        true_args,
                        false_args,
                    } => {
                        let true_params = cfg
                            .get_block(true_blk.to_block())
                            .map(|b| b.params().to_vec())
                            .unwrap_or_default();
                        self.store_jump_args(&true_params, &true_args);

                        let false_params = cfg
                            .get_block(false_blk.to_block())
                            .map(|b| b.params().to_vec())
                            .unwrap_or_default();
                        self.store_jump_args(&false_params, &false_args);

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

                self.inst_index += 1;
            }
        }

        for patch in patchs {
            patch(self);
        }

        &self.codes
    }

    fn gen_call(&mut self, func: Value, args: &[Value], result: Value) {
        // 先解析全部实参：解析时可能新占用寄存器，backup 必须在解析之后取，
        // 否则这些寄存器的值会被被调函数覆盖。
        let arg_ops = self.load_args(args);

        // 1. Backup used registers
        let in_use_registers = self.reg_alloc.in_use_registers();
        for reg in in_use_registers.iter().copied() {
            self.codes.push(Bytecode::single(Opcode::Push, reg.into()));
        }

        // 2. Push arguments onto the stack
        self.push_args(&arg_ops);

        self.spill_return_register();

        // 3. Call：VM 的 open_frame 建帧（rbp = rsp）并保存返回地址，
        //    close_frame 回退 rsp/rbp，不再需要 PushC / PopC / MovC。
        self.codes
            .push(Bytecode::single(Opcode::Call, func.to_operand()));

        // 4. Clean up arguments from the stack
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 5. Restore backed-up registers
        for reg in in_use_registers.iter().rev().copied() {
            self.codes.push(Bytecode::single(Opcode::Pop, reg.into()));
        }

        // 6. Move return value to destination register
        // result 是定义位置：用 alloc_def 避免 Restore 抢先覆写 R0。
        let result_reg = self.gen_operand_def(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            RETURN_REGISTER.into(),
        ));
    }

    fn gen_call_ex(&mut self, func: Value, args: &[Value], result: Value) {
        let callable = self.gen_operand(func);
        let arg_ops = self.load_args(args);

        // 1. Backup used registers
        let in_use_registers = self.reg_alloc.in_use_registers();
        for reg in in_use_registers.iter().copied() {
            self.codes.push(Bytecode::single(Opcode::Push, reg.into()));
        }

        // 2. Push arguments onto the stack
        self.push_args(&arg_ops);

        self.spill_return_register();

        // 3. Call the function (CallEx)，建帧/退帧由 VM 完成
        self.codes.push(Bytecode::single(Opcode::CallEx, callable));

        // 4. Clean up arguments from the stack
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 5. Restore backed-up registers
        for reg in in_use_registers.iter().rev().copied() {
            self.codes.push(Bytecode::single(Opcode::Pop, reg.into()));
        }

        // 6. Move return value to destination register
        // result 是定义位置：用 alloc_def 避免 Restore 抢先覆写 R0。
        let result_reg = self.gen_operand_def(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            RETURN_REGISTER.into(),
        ));
    }

    fn gen_call_native(&mut self, func: Value, args: &[Value], result: Value) {
        let callable = self.gen_operand(func);

        // 1. Push arguments onto the stack
        self.store_args(args);

        self.spill_return_register();

        // 2. Call the native function：实参按 rsp 相对读取，无需改 rbp
        self.codes.push(Bytecode::double(
            Opcode::CallNative,
            callable,
            Operand::new_immd(args.len() as isize),
        ));

        // 3. Clean up arguments from the stack
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 4. Move return value to destination register
        // result 是定义位置：用 alloc_def 避免 Restore 抢先覆写 R0。
        let result_reg = self.gen_operand_def(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            RETURN_REGISTER.into(),
        ));
    }

    fn gen_prop_call(&mut self, object: Value, property: Value, args: &[Value], result: Value) {
        let callable = self.gen_operand(object);

        // 1. Push arguments onto the stack
        self.store_args(args);

        let prop = self.gen_operand(property);
        self.spill_return_register();
        // 2. Call method on the object：实参按 rsp 相对读取
        self.codes.push(Bytecode::triple(
            Opcode::CallMethod,
            callable,
            prop,
            Operand::new_immd(args.len() as isize),
        ));

        // 3. Clean up arguments from the stack
        self.codes.push(Bytecode::triple(
            Opcode::SubC,
            Operand::Register(Register::Rsp),
            Operand::Register(Register::Rsp),
            Operand::new_immd(args.len() as isize),
        ));

        // 4. Move return value to destination register
        // result 是定义位置：用 alloc_def 避免 Restore 抢先覆写 R0。
        let result_reg = self.gen_operand_def(result);
        self.codes.push(Bytecode::double(
            Opcode::Mov,
            result_reg,
            RETURN_REGISTER.into(),
        ));
    }

    fn store_args(&mut self, args: &[Value]) {
        let ops = self.load_args(args);
        self.push_args(&ops);
    }

    /// 先解析全部实参操作数再统一 Push：解析期间的寄存器分配由 `locked`
    /// 保护，调用备份也能覆盖到所有被占用的寄存器。
    fn load_args(&mut self, args: &[Value]) -> Vec<Operand> {
        args.iter()
            .rev()
            .map(|arg| self.gen_operand(*arg))
            .collect()
    }

    fn push_args(&mut self, ops: &[Operand]) {
        for &op in ops {
            self.codes.push(Bytecode::single(Opcode::Push, op));
        }
    }

    /// 每条边把实参写进目标块参数的固定栈槽，并作废参数在寄存器里的旧副本。
    /// phi 交接只经内存：一条边写寄存器、另一条边写栈槽会让块入口读到旧值。
    fn store_jump_args(&mut self, params: &[Variable], args: &[Value]) {
        for (param, arg) in params.iter().zip(args.iter()) {
            let arg_op = self.gen_operand(*arg);
            let slot = self.reg_alloc.stack_slot(*param);
            self.codes.push(Bytecode::double(
                Opcode::Mov,
                Operand::Stack(slot as isize),
                arg_op,
            ));
            self.reg_alloc.invalidate(*param);
        }
    }

    /// 发射分配器给出的**全部**动作，顺序不能变：先 Spill 后 Restore。
    fn emit_actions(&mut self, actions: &[Action]) {
        for action in actions {
            match *action {
                Action::Restore { stack, register } => {
                    trace!("restoring [rbp+{stack}] -> {register}");
                    self.codes.push(Bytecode::double(
                        Opcode::Mov,
                        register.into(),
                        Operand::Stack(stack as isize),
                    ));
                }
                Action::Spill { register, stack } => {
                    trace!("spilling {register} -> [rbp+{stack}]");
                    self.codes.push(Bytecode::double(
                        Opcode::Mov,
                        Operand::Stack(stack as isize),
                        register.into(),
                    ));
                }
            }
        }
    }

    fn begin_inst(&mut self) {
        self.locked.clear();
    }

    /// 调用前：R0 是 ABI 返回值寄存器且 caller-clobbered，先把它上面可能存在的
    /// 活跃值写回固定栈槽并解绑，调用方之后从槽 Restore。
    fn spill_return_register(&mut self) {
        if let Some(action) = self.reg_alloc.spill_return_register() {
            self.emit_actions(&[action]);
        }
    }

    /// 定义位置：分配目标寄存器但不发 Restore（值马上被返回值覆盖）。
    fn gen_operand_def(&mut self, value: Value) -> Operand {
        match value {
            Value::Variable(var) => {
                let (register, actions) = self.reg_alloc.alloc_def(var, &self.locked);
                trace!("defining {value} -> {register}, actions = {actions:?}");
                self.emit_actions(&actions);
                self.locked.push(register);
                Operand::new_register(register)
            }
            _ => self.gen_operand(value),
        }
    }

    fn gen_operand(&mut self, value: Value) -> Operand {
        match value {
            Value::Primitive(v) => Operand::new_primitive(v),
            Value::Constant(id) => Operand::new_immd(id.as_usize() as isize),
            Value::Function(id) => Operand::new_symbol(id.as_usize() as u32),
            Value::Block(id) => Operand::new_immd(id.as_usize() as isize),
            Value::Variable(var) => {
                let (register, actions) = self.reg_alloc.alloc(var, &self.locked);
                trace!("allocating {value} -> {register}, actions = {actions:?}");
                self.emit_actions(&actions);
                self.locked.push(register);
                Operand::new_register(register)
            }
        }
    }

    pub fn debug_insts(&self) -> &BTreeMap<usize, Instruction> {
        &self.insts
    }
}
