use std::{
    collections::HashMap,
    fmt::{self},
    sync::Arc,
};

#[cfg(feature = "async")]
use super::Promise;
use super::{
    Enumerator, EnvVariable, Environment, NativeFunction, Object, Range, RuntimeError,
    UserFunction, Value, object::StructObject, value::ValueRef,
};
use crate::bytecode::{
    Bytecode, Constant, FunctionId, Module, Opcode, Operand, RETURN_REGISTER, Register,
};
use log::debug;

const STACK_MAX: usize = 0x0FFF;

#[derive(Debug)]
pub struct VM {
    state: State,
    program: Arc<Module>,
    env: Environment,
}

impl VM {
    pub fn new(program: Arc<Module>, env: Environment) -> Self {
        Self {
            state: State::new(),
            program,
            env,
        }
    }

    #[cfg(feature = "async")]
    pub async fn run(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        debug!("{}", self.program);

        while let Some(inst) = self.program.instructions.get(self.state.pc).cloned() {
            // debug!("{inst}, {}", self.state);

            let Bytecode { opcode, operands } = inst;

            match opcode {
                Opcode::Halt => {
                    // 无显式 return 的脚本自然结束，返回 null；显式 return
                    // 走顶层 Ret 分支，从 ABI 返回值寄存器取结果。
                    return Ok(Some(ValueRef::null()));
                }

                Opcode::Ret => {
                    // close_frame 一次完成退帧：回退 rsp/rbp 并把 pc 指向调用方。
                    // 顶层（没有帧）返回即结束执行。
                    if !self.state.close_frame() {
                        let ret = self.get_value(Operand::Register(RETURN_REGISTER))?;
                        return Ok(Some(ret));
                    }
                }

                Opcode::Await => {
                    let promise = self.get_value(operands[1])?;
                    let mut promise = promise.take();
                    let promise = promise
                        .downcast_mut::<Promise>()
                        .ok_or(RuntimeError::internal("only can await promise"))?;
                    let ret = promise.await;
                    self.set_value(operands[0], ret)?;
                    self.state.jump_offset(1);
                }

                _ => {
                    self.run_instruction(&inst)?;
                }
            }
        }

        Ok(None)
    }

    #[cfg(not(feature = "async"))]
    pub fn run(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        debug!("{}", self.program);

        while let Some(inst) = self.program.instructions.get(self.state.pc).cloned() {
            debug!("{}", self.state);
            debug!("{inst:?}");

            let Bytecode { opcode, operands } = inst;

            match opcode {
                Opcode::Halt => {
                    // 无显式 return 的脚本自然结束，返回 null；显式 return
                    // 走顶层 Ret 分支，从 ABI 返回值寄存器取结果。
                    return Ok(Some(ValueRef::null()));
                }

                Opcode::Ret => {
                    // close_frame 一次完成退帧：回退 rsp/rbp 并把 pc 指向调用方。
                    // 顶层（没有帧）返回即结束执行。
                    if !self.state.close_frame() {
                        let ret = self.get_value(Operand::Register(RETURN_REGISTER))?;
                        return Ok(Some(ret));
                    }
                }

                _ => {
                    self.run_instruction(&inst)?;
                }
            }
        }

        Ok(None)
    }

    fn run_instruction(&mut self, inst: &Bytecode) -> Result<(), RuntimeError> {
        let Bytecode { opcode, operands } = inst;

        match opcode {
            // Control Flow Instructions
            Opcode::Call => {
                let func = operands[0].as_immd();
                match self.program.symtab.get(&FunctionId::new(func as u32)) {
                    Some(location) => {
                        self.state.open_frame(*location)?;
                        return Ok(());
                    }
                    None => {
                        return Err(RuntimeError::SymbolNotFound {
                            name: format!("{func}"),
                        });
                    }
                }
            }
            Opcode::CallEx => match operands[0] {
                Operand::Symbol(sym) => match self.program.symtab.get(&FunctionId::new(sym)) {
                    Some(location) => {
                        self.state.open_frame(*location)?;
                        return Ok(());
                    }
                    None => {
                        return Err(RuntimeError::SymbolNotFound {
                            name: format!("{sym}"),
                        });
                    }
                },
                Operand::Register(_) | Operand::Stack(_) => {
                    let value = self.get_value(operands[0])?;
                    match value.value().downcast_ref::<UserFunction>() {
                        Some(func) => match self.program.symtab.get(&func.id()) {
                            Some(location) => {
                                self.state.open_frame(*location)?;
                                return Ok(());
                            }
                            None => {
                                return Err(RuntimeError::SymbolNotFound {
                                    name: format!("{:?}", func.id()),
                                });
                            }
                        },
                        None => {
                            return Err(RuntimeError::invalid_operand(operands[0]));
                        }
                    }
                }

                _ => return Err(RuntimeError::invalid_operand(operands[0])),
            },

            Opcode::Jump => {
                let offset = operands[0].as_immd();
                self.state.jump_offset(offset);
                return Ok(());
            }

            Opcode::BrIf => {
                let cond = self.get_value(operands[0])?;
                match cond.value().downcast_ref::<bool>() {
                    Some(b) => {
                        let offset = if *b {
                            operands[1].as_immd()
                        } else {
                            operands[2].as_immd()
                        };
                        self.state.jump_offset(offset);
                    }
                    None => return Err(RuntimeError::invalid_type::<bool>(&cond)),
                }
                return Ok(());
            }

            // Stack and Register Manipulation
            Opcode::Mov => {
                let value = self.get_value(operands[1])?;
                match operands[0] {
                    Operand::Register(reg) => {
                        self.state.set_register(reg, value)?;
                    }
                    Operand::Stack(offset) => {
                        self.state.set_value_to_stack(offset, value)?;
                    }
                    op => return Err(RuntimeError::invalid_operand(op)),
                }
            }
            Opcode::Push => {
                let value = self.get_value(operands[0])?;
                self.state.push(value)?;
            }
            Opcode::Pop => {
                let value = self.state.pop()?;
                self.set_value(operands[0], value)?;
            }

            // 单栈下 rsp 只由 AddC/SubC 与 Push/Pop 推进，rbp 只在开帧写。
            Opcode::AddC => match (operands[0], operands[1]) {
                (Operand::Register(Register::Rsp), Operand::Register(Register::Rsp)) => {
                    let next = self.state.rsp() as isize + operands[2].as_immd();
                    if next < 0 || next as usize > STACK_MAX {
                        return Err(RuntimeError::StackOverflow);
                    }
                    *self.state.rsp_mut() = next as usize;
                }
                _ => {
                    return Err(RuntimeError::internal(format!(
                        "unsupported instruction: {inst:?}"
                    )));
                }
            },
            Opcode::SubC => match (operands[0], operands[1]) {
                (Operand::Register(Register::Rsp), Operand::Register(Register::Rsp)) => {
                    let next = self.state.rsp() as isize - operands[2].as_immd();
                    if next < 0 || next as usize > STACK_MAX {
                        return Err(RuntimeError::StackOverflow);
                    }
                    *self.state.rsp_mut() = next as usize;
                }
                _ => {
                    return Err(RuntimeError::internal(format!(
                        "unsupported instruction: {inst:?}"
                    )));
                }
            },

            // Arithmetic Operations
            Opcode::Addx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.value().as_object().add(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Subx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.value().as_object().sub(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Mulx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.value().as_object().mul(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Divx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.value().as_object().div(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Remx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.value().as_object().rem(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }

            // Logical Operations
            Opcode::Not | Opcode::Neg => {
                let value = self.get_value(operands[1])?;
                let value = value.value().as_object().negate()?;
                self.set_value(operands[0], ValueRef::from(value))?;
            }
            Opcode::And => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.value().as_object().logic_and(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Or => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.value().as_object().logic_or(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }

            // Comparison Operations
            Opcode::Greater => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let ordering = lhs.value().as_object().compare(&rhs.value())?;
                self.set_value(
                    operands[0],
                    ValueRef::new(ordering == std::cmp::Ordering::Greater),
                )?;
            }
            Opcode::GreaterEqual => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let ordering = lhs.value().as_object().compare(&rhs.value())?;
                self.set_value(
                    operands[0],
                    ValueRef::new(
                        ordering == std::cmp::Ordering::Greater
                            || ordering == std::cmp::Ordering::Equal,
                    ),
                )?;
            }

            Opcode::Less => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let ordering = lhs.value().as_object().compare(&rhs.value())?;
                self.set_value(
                    operands[0],
                    ValueRef::new(ordering == std::cmp::Ordering::Less),
                )?;
            }

            Opcode::LessEqual => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let ordering = lhs.value().as_object().compare(&rhs.value())?;
                self.set_value(
                    operands[0],
                    ValueRef::new(
                        ordering == std::cmp::Ordering::Less
                            || ordering == std::cmp::Ordering::Equal,
                    ),
                )?;
            }

            Opcode::Equal => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let eq = lhs.value().as_object().equal(&rhs.value())?;
                self.set_value(operands[0], eq)?;
            }

            Opcode::NotEqual => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let eq = lhs.value().as_object().equal(&rhs.value())?;
                let not_eq = !*eq.downcast_ref::<bool>().unwrap();
                self.set_value(operands[0], Value::new(not_eq))?;
            }

            // Load Instructions
            Opcode::LoadConst => {
                let const_index = operands[1].as_immd();
                let value = ValueRef::from_constant(&self.program.constants[const_index as usize]);
                self.set_value(operands[0], value)?;
            }

            Opcode::LoadEnv => {
                let name = operands[1].as_immd();
                let name = &self.program.constants[name as usize];
                match name {
                    Constant::String(name) => match self.env.get(name.as_str()) {
                        Some(EnvVariable::Value(value)) => {
                            self.set_value(operands[0], value.clone())?;
                        }
                        Some(EnvVariable::Function(function)) => {
                            self.set_value(operands[0], function.clone())?;
                        }
                        None => {
                            return Err(RuntimeError::internal("undefined variable"));
                        }
                    },
                };
            }

            // Collection / Structural Operations
            Opcode::MakeArray => {
                let array: Vec<ValueRef> = Vec::new();
                self.set_value(operands[0], ValueRef::new(array))?;
            }

            Opcode::ArrayPush => {
                let array = self.get_value(operands[0])?;
                let value = self.get_value(operands[1])?;
                let array_cloned = array.clone();
                let mut array = array.value_mut();
                let array = array
                    .downcast_mut::<Vec<ValueRef>>()
                    .ok_or(RuntimeError::invalid_type::<Vec<ValueRef>>(array_cloned))?;
                array.push(value);
            }

            Opcode::MakeMap => {
                let map: HashMap<String, ValueRef> = HashMap::new();
                self.set_value(operands[0], ValueRef::new(map))?;
            }

            Opcode::MakeSlice => {
                let object = self.get_value(operands[1])?;
                let range = self.get_value(operands[2])?;

                let slice = object.value().as_object().make_slice(range)?;
                self.set_value(operands[0], slice)?;
            }

            Opcode::MakeStruct => {
                let struct_object = StructObject::new();
                self.set_value(operands[0], ValueRef::new(struct_object))?;
            }

            Opcode::MakeStructField => {
                let struct_object = self.get_value(operands[0])?;
                let field_value = self.get_value(operands[2])?;

                let st_cloned = struct_object.clone();
                let mut st_obj = struct_object.value_mut();
                let struct_object = st_obj
                    .downcast_mut::<StructObject>()
                    .ok_or(RuntimeError::invalid_type::<Vec<ValueRef>>(st_cloned))?;
                let field_name = self.load_string(operands[1])?;
                struct_object.make_field(field_name, field_value);
            }

            Opcode::IndexSet => {
                let object = self.get_value(operands[0])?;
                let index = self.get_value(operands[1])?;
                let value = self.get_value(operands[2])?;
                object.as_object_mut().index_set(&index.value(), value)?;
            }

            Opcode::IndexGet => {
                let object = self.get_value(operands[1])?;
                let index = self.get_value(operands[2])?;
                let value = object.value().as_object().index_get(&index.value())?;
                self.set_value(operands[0], value)?;
            }

            Opcode::PropGet => {
                let object = self.get_value(operands[1])?;
                let prop = self.load_string(operands[2])?;

                let value = object.value().as_object().property_get(&prop)?;

                self.set_value(operands[0], value)?;
            }
            Opcode::PropSet => {
                let object = self.get_value(operands[0])?;
                let prop = self.load_string(operands[1])?;
                let value = self.get_value(operands[2])?;

                object.as_object_mut().property_set(&prop, value)?;
            }

            // Range Operations
            Opcode::Range => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = Range::new(lhs, rhs)?;
                self.set_value(operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeInclusive => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = Range::inclusive(lhs, rhs)?;
                self.set_value(operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeFull => {
                let value = Range::range_full();
                self.set_value(operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeFrom => {
                let lhs = self.get_value(operands[1])?;
                let value = Range::range_from(lhs)?;
                self.set_value(operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeTo => {
                let lhs = self.get_value(operands[1])?;
                let value = Range::range_to(lhs)?;
                self.set_value(operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeToInclusive => {
                let lhs = self.get_value(operands[1])?;
                let value = Range::range_to_inclusive(lhs)?;
                self.set_value(operands[0], ValueRef::new(value))?;
            }

            // Iteration Support
            Opcode::MakeIter => {
                let obj = self.get_value(operands[1])?;
                match obj.value().downcast_ref::<Enumerator>() {
                    Some(_) => {
                        self.set_value(operands[0], obj.clone())?;
                    }
                    None => {
                        let iterator = obj.value().as_object().make_iterator()?;
                        self.set_value(operands[0], ValueRef::new(Enumerator::new(iterator)))?;
                    }
                }
            }

            Opcode::IterNext => {
                let iterator = self.get_value(operands[2])?;

                match iterator.value_mut().downcast_mut::<Enumerator>() {
                    Some(obj) => {
                        let (next, has_next) = match obj.next() {
                            Some(next) => (next, true),
                            None => (Value::null().into(), false),
                        };
                        self.set_value(operands[0], next)?;
                        self.set_value(operands[1], ValueRef::new(has_next))?;
                    }
                    None => {
                        return Err(RuntimeError::invalid_type::<Enumerator>(&iterator));
                    }
                }
            }

            // Object Method Call
            Opcode::CallMethod => {
                let object = self.get_value(operands[0])?;
                let prop = self.load_string(operands[1])?;
                let arg_count = operands[2].as_immd() as usize;
                // load args from stack
                let mut args = Vec::with_capacity(arg_count);
                for i in 0..arg_count {
                    let arg = self.state.call_arg(i, arg_count)?;
                    args.push(arg);
                }
                let ret = object.as_object_mut().call_method(&prop, &args)?;
                let ret = ret.unwrap_or(ValueRef::null());
                self.state.set_register(RETURN_REGISTER, ret)?;
            }

            // Native method call
            Opcode::CallNative => {
                let func = self.get_value(operands[0])?;
                let arg_count = operands[1].as_immd() as usize;
                match func.value_mut().downcast_mut::<NativeFunction>() {
                    Some(func) => {
                        // load args from stack
                        let mut args = Vec::with_capacity(arg_count);
                        for i in 0..arg_count {
                            let arg = self.state.call_arg(i, arg_count)?;
                            args.push(arg);
                        }
                        let ret = func.call(&args)?;
                        let ret = ret.unwrap_or(Value::null());
                        self.state
                            .set_register(RETURN_REGISTER, ValueRef::from(ret))?;
                    }
                    None => {
                        return Err(RuntimeError::invalid_operand(operands[0]));
                    }
                }
            }

            opcode => {
                return Err(RuntimeError::internal(format!(
                    "unsupported opcode: {opcode:?}"
                )));
            }
        }

        self.state.jump_offset(1);

        Ok(())
    }

    fn get_value(&self, operand: Operand) -> Result<ValueRef, RuntimeError> {
        match operand {
            Operand::Primitive(primitive) => Ok(ValueRef::from_primitive(primitive)),
            Operand::Register(reg) => self.state.get_register(reg),
            Operand::Stack(offset) => self.state.get_value_from_stack(offset),
            Operand::Immd(immd) => Ok(ValueRef::immd(immd)),
            Operand::Symbol(symbol) => {
                Ok(ValueRef::new(UserFunction::new(FunctionId::new(symbol))))
            }
        }
    }

    fn set_value(
        &mut self,
        operand: Operand,
        value: impl Into<ValueRef>,
    ) -> Result<(), RuntimeError> {
        let value = value.into();

        match operand {
            Operand::Register(reg) => self.state.set_register(reg, value),
            Operand::Stack(offset) => self.state.set_value_to_stack(offset, value),
            op => Err(RuntimeError::invalid_operand(op)),
        }
    }

    fn load_string(&self, operand: Operand) -> Result<Arc<String>, RuntimeError> {
        let const_id = operand.as_immd();
        let name = &self.program.constants[const_id as usize];

        match name {
            Constant::String(name) => Ok(name.clone()),
        }
    }
}

/// 帧元数据：返回地址与帧窗口。
///
/// 运行时数据只有 `data_stack` 一个栈（值、实参、局部槽、寄存器备份都在其中），
/// 控制信息改为显式帧对象，不再有独立的 ctrl_stack。
/// 对齐 biujs 的两条不变量：rbp 只在开帧写一次，退帧时由帧派生。
#[derive(Debug, Clone, Copy)]
struct Frame {
    return_pc: usize,
    /// 开帧时的 rsp：既是被调帧的 rbp，也是退帧时 rsp 的回退点。
    base: usize,
}

/// State
/// note: the stack is from bottom to top
#[derive(Debug)]
pub struct State {
    pub data_stack: [ValueRef; STACK_MAX],
    /// 16 个通用寄存器（R0..R15）；Rsp/Rbp 是栈寻址伪寄存器，不在寄存器文件里。
    pub registers: [ValueRef; 16],
    frames: Vec<Frame>,
    pub rsp: usize,
    pub rbp: usize,
    pub pc: usize,
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    pub fn new() -> Self {
        Self {
            data_stack: std::array::from_fn(|_| ValueRef::null()),
            registers: std::array::from_fn(|_| ValueRef::null()),
            frames: Vec::new(),
            rsp: 0,
            rbp: 0,
            pc: 0,
        }
    }

    pub fn jump(&mut self, offset: usize) {
        self.pc = offset;
    }

    pub fn jump_offset(&mut self, offset: isize) {
        self.pc = (self.pc as isize + offset) as usize;
    }

    pub fn rsp(&self) -> usize {
        self.rsp
    }

    pub fn rsp_mut(&mut self) -> &mut usize {
        &mut self.rsp
    }

    /// 开帧：保存返回地址与帧基址，rbp 指向参数顶（= 当前 rsp）。
    /// 之后 callee 的 AddC 只推进 rsp，局部槽落在 `[rbp+slot]`。
    pub fn open_frame(&mut self, callee_pc: usize) -> Result<(), RuntimeError> {
        if self.frames.len() >= STACK_MAX {
            return Err(RuntimeError::StackOverflow);
        }
        self.frames.push(Frame {
            return_pc: self.pc + 1,
            base: self.rsp,
        });
        self.rbp = self.rsp;
        self.jump(callee_pc);
        Ok(())
    }

    /// 收帧：一次回退 rsp/rbp 并恢复 pc；返回 false 表示已是最外层。
    pub fn close_frame(&mut self) -> bool {
        match self.frames.pop() {
            Some(frame) => {
                // rsp 回到参数顶（丢弃 callee 的 AddC 局部帧），调用方随后
                // 用 SubC 清参数、Pop 恢复备份寄存器。
                self.rsp = frame.base;
                self.rbp = self.frames.last().map(|f| f.base).unwrap_or(0);
                self.jump(frame.return_pc);
                true
            }
            None => false,
        }
    }

    /// 实参按逆序压栈，arg0 最后入栈：`arg[i]` 位于 `rsp - 1 - i`
    /// （等价于旧实现建帧后的 `[rbp - (i+1)]`）。
    pub fn call_arg(&self, index: usize, argc: usize) -> Result<ValueRef, RuntimeError> {
        if index >= argc {
            return Err(RuntimeError::internal("argument index out of range"));
        }
        let Some(pos) = self.rsp.checked_sub(index + 1) else {
            return Err(RuntimeError::StackOverflow);
        };
        if pos >= STACK_MAX {
            return Err(RuntimeError::StackOverflow);
        }
        Ok(self.data_stack[pos].clone())
    }

    pub fn get_register(&self, reg: Register) -> Result<ValueRef, RuntimeError> {
        match reg {
            Register::Rsp => Err(RuntimeError::InvalidRegisterAccess(reg)),
            Register::Rbp => Err(RuntimeError::InvalidRegisterAccess(reg)),
            _ => {
                let index = reg.as_usize();
                if index >= self.registers.len() {
                    return Err(RuntimeError::InvalidRegisterAccess(reg));
                }
                Ok(self.registers[index].clone())
            }
        }
    }

    pub fn set_register(&mut self, reg: Register, value: ValueRef) -> Result<(), RuntimeError> {
        match reg {
            Register::Rsp => Err(RuntimeError::InvalidRegisterAccess(reg)),
            Register::Rbp => Err(RuntimeError::InvalidRegisterAccess(reg)),
            _ => {
                let index = reg.as_usize();
                if index >= self.registers.len() {
                    return Err(RuntimeError::InvalidRegisterAccess(reg));
                }
                self.registers[index] = value;
                Ok(())
            }
        }
    }

    pub fn push(&mut self, value: ValueRef) -> Result<(), RuntimeError> {
        if self.rsp >= STACK_MAX {
            return Err(RuntimeError::StackOverflow);
        }
        self.data_stack[self.rsp] = value;
        self.rsp += 1;
        Ok(())
    }

    pub fn pop(&mut self) -> Result<ValueRef, RuntimeError> {
        if self.rsp == 0 {
            return Err(RuntimeError::StackOverflow);
        }
        self.rsp -= 1;
        Ok(self.data_stack[self.rsp].clone())
    }

    pub fn get_value_from_stack(&self, offset: isize) -> Result<ValueRef, RuntimeError> {
        let index = self.rbp as isize + offset;
        if index < 0 || index as usize >= STACK_MAX {
            return Err(RuntimeError::StackOverflow);
        }
        Ok(self.data_stack[index as usize].clone())
    }

    pub fn set_value_to_stack(
        &mut self,
        offset: isize,
        value: ValueRef,
    ) -> Result<(), RuntimeError> {
        let index = self.rbp as isize + offset;
        if index < 0 || index as usize >= STACK_MAX {
            return Err(RuntimeError::StackOverflow);
        }
        self.data_stack[index as usize] = value;
        Ok(())
    }
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "VM: rsp: {}, rbp: {}, frames: {}, pc: {}",
            self.rsp,
            self.rbp,
            self.frames.len(),
            self.pc
        )?;
        let end = self.rsp.min(STACK_MAX);
        writeln!(f, "Data Stack: {:?}", &self.data_stack[0..end])?;
        writeln!(f, "=== Registers ===")?;
        // 每8个寄存器成组打印
        for chunk in self.registers.chunks(8) {
            let start = chunk.as_ptr() as usize - self.registers.as_ptr() as usize;
            let start_idx = start / std::mem::size_of::<ValueRef>();
            // 第一行：寄存器名称
            for offset in 0..chunk.len() {
                write!(f, "r{:<3} ", start_idx + offset)?;
            }
            writeln!(f)?;
            // 第二行：寄存器内容
            for reg in chunk {
                write!(f, "{:<15} ", reg)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod state_tests {
    use super::*;

    #[test]
    fn register_file_is_sixteen_general_registers() {
        let mut state = State::new();
        assert_eq!(state.registers.len(), 16);
        assert_eq!(RETURN_REGISTER, Register::R0);
        assert!(matches!(
            state.get_register(Register::Rsp),
            Err(RuntimeError::InvalidRegisterAccess(_))
        ));
        assert!(matches!(
            state.set_register(Register::Rbp, ValueRef::null()),
            Err(RuntimeError::InvalidRegisterAccess(_))
        ));
        assert!(
            state
                .set_register(Register::R15, ValueRef::new(5i64))
                .is_ok()
        );
        assert_eq!(
            format!("{}", state.get_register(Register::R15).unwrap()),
            "5"
        );
    }

    #[test]
    fn push_pop_and_empty_stack_bounds() {
        let mut state = State::new();
        state.push(ValueRef::new(9i64)).unwrap();
        assert_eq!(state.rsp, 1);
        let value = state.pop().unwrap();
        assert_eq!(format!("{value}"), "9");
        assert_eq!(state.rsp, 0);
        assert!(matches!(state.pop(), Err(RuntimeError::StackOverflow)));
    }

    #[test]
    fn stack_slots_are_rbp_relative() {
        let mut state = State::new();
        assert!(matches!(
            state.get_value_from_stack(-1),
            Err(RuntimeError::StackOverflow)
        ));
        state.set_value_to_stack(2, ValueRef::new(7i64)).unwrap();
        assert_eq!(format!("{}", state.get_value_from_stack(2).unwrap()), "7");

        state.rbp = 100;
        assert_eq!(
            format!("{}", state.get_value_from_stack(2).unwrap()),
            "null"
        );
        state.set_value_to_stack(2, ValueRef::new(8i64)).unwrap();
        assert_eq!(format!("{}", state.get_value_from_stack(2).unwrap()), "8");
    }

    #[test]
    fn call_args_are_rsp_relative_and_bounded() {
        let mut state = State::new();
        for i in 1..=3i64 {
            state.push(ValueRef::new(i)).unwrap();
        }
        // arg0 最后入栈，因此位于 rsp-1。
        assert_eq!(format!("{}", state.call_arg(0, 3).unwrap()), "3");
        assert_eq!(format!("{}", state.call_arg(1, 3).unwrap()), "2");
        assert_eq!(format!("{}", state.call_arg(2, 3).unwrap()), "1");
        assert!(state.call_arg(3, 3).is_err());
    }

    #[test]
    fn open_and_close_frame_restore_state() {
        let mut state = State::new();
        state.push(ValueRef::new(1i64)).unwrap();
        let base = state.rsp;

        state.open_frame(42).unwrap();
        assert_eq!(state.pc, 42);
        assert_eq!(state.rbp, base);
        assert_eq!(state.frames.len(), 1);

        // 模拟被调函数推进栈指针、执行到别的 pc。
        state.rsp += 5;
        state.jump(50);
        assert!(state.close_frame());
        assert_eq!(state.rsp, base, "callee frame is discarded");
        assert_eq!(state.rbp, 0);
        assert_eq!(state.pc, 1, "return_pc is the instruction after Call");
        assert!(!state.close_frame(), "outermost frame has no caller");
    }

    #[test]
    fn frame_depth_is_bounded() {
        let mut state = State::new();
        for _ in 0..STACK_MAX {
            state.open_frame(0).unwrap();
        }
        assert!(matches!(
            state.open_frame(0),
            Err(RuntimeError::StackOverflow)
        ));
    }

    #[test]
    fn jump_offset_moves_pc() {
        let mut state = State::new();
        state.jump(10);
        state.jump_offset(-3);
        assert_eq!(state.pc, 7);
        state.jump_offset(5);
        assert_eq!(state.pc, 12);
    }
}
