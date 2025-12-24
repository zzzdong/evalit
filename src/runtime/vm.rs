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
use crate::bytecode::{Bytecode, Constant, FunctionId, Module, Opcode, Operand, Register};
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
                    let ret = self.get_value(Operand::Register(Register::Rv))?;
                    return Ok(Some(ret));
                }

                Opcode::Ret => {
                    if self.state.ctrl_stack_reached_bottom() {
                        let ret = self.get_value(Operand::Register(Register::Rv))?;
                        return Ok(Some(ret));
                    }
                    let pc = self.state.popc()?;

                    self.state.jump(pc);
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
                    let ret = self.get_value(Operand::Register(Register::Rv))?;
                    return Ok(Some(ret));
                }

                Opcode::Ret => {
                    if self.state.ctrl_stack_reached_bottom() {
                        let ret = self.get_value(Operand::Register(Register::Rv))?;
                        return Ok(Some(ret));
                    }
                    let pc = self.state.popc()?;

                    self.state.jump(pc);
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
                        self.state.pushc(self.state.pc() + 1)?;
                        self.state.jump(*location);
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
                        self.state.pushc(self.state.pc() + 1)?;
                        self.state.jump(*location);
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
                                self.state.pushc(self.state.pc() + 1)?;
                                self.state.jump(*location);
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

            Opcode::MovC => match (operands[0], operands[1]) {
                (Operand::Register(Register::Rsp), Operand::Register(Register::Rbp)) => {
                    *self.state.rsp_mut() = self.state.rbp();
                }
                (Operand::Register(Register::Rbp), Operand::Register(Register::Rsp)) => {
                    *self.state.rbp_mut() = self.state.rsp();
                }

                _ => unimplemented!("unsupported instruction:{inst:?}"),
            },
            Opcode::PushC => match operands[0] {
                Operand::Register(Register::Rsp) => {
                    self.state.pushc(self.state.rsp())?;
                }
                Operand::Register(Register::Rbp) => {
                    self.state.pushc(self.state.rbp())?;
                }
                _ => unimplemented!("unsupported instruction:{inst:?}"),
            },
            Opcode::PopC => match operands[0] {
                Operand::Register(Register::Rsp) => {
                    let value = self.state.popc()?;
                    *self.state.rsp_mut() = value;
                }
                Operand::Register(Register::Rbp) => {
                    let value = self.state.popc()?;
                    *self.state.rbp_mut() = value;
                }
                _ => unimplemented!("unsupported instruction:{inst:?}"),
            },
            Opcode::AddC => match (operands[0], operands[1]) {
                (Operand::Register(Register::Rsp), Operand::Register(Register::Rsp)) => {
                    *self.state.rsp_mut() = self.state.rsp() + operands[2].as_immd() as usize;
                }
                _ => unimplemented!("unsupported instruction:{inst:?}"),
            },
            Opcode::SubC => match (operands[0], operands[1]) {
                (Operand::Register(Register::Rsp), Operand::Register(Register::Rsp)) => {
                    *self.state.rsp_mut() = self.state.rsp() - operands[2].as_immd() as usize;
                }
                _ => unimplemented!("unsupported instruction:{inst:?}"),
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
                let iterator = self.get_value(operands[1])?;
                let next = iterator.as_object_mut().iterate_next()?;
                self.set_value(operands[0], Value::new(next))?;
            }

            // Object Method Call
            Opcode::CallMethod => {
                let object = self.get_value(operands[0])?;
                let prop = self.load_string(operands[1])?;
                let arg_count = operands[2].as_immd() as usize;
                // load args from stack
                let mut args = Vec::with_capacity(arg_count);
                for i in 0..arg_count {
                    let offset = -(i as isize + 1);
                    let arg = self.get_value(Operand::Stack(offset))?;
                    args.push(arg);
                }
                let ret = object.as_object_mut().call_method(&prop, &args)?;
                let ret = ret.unwrap_or(ValueRef::null());
                self.state.set_register(Register::Rv, ret)?;
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
                            let offset = -(i as isize + 1);
                            let arg = self.get_value(Operand::Stack(offset))?;
                            args.push(arg);
                        }
                        let ret = func.call(&args)?;
                        let ret = ret.unwrap_or(Value::null());
                        self.state.set_register(Register::Rv, ValueRef::from(ret))?;
                    }
                    None => {
                        return Err(RuntimeError::invalid_operand(operands[0]));
                    }
                }
            }

            inst => unreachable!("unsupported instruction {inst:?}"),
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

/// State
/// note: the stack is from bottom to top
#[derive(Debug)]
pub struct State {
    pub data_stack: [ValueRef; STACK_MAX],
    pub ctrl_stack: [usize; STACK_MAX],
    pub registers: [ValueRef; 20],
    pub rsp: usize,
    pub rbp: usize,
    pub ctrl_rsp: usize,
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
            ctrl_stack: [0; STACK_MAX],
            registers: std::array::from_fn(|_| ValueRef::null()),
            rsp: 0,
            rbp: 0,
            ctrl_rsp: 0,
            pc: 0,
        }
    }

    pub fn ctrl_stack_reached_bottom(&self) -> bool {
        self.ctrl_rsp == 0
    }

    pub fn pc(&self) -> usize {
        self.pc
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

    pub fn rbp(&self) -> usize {
        self.rbp
    }

    pub fn rbp_mut(&mut self) -> &mut usize {
        &mut self.rbp
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

    pub fn pushc(&mut self, value: usize) -> Result<(), RuntimeError> {
        if self.ctrl_rsp >= STACK_MAX {
            return Err(RuntimeError::StackOverflow);
        }
        self.ctrl_stack[self.ctrl_rsp] = value;
        self.ctrl_rsp += 1;
        Ok(())
    }

    pub fn popc(&mut self) -> Result<usize, RuntimeError> {
        if self.ctrl_rsp == 0 {
            return Err(RuntimeError::StackOverflow);
        }
        self.ctrl_rsp -= 1;
        Ok(self.ctrl_stack[self.ctrl_rsp])
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
            "VM: rsp: {}, rbp: {}, ctrl_rsp: {}, pc: {}",
            self.rsp, self.rbp, self.ctrl_rsp, self.pc
        )?;
        writeln!(f, "Data Stack: {:?}", &self.data_stack[0..self.rsp])?;
        writeln!(f, "Control Stack: {:?}", &self.ctrl_stack[0..self.ctrl_rsp])?;
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
