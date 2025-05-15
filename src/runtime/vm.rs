use std::{
    collections::HashMap,
    fmt::{self},
};

use log::debug;

#[cfg(feature = "async")]
use super::Promise;
use super::{
    Enumerator, Environment, NativeFunction, Object, Range, RuntimeError, UserFunction, Value,
    value::ValueRef,
};
use crate::bytecode::{Bytecode, FunctionId, Module, Opcode, Operand, Register};

const STACK_MAX: usize = 0x0FFF;

#[derive(Debug)]
pub struct Program {
    constants: Vec<ValueRef>,
    instructions: Vec<Bytecode>,
    symtab: HashMap<FunctionId, usize>,
}

impl Program {
    pub fn new(module: Module) -> Self {
        let Module {
            constants,
            instructions,
            symtab: symbles,
            ..
        } = module;

        let constants = constants.into_iter().map(ValueRef::from_constant).collect();

        Self {
            constants,
            instructions,
            symtab: symbles,
        }
    }
}

#[derive(Debug)]
pub struct VM<'a> {
    state: State,
    program: &'a Program,
    env: &'a Environment,
}

impl<'a> VM<'a> {
    pub fn new(program: &'a Program, env: &'a Environment) -> Self {
        Self {
            state: State::new(),
            program,
            env,
        }
    }

    #[cfg(feature = "async")]
    pub async fn run(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        debug!("===constants===");
        for (i, constant) in self.program.constants.iter().enumerate() {
            debug!("{i}: {constant:?}");
        }
        debug!("===instructions===");
        for (i, inst) in self.program.instructions.iter().enumerate() {
            debug!("{i}: {inst:?}");
        }

        while let Some(inst) = self.program.instructions.get(self.state.pc) {
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
                    self.run_instruction(inst)?;
                }
            }
        }

        Ok(None)
    }

    #[cfg(not(feature = "async"))]
    pub fn run(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        while let Some(inst) = self.program.instructions.get(self.state.pc) {
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
                    self.run_instruction(inst)?;
                }
            }
        }

        Ok(None)
    }

    fn run_instruction(&mut self, inst: &Bytecode) -> Result<(), RuntimeError> {
        let Bytecode { opcode, operands } = inst;

        match opcode {
            // ctrl stack opcode begin
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
            // ctrl stack opcode end
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
            Opcode::CallEx => {
                match operands[0] {
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
                        match value.downcast_ref::<UserFunction>() {
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
                        // value.get_mut().call(&[]);
                        // unimplemented!("call object {value:?}")
                    }

                    _ => return Err(RuntimeError::invalid_operand(operands[0])),
                }
            }

            Opcode::CallNative => {
                let mut func = self.get_value(operands[0])?;
                let arg_count = operands[1].as_immd() as usize;
                match func.downcast_mut::<NativeFunction>() {
                    Some(mut func) => {
                        // load args from stack
                        let mut args = Vec::with_capacity(arg_count);
                        for i in 0..arg_count {
                            let arg = self.get_value(Operand::Stack(-(i as isize + 1)))?;
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

            Opcode::MethodCall => {
                let mut object = self.get_value(operands[0])?;
                let prop = self.load_string(operands[1])?;
                let arg_count = operands[2].as_immd() as usize;
                // load args from stack
                let mut args = Vec::with_capacity(arg_count);
                for i in 0..arg_count {
                    let arg = self.get_value(Operand::Stack(-(i as isize + 1)))?;
                    args.push(arg);
                }
                let ret = object.borrow_mut().method_call(&prop, &args)?;
                let ret = ret.unwrap_or(ValueRef::null());
                self.state.set_register(Register::Rv, ret)?;
            }

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

            Opcode::Not | Opcode::Neg => {
                let value = self.get_value(operands[1])?;
                let value = value.borrow().negate()?;
                self.set_value(operands[0], ValueRef::from(value))?;
            }
            Opcode::Addx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.borrow().add(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Subx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.borrow().sub(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Mulx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.borrow().mul(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Divx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.borrow().div(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Remx => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.borrow().rem(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }

            Opcode::And => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.borrow().logic_and(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }
            Opcode::Or => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let value = lhs.borrow().logic_or(&rhs.value())?;
                self.set_value(operands[0], value)?;
            }

            Opcode::Greater => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let ordering = lhs.borrow().compare(&rhs.value())?;
                self.set_value(
                    operands[0],
                    ValueRef::new(ordering == std::cmp::Ordering::Greater),
                )?;
            }
            Opcode::GreaterEqual => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let ordering = lhs.borrow().compare(&rhs.value())?;
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
                let ordering = lhs.borrow().compare(&rhs.value())?;
                self.set_value(
                    operands[0],
                    ValueRef::new(ordering == std::cmp::Ordering::Less),
                )?;
            }

            Opcode::LessEqual => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let ordering = lhs.borrow().compare(&rhs.value())?;
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
                let eq = lhs.borrow().equal(&rhs.value())?;
                self.set_value(operands[0], eq)?;
            }

            Opcode::NotEqual => {
                let lhs = self.get_value(operands[1])?;
                let rhs = self.get_value(operands[2])?;
                let eq = lhs.borrow().equal(&rhs.value())?;
                let not_eq = !*eq.downcast_ref::<bool>().unwrap();
                self.set_value(operands[0], Value::new(not_eq))?;
            }

            Opcode::LoadConst => {
                let const_index = operands[1].as_immd();
                let value = self.program.constants[const_index as usize].clone();
                self.set_value(operands[0], value)?;
            }

            Opcode::LoadEnv => {
                let name = operands[1].as_immd();
                let name = self.program.constants[name as usize].clone();
                let name = name
                    .downcast_ref::<String>()
                    .ok_or(RuntimeError::invalid_type::<String>(&name))?;
                match self.env.get(name.as_str()) {
                    Some(value) => {
                        self.set_value(operands[0], value.clone())?;
                    }
                    None => {
                        return Err(RuntimeError::symbol_not_found(name.as_str()));
                    }
                }
            }

            Opcode::Br => {
                let offset = operands[0].as_immd();
                self.state.jump_offset(offset);
                return Ok(());
            }

            Opcode::BrIf => {
                let cond = self.get_value(operands[0])?;
                match cond.downcast_ref::<bool>() {
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

            Opcode::MakeIter => {
                let obj = self.get_value(operands[1])?;
                match obj.downcast_ref::<Enumerator>() {
                    Some(_) => {
                        self.set_value(operands[0], obj.clone())?;
                    }
                    None => {
                        let iterator = obj.borrow().make_iterator()?;
                        self.set_value(operands[0], ValueRef::new(Enumerator::new(iterator)))?;
                    }
                }
            }

            Opcode::IterNext => {
                let mut iterator = self.get_value(operands[1])?;
                let next = iterator.borrow_mut().iterate_next()?;
                self.set_value(operands[0], Value::new(next))?;
            }

            Opcode::MakeMap => {
                let map: HashMap<String, ValueRef> = HashMap::new();
                self.set_value(operands[0], ValueRef::new(map))?;
            }

            Opcode::IndexSet => {
                let mut object = self.get_value(operands[0])?;
                let index = self.get_value(operands[1])?;
                let value = self.get_value(operands[2])?;
                object.borrow_mut().index_set(&index.value(), value)?;
            }

            Opcode::IndexGet => {
                let object = self.get_value(operands[1])?;
                let index = self.get_value(operands[2])?;
                let value = object.borrow().index_get(&index.value())?;
                self.set_value(operands[0], value)?;
            }

            Opcode::MakeArray => {
                let array: Vec<ValueRef> = Vec::new();
                self.set_value(operands[0], ValueRef::new(array))?;
            }

            Opcode::ArrayPush => {
                let mut array = self.get_value(operands[0])?;
                let value = self.get_value(operands[1])?;
                let array_cloned = array.clone();
                let mut array = array
                    .downcast_mut::<Vec<ValueRef>>()
                    .ok_or(RuntimeError::invalid_type::<Vec<ValueRef>>(array_cloned))?;
                array.push(value);
            }

            Opcode::MakeSlice => {
                let object = self.get_value(operands[1])?;
                let range = self.get_value(operands[2])?;

                let slice = object.borrow().make_slice(range)?;
                self.set_value(operands[0], slice)?;
            }
            Opcode::PropGet => {
                let object = self.get_value(operands[1])?;
                let prop = self.load_string(operands[2])?;

                let value = object.borrow().property_get(&prop)?;

                self.set_value(operands[0], value)?;
            }
            Opcode::PropSet => {
                let mut object = self.get_value(operands[0])?;
                let prop = self.load_string(operands[1])?;
                let value = self.get_value(operands[2])?;

                object.borrow_mut().property_set(&prop, value)?;
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

    fn load_string(&self, operand: Operand) -> Result<String, RuntimeError> {
        let const_id = operand.as_immd();
        let name = self.program.constants[const_id as usize].clone();
        let name = name
            .downcast_ref::<String>()
            .ok_or(RuntimeError::invalid_type::<String>(&name))?;
        Ok(name.clone())
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
        write!(f, "Registers: {:?}", &self.registers[0..20])?;
        Ok(())
    }
}
