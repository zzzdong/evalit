use std::collections::HashMap;

use log::debug;

#[cfg(feature = "async")]
pub use crate::Promise;
use crate::bytecode::{Bytecode, FunctionId, Module, Opcode, Operand, Register};
use crate::compiler::Compiler;
use crate::runtime::{Enumerator, Range, RuntimeError, UserFunction, VM};
use crate::runtime::{Environment, ValueRef};
use crate::{Error, NativeFunction, Object, Value};

pub struct Interpreter {
    constants: Vec<ValueRef>,
    instructions: Vec<Bytecode>,
    symtab: HashMap<FunctionId, usize>,
    env: Environment,
    vm: VM,
}

impl Interpreter {
    pub fn new(module: Module, env: Environment, vm: VM) -> Self {
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
            env,
            vm,
        }
    }

    #[cfg(not(feature = "async"))]
    pub fn eval_script(script: &str, env: Environment) -> Result<Option<Value>, Error> {
        let compiler = Compiler::new();

        let module = compiler.compile(script, &env)?;

        // log::trace!("module {module}");

        let mut interpreter = Interpreter::new(module, env, VM::new());

        debug!("===constants===");
        for (i, constant) in interpreter.constants.iter().enumerate() {
            debug!("{i}\t{constant:?}");
        }
        debug!("===instructions===");
        for (i, bytecode) in interpreter.instructions.iter().enumerate() {
            debug!("{i}\t{bytecode:?}");
        }
        debug!("===symtab===");
        for (i, (id, offset)) in interpreter.symtab.iter().enumerate() {
            debug!("{i}\t{id:?}\t{offset}");
        }

        let ret = interpreter.run()?;

        Ok(ret.map(ValueRef::take))
    }

    #[cfg(feature = "async")]
    pub fn eval_script(script: &str, env: Environment) -> Result<Option<Value>, Error> {
        futures::executor::block_on(Self::eval_script_async(script, env))
    }

    #[cfg(feature = "async")]
    pub async fn eval_script_async(script: &str, env: Environment) -> Result<Option<Value>, Error> {
        let compiler = Compiler::new();

        let module = compiler.compile(script, &env)?;

        // log::trace!("module {module}");

        let mut interpreter = Interpreter::new(module, env, VM::new());

        debug!("===constants===");
        for (i, constant) in interpreter.constants.iter().enumerate() {
            debug!("{i}\t{constant:?}");
        }
        debug!("===instructions===");
        for (i, bytecode) in interpreter.instructions.iter().enumerate() {
            debug!("{i}\t{bytecode:?}");
        }
        debug!("===symtab===");
        for (i, (id, offset)) in interpreter.symtab.iter().enumerate() {
            debug!("{i}\t{id:?}\t{offset}");
        }

        #[cfg(feature = "async")]
        let ret = interpreter.run_async().await?;

        Ok(ret.map(ValueRef::take))
    }

    #[cfg(feature = "async")]
    pub async fn run_async(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        loop {
            let bytecode = self.instructions.get(self.vm.pc());
            if bytecode.is_none() {
                return Ok(None);
            }

            let bytecode = bytecode.unwrap().clone();

            debug!("{}", self.vm);
            debug!("{bytecode:?}");

            match bytecode.opcode {
                Opcode::Halt => {
                    let ret = self.get_value(Operand::Register(Register::RV))?;
                    return Ok(Some(ret));
                }

                Opcode::Ret => {
                    if self.vm.ctrl_stack_reached_bottom() {
                        let ret = self.get_value(Operand::Register(Register::RV))?;
                        return Ok(Some(ret));
                    }
                    let pc = self.vm.popc()?;

                    self.vm.jump(pc);
                }

                Opcode::Await => {
                    let promise = self.get_value(bytecode.operands[1])?;
                    let mut promise = promise.take();
                    let promise = promise
                        .downcast_mut::<Promise>()
                        .ok_or(RuntimeError::internal("only can await promise"))?;
                    let ret = promise.await;
                    self.set_value(bytecode.operands[0], ret)?;
                    self.vm.jump_offset(1);
                }

                _ => {
                    self.run_instruction(&bytecode)?;
                }
            }
        }
    }

    pub fn run(&mut self) -> Result<Option<ValueRef>, RuntimeError> {
        loop {
            let bytecode = self.instructions.get(self.vm.pc());
            if bytecode.is_none() {
                return Ok(None);
            }

            let bytecode = bytecode.unwrap().clone();

            debug!("{}", self.vm);
            debug!("{bytecode:?}");

            match bytecode.opcode {
                Opcode::Halt => {
                    let ret = self.get_value(Operand::Register(Register::RV))?;
                    return Ok(Some(ret));
                }

                Opcode::Ret => {
                    if self.vm.ctrl_stack_reached_bottom() {
                        let ret = self.get_value(Operand::Register(Register::RV))?;
                        return Ok(Some(ret));
                    }
                    let pc = self.vm.popc()?;

                    self.vm.jump(pc);
                }

                _ => {
                    self.run_instruction(&bytecode)?;
                }
            }
        }
    }

    fn run_instruction(&mut self, bytecode: &Bytecode) -> Result<(), RuntimeError> {
        match bytecode.opcode {
            // ctrl stack opcode begin
            Opcode::MovC => match (bytecode.operands[0], bytecode.operands[1]) {
                (Operand::Register(Register::RSP), Operand::Register(Register::RBP)) => {
                    *self.vm.rsp_mut() = self.vm.rbp();
                }
                (Operand::Register(Register::RBP), Operand::Register(Register::RSP)) => {
                    *self.vm.rbp_mut() = self.vm.rsp();
                }

                _ => unimplemented!("unsupported instruction:{bytecode:?}"),
            },
            Opcode::PushC => match bytecode.operands[0] {
                Operand::Register(Register::RSP) => {
                    self.vm.pushc(self.vm.rsp())?;
                }
                Operand::Register(Register::RBP) => {
                    self.vm.pushc(self.vm.rbp())?;
                }
                _ => unimplemented!("unsupported instruction:{bytecode:?}"),
            },
            Opcode::PopC => match bytecode.operands[0] {
                Operand::Register(Register::RSP) => {
                    let value = self.vm.popc()?;
                    *self.vm.rsp_mut() = value;
                }
                Operand::Register(Register::RBP) => {
                    let value = self.vm.popc()?;
                    *self.vm.rbp_mut() = value;
                }
                _ => unimplemented!("unsupported instruction:{bytecode:?}"),
            },
            Opcode::AddC => match (bytecode.operands[0], bytecode.operands[1]) {
                (Operand::Register(Register::RSP), Operand::Register(Register::RSP)) => {
                    *self.vm.rsp_mut() = self.vm.rsp() + bytecode.operands[2].as_immd() as usize;
                }
                _ => unimplemented!("unsupported instruction:{bytecode:?}"),
            },
            Opcode::SubC => match (bytecode.operands[0], bytecode.operands[1]) {
                (Operand::Register(Register::RSP), Operand::Register(Register::RSP)) => {
                    *self.vm.rsp_mut() = self.vm.rsp() - bytecode.operands[2].as_immd() as usize;
                }
                _ => unimplemented!("unsupported instruction:{bytecode:?}"),
            },
            // ctrl stack opcode end
            Opcode::Call => {
                let func = bytecode.operands[0].as_immd();
                match self.symtab.get(&FunctionId::new(func as u32)) {
                    Some(location) => {
                        self.vm.pushc(self.vm.pc() + 1)?;
                        self.vm.jump(*location);
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
                match bytecode.operands[0] {
                    Operand::Symbol(sym) => match self.symtab.get(&FunctionId::new(sym)) {
                        Some(location) => {
                            self.vm.pushc(self.vm.pc() + 1)?;
                            self.vm.jump(*location);
                            return Ok(());
                        }
                        None => {
                            return Err(RuntimeError::SymbolNotFound {
                                name: format!("{sym}"),
                            });
                        }
                    },
                    Operand::Register(_) | Operand::Stack(_) => {
                        let value = self.get_value(bytecode.operands[0])?;
                        match value.downcast_ref::<UserFunction>() {
                            Some(func) => match self.symtab.get(&func.id()) {
                                Some(location) => {
                                    self.vm.pushc(self.vm.pc() + 1)?;
                                    self.vm.jump(*location);
                                    return Ok(());
                                }
                                None => {
                                    return Err(RuntimeError::SymbolNotFound {
                                        name: format!("{:?}", func.id()),
                                    });
                                }
                            },
                            None => {
                                return Err(RuntimeError::invalid_operand(bytecode.operands[0]));
                            }
                        }
                        // value.get_mut().call(&[]);
                        // unimplemented!("call object {value:?}")
                    }

                    _ => return Err(RuntimeError::invalid_operand(bytecode.operands[0])),
                }
            }

            Opcode::CallNative => {
                let mut func = self.get_value(bytecode.operands[0])?;
                let arg_count = bytecode.operands[1].as_immd() as usize;
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
                        self.vm.set_register(Register::RV, ValueRef::from(ret))?;
                    }
                    None => {
                        return Err(RuntimeError::invalid_operand(bytecode.operands[0]));
                    }
                }
            }

            Opcode::PropCall => {
                let mut object = self.get_value(bytecode.operands[0])?;
                let prop = self.load_string(bytecode.operands[1])?;
                let arg_count = bytecode.operands[2].as_immd() as usize;
                // load args from stack
                let mut args = Vec::with_capacity(arg_count);
                for i in 0..arg_count {
                    let arg = self.get_value(Operand::Stack(-(i as isize + 1)))?;
                    args.push(arg);
                }
                let ret = object.borrow_mut().property_call(&prop, &args)?;
                let ret = ret.unwrap_or(Value::null());
                self.vm.set_register(Register::RV, ValueRef::from(ret))?;
            }

            Opcode::Mov => {
                let value = self.get_value(bytecode.operands[1])?;
                match bytecode.operands[0] {
                    Operand::Register(reg) => {
                        self.vm.set_register(reg, value)?;
                    }
                    Operand::Stack(offset) => {
                        self.vm.set_value_to_stack(offset, value)?;
                    }
                    op => return Err(RuntimeError::invalid_operand(op)),
                }
            }
            Opcode::Push => {
                let value = self.get_value(bytecode.operands[0])?;
                self.vm.push(value)?;
            }
            Opcode::Pop => {
                let value = self.vm.pop()?;
                self.set_value(bytecode.operands[0], value)?;
            }

            Opcode::Not | Opcode::Neg => {
                let value = self.get_value(bytecode.operands[1])?;
                let value = value.borrow().negate()?;
                self.set_value(bytecode.operands[0], ValueRef::from(value))?;
            }
            Opcode::Addx => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let value = lhs.borrow().add(&rhs.value())?;
                self.set_value(bytecode.operands[0], value)?;
            }
            Opcode::Subx => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let value = lhs.borrow().sub(&rhs.value())?;
                self.set_value(bytecode.operands[0], value)?;
            }
            Opcode::Mulx => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let value = lhs.borrow().mul(&rhs.value())?;
                self.set_value(bytecode.operands[0], value)?;
            }
            Opcode::Divx => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let value = lhs.borrow().div(&rhs.value())?;
                self.set_value(bytecode.operands[0], value)?;
            }
            Opcode::Modx => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let value = lhs.borrow().modulo(&rhs.value())?;
                self.set_value(bytecode.operands[0], value)?;
            }

            Opcode::And => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let value = lhs.borrow().logic_and(&rhs.value())?;
                self.set_value(bytecode.operands[0], value)?;
            }
            Opcode::Or => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let value = lhs.borrow().logic_or(&rhs.value())?;
                self.set_value(bytecode.operands[0], value)?;
            }

            Opcode::Greater => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let ordering = lhs.borrow().compare(&rhs.value())?;
                self.set_value(
                    bytecode.operands[0],
                    ValueRef::new(ordering == std::cmp::Ordering::Greater),
                )?;
            }
            Opcode::GreaterEqual => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let ordering = lhs.borrow().compare(&rhs.value())?;
                self.set_value(
                    bytecode.operands[0],
                    ValueRef::new(
                        ordering == std::cmp::Ordering::Greater
                            || ordering == std::cmp::Ordering::Equal,
                    ),
                )?;
            }

            Opcode::Less => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let ordering = lhs.borrow().compare(&rhs.value())?;
                self.set_value(
                    bytecode.operands[0],
                    ValueRef::new(ordering == std::cmp::Ordering::Less),
                )?;
            }

            Opcode::LessEqual => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let ordering = lhs.borrow().compare(&rhs.value())?;
                self.set_value(
                    bytecode.operands[0],
                    ValueRef::new(
                        ordering == std::cmp::Ordering::Less
                            || ordering == std::cmp::Ordering::Equal,
                    ),
                )?;
            }

            Opcode::Equal => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let ordering = lhs.borrow().compare(&rhs.value())?;
                self.set_value(
                    bytecode.operands[0],
                    ValueRef::new(ordering == std::cmp::Ordering::Equal),
                )?;
            }

            Opcode::NotEqual => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let ordering = lhs.borrow().compare(&rhs.value())?;
                self.set_value(
                    bytecode.operands[0],
                    ValueRef::new(ordering != std::cmp::Ordering::Equal),
                )?;
            }

            Opcode::LoadConst => {
                let const_index = bytecode.operands[1].as_immd();
                let value = self.constants[const_index as usize].clone();
                self.set_value(bytecode.operands[0], value)?;
            }

            Opcode::LoadEnv => {
                let name = bytecode.operands[1].as_immd();
                let name = self.constants[name as usize].clone();
                let name = name
                    .downcast_ref::<String>()
                    .ok_or(RuntimeError::invalid_type::<String>(&name))?;
                match self.env.get(name.as_str()) {
                    Some(value) => {
                        self.set_value(bytecode.operands[0], value.clone())?;
                    }
                    None => {
                        return Err(RuntimeError::symbol_not_found(name.as_str()));
                    }
                }
            }

            Opcode::Br => {
                let offset = bytecode.operands[0].as_immd();
                self.vm.jump_offset(offset);
                return Ok(());
            }

            Opcode::BrIf => {
                let cond = self.get_value(bytecode.operands[0])?;
                match cond.downcast_ref::<bool>() {
                    Some(b) => {
                        let offset = if *b {
                            bytecode.operands[1].as_immd()
                        } else {
                            bytecode.operands[2].as_immd()
                        };
                        self.vm.jump_offset(offset);
                    }
                    None => return Err(RuntimeError::invalid_type::<bool>(&cond)),
                }
                return Ok(());
            }

            Opcode::Range => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let value = Range::new(lhs, rhs)?;
                self.set_value(bytecode.operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeInclusive => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let rhs = self.get_value(bytecode.operands[2])?;
                let value = Range::inclusive(lhs, rhs)?;
                self.set_value(bytecode.operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeFull => {
                let value = Range::range_full();
                self.set_value(bytecode.operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeFrom => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let value = Range::range_from(lhs)?;
                self.set_value(bytecode.operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeTo => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let value = Range::range_to(lhs)?;
                self.set_value(bytecode.operands[0], ValueRef::new(value))?;
            }

            Opcode::RangeToInclusive => {
                let lhs = self.get_value(bytecode.operands[1])?;
                let value = Range::range_to_inclusive(lhs)?;
                self.set_value(bytecode.operands[0], ValueRef::new(value))?;
            }

            Opcode::MakeIter => {
                let obj = self.get_value(bytecode.operands[1])?;
                let iterator = obj.borrow().make_iterator()?;
                self.set_value(
                    bytecode.operands[0],
                    ValueRef::new(Enumerator::new(iterator)),
                )?;
            }

            Opcode::IterHasNext => {
                let iterator = self.get_value(bytecode.operands[1])?;
                let value = iterator.borrow().iterator_has_next()?;
                self.set_value(bytecode.operands[0], ValueRef::new(value))?;
            }

            Opcode::IterNext => {
                let mut iterator = self.get_value(bytecode.operands[1])?;
                let value = iterator.borrow_mut().iterate_next()?;
                self.set_value(bytecode.operands[0], value)?;
            }

            Opcode::MakeMap => {
                let map: HashMap<String, ValueRef> = HashMap::new();
                self.set_value(bytecode.operands[0], ValueRef::new(map))?;
            }

            Opcode::IndexSet => {
                let mut object = self.get_value(bytecode.operands[0])?;
                let index = self.get_value(bytecode.operands[1])?;
                let value = self.get_value(bytecode.operands[2])?;
                object.borrow_mut().index_set(&index.value(), value)?;
            }

            Opcode::IndexGet => {
                let object = self.get_value(bytecode.operands[1])?;
                let index = self.get_value(bytecode.operands[2])?;
                let value = object.borrow().index_get(&index.value())?;
                self.set_value(bytecode.operands[0], value)?;
            }

            Opcode::MakeArray => {
                let array: Vec<ValueRef> = Vec::new();
                self.set_value(bytecode.operands[0], ValueRef::new(array))?;
            }

            Opcode::ArrayPush => {
                let mut array = self.get_value(bytecode.operands[0])?;
                let value = self.get_value(bytecode.operands[1])?;
                let array_cloned = array.clone();
                let mut array = array
                    .downcast_mut::<Vec<ValueRef>>()
                    .ok_or(RuntimeError::invalid_type::<Vec<ValueRef>>(array_cloned))?;
                array.push(value);
            }

            Opcode::MakeSlice => {
                let object = self.get_value(bytecode.operands[1])?;
                let range = self.get_value(bytecode.operands[2])?;

                let slice = object.borrow().make_slice(range)?;
                self.set_value(bytecode.operands[0], slice)?;
            }
            Opcode::PropGet => {
                let object = self.get_value(bytecode.operands[1])?;
                let prop = self.load_string(bytecode.operands[2])?;

                let value = object.borrow().property_get(&prop)?;

                self.set_value(bytecode.operands[0], value)?;
            }
            Opcode::PropSet => {
                let mut object = self.get_value(bytecode.operands[0])?;
                let prop = self.load_string(bytecode.operands[1])?;
                let value = self.get_value(bytecode.operands[2])?;

                object.borrow_mut().property_set(&prop, value)?;
            }
            inst => unreachable!("unsupported instruction {inst:?}"),
        }

        self.vm.jump_offset(1);

        Ok(())
    }

    fn get_value(&self, operand: Operand) -> Result<ValueRef, RuntimeError> {
        match operand {
            Operand::Primitive(primitive) => Ok(ValueRef::from_primitive(primitive)),
            Operand::Register(reg) => self.vm.get_register(reg),
            Operand::Stack(offset) => self.vm.get_value_from_stack(offset),
            Operand::Immd(immd) => Ok(ValueRef::immd(immd)),
            Operand::Symbol(symbol) => {
                Ok(ValueRef::new(UserFunction::new(FunctionId::new(symbol))))
            } // op => Err(RuntimeError::UnsupportedOperand(format!("{:?}", op))),
        }
    }

    fn set_value(
        &mut self,
        operand: Operand,
        value: impl Into<ValueRef>,
    ) -> Result<(), RuntimeError> {
        let value = value.into();

        match operand {
            Operand::Register(reg) => self.vm.set_register(reg, value),
            Operand::Stack(offset) => self.vm.set_value_to_stack(offset, value),
            op => Err(RuntimeError::invalid_operand(op)),
        }
    }

    fn load_string(&self, operand: Operand) -> Result<String, RuntimeError> {
        let const_id = operand.as_immd();
        let name = self.constants[const_id as usize].clone();
        let name = name
            .downcast_ref::<String>()
            .ok_or(RuntimeError::invalid_type::<String>(&name))?;
        Ok(name.clone())
    }
}
