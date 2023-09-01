use std::fmt;

use crate::bytecode::Register;

use super::{RuntimeError, value::ValueRef};

const STACK_MAX: usize = 0x0FFF;

/// VM
/// note: the stack is from bottom to top
pub struct VM {
    pub data_stack: [ValueRef; STACK_MAX],
    pub ctrl_stack: [usize; STACK_MAX],
    pub registers: [ValueRef; 20],
    pub rsp: usize,
    pub rbp: usize,
    pub ctrl_rsp: usize,
    pub pc: usize,
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
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
            Register::RSP => Err(RuntimeError::InvalidRegisterAccess(reg)),
            Register::RBP => Err(RuntimeError::InvalidRegisterAccess(reg)),
            Register::PC => Err(RuntimeError::InvalidRegisterAccess(reg)),
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
            Register::RSP => Err(RuntimeError::InvalidRegisterAccess(reg)),
            Register::RBP => Err(RuntimeError::InvalidRegisterAccess(reg)),
            Register::PC => Err(RuntimeError::InvalidRegisterAccess(reg)),
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

    pub fn set_value_to_stack(&mut self, offset: isize, value: ValueRef) -> Result<(), RuntimeError> {
        let index = self.rbp as isize + offset;
        if index < 0 || index as usize >= STACK_MAX {
            return Err(RuntimeError::StackOverflow);
        }
        self.data_stack[index as usize] = value;
        Ok(())
    }
}

impl fmt::Display for VM {
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