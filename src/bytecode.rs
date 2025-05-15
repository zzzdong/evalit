use std::{collections::HashMap, fmt, sync::Arc};

pub const MIN_REQUIRED_REGISTER: usize = 3;

#[derive(Debug, Clone)]
pub struct Module {
    pub name: Option<String>,
    pub constants: Vec<Constant>,
    pub symtab: HashMap<FunctionId, usize>,
    pub instructions: Vec<Bytecode>,
}

impl Module {
    pub fn new(
        name: impl Into<Option<String>>,
        constants: Vec<Constant>,
        symtab: HashMap<FunctionId, usize>,
        instructions: Vec<Bytecode>,
    ) -> Self {
        Self {
            name: name.into(),
            constants,
            symtab,
            instructions,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Bytecode {
    pub opcode: Opcode,
    pub operands: [Operand; 3],
}

impl Bytecode {
    pub fn empty(opcode: Opcode) -> Self {
        Self {
            opcode,
            operands: [Operand::Immd(0); 3],
        }
    }

    pub fn single(opcode: Opcode, operand: Operand) -> Self {
        Self {
            opcode,
            operands: [operand, Operand::Immd(0), Operand::Immd(0)],
        }
    }

    pub fn double(opcode: Opcode, dst: Operand, src: Operand) -> Self {
        Self {
            opcode,
            operands: [dst, src, Operand::Immd(0)],
        }
    }

    pub fn triple(opcode: Opcode, dst: Operand, src1: Operand, src2: Operand) -> Self {
        Self {
            opcode,
            operands: [dst, src1, src2],
        }
    }
}

impl fmt::Display for Bytecode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.opcode)?;
        if let Some((last, operands)) = self.operands.split_last() {
            for operand in operands {
                write!(f, " {operand},")?;
            }
            write!(f, " {last}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    /// load_const dst, const_id
    LoadConst,
    /// load_env dst, name
    LoadEnv,
    /// halt
    Halt,
    /// push src
    Push,
    /// pop dst
    Pop,
    /// pushc offset
    PushC,
    /// popc dst
    PopC,
    /// addc offset
    AddC,
    /// subc offset
    SubC,
    /// movc
    MovC,
    /// call func_id
    Call,
    /// call_ex callable
    CallEx,
    /// call_native callable args_count
    CallNative,
    /// ret value
    Ret,
    /// mov dst, src
    Mov,
    /// br offset
    Br,
    /// br_if cond, true_offset, false_offset
    BrIf,
    /// not dst, src
    Not,
    /// neg dst, src
    Neg,
    /// addx dst, src1, src2 (object addition)
    Addx,
    /// subx dst, src1, src2 (object subtraction)
    Subx,
    /// mulx dst, src1, src2 (object multiplication)
    Mulx,
    /// divx dst, src1, src2 (object division)
    Divx,
    /// remx dst, src1, src2 (object remainder)
    Remx,
    /// and dst, src1, src2
    And,
    /// or dst, src1, src2
    Or,
    /// less dst, src1, src2
    Less,
    /// less_equal dst, src1, src2
    LessEqual,
    /// greater dst, src1, src2
    Greater,
    /// greater_equal dst, src1, src2
    GreaterEqual,
    /// equal dst, src1, src2
    Equal,
    /// not_equal dst, src1, src2
    NotEqual,
    /// range dst, src1, src2
    /// begin..end
    Range,
    /// range_inclusive dst, src1, src2
    /// begin..=end
    RangeInclusive,
    /// range_from dst, src1, src2
    /// begin..
    RangeFrom,
    /// range_full dst, src1, src2
    /// ..
    RangeFull,
    /// range_to dst, src1, src2
    /// ..end
    RangeTo,
    /// range_to_inclusive dst, src1, src2
    /// ..=end
    RangeToInclusive,
    /// make_iter dst, src
    MakeIter,
    /// iter_next dst, src
    IterNext,
    /// make_array dst
    MakeArray,
    /// array_push dst, src
    ArrayPush,
    /// make_map dst
    MakeMap,
    /// index_get dst, obj, idx
    IndexGet,
    /// index_set obj, idx, value
    IndexSet,
    /// make_slice dst, object, range
    MakeSlice,
    /// prop_get dst, obj, prop
    PropGet,
    /// prop_set obj, prop, value
    PropSet,
    /// method_call dst, obj, method
    MethodCall,
    /// await dst, promise
    Await,
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Opcode::Br => write!(f, "br"),
            Opcode::BrIf => write!(f, "br_if"),
            Opcode::Halt => write!(f, "halt"),
            Opcode::Push => write!(f, "push"),
            Opcode::Pop => write!(f, "pop"),
            Opcode::PushC => write!(f, "pushc"),
            Opcode::PopC => write!(f, "popc"),
            Opcode::MovC => write!(f, "movc"),
            Opcode::AddC => write!(f, "addc"),
            Opcode::SubC => write!(f, "subc"),
            Opcode::Call => write!(f, "call"),
            Opcode::CallEx => write!(f, "call_ex"),
            Opcode::CallNative => write!(f, "call_native"),
            Opcode::Ret => write!(f, "ret"),
            Opcode::LoadConst => write!(f, "load_const"),
            Opcode::LoadEnv => write!(f, "load_env"),
            Opcode::Mov => write!(f, "mov"),
            Opcode::Not => write!(f, "not"),
            Opcode::Neg => write!(f, "neg"),
            Opcode::Addx => write!(f, "addx"),
            Opcode::Subx => write!(f, "subx"),
            Opcode::Mulx => write!(f, "mulx"),
            Opcode::Divx => write!(f, "divx"),
            Opcode::Remx => write!(f, "remx"),
            Opcode::And => write!(f, "and"),
            Opcode::Or => write!(f, "or"),
            Opcode::Less => write!(f, "lt"),
            Opcode::LessEqual => write!(f, "lte"),
            Opcode::Greater => write!(f, "gt"),
            Opcode::GreaterEqual => write!(f, "gte"),
            Opcode::Equal => write!(f, "eq"),
            Opcode::NotEqual => write!(f, "ne"),
            Opcode::Range => write!(f, "range"),
            Opcode::RangeInclusive => write!(f, "range_inclusive"),
            Opcode::RangeFrom => write!(f, "range_from"),
            Opcode::RangeFull => write!(f, "range_full"),
            Opcode::RangeTo => write!(f, "range_to"),
            Opcode::RangeToInclusive => write!(f, "range_to_inclusive"),
            Opcode::MakeIter => write!(f, "make_iter"),
            Opcode::IterNext => write!(f, "iter_next"),
            Opcode::MakeArray => write!(f, "make_array"),
            Opcode::ArrayPush => write!(f, "array_push"),
            Opcode::MakeMap => write!(f, "make_map"),
            Opcode::IndexGet => write!(f, "index_get"),
            Opcode::IndexSet => write!(f, "index_set"),
            Opcode::MakeSlice => write!(f, "make_slice"),
            Opcode::PropGet => write!(f, "prop_get"),
            Opcode::PropSet => write!(f, "prop_set"),
            Opcode::MethodCall => write!(f, "method_call"),
            Opcode::Await => write!(f, "await"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum Operand {
    Primitive(Primitive),
    Register(Register),
    Stack(isize),
    Immd(isize),
    Symbol(u32),
}

impl Operand {
    pub fn new_immd(immd: isize) -> Self {
        Self::Immd(immd)
    }

    pub fn new_primitive(primitive: Primitive) -> Self {
        Self::Primitive(primitive)
    }

    pub fn new_register(reg: Register) -> Self {
        Self::Register(reg)
    }

    pub fn new_stack(offset: isize) -> Self {
        Self::Stack(offset)
    }

    pub fn new_symbol(id: u32) -> Self {
        Self::Symbol(id)
    }

    pub fn as_immd(&self) -> isize {
        match self {
            Operand::Immd(immd) => *immd,
            _ => panic!("{self:?} not an immediate"),
        }
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Primitive(primitive) => write!(f, "{primitive}"),
            Operand::Register(reg) => write!(f, "{reg}"),
            Operand::Stack(offset) => write!(f, "[rbp{offset:+}]"),
            Operand::Immd(immd) => write!(f, "{immd}"),
            Operand::Symbol(id) => write!(f, "sym_{id}"),
        }
    }
}

impl From<Register> for Operand {
    fn from(reg: Register) -> Self {
        Operand::Register(reg)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum Register {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    /// Stack pointer
    Rsp,
    /// Base pointer
    Rbp,
    /// Return value
    Rv,
}

impl Register {
    pub fn as_usize(&self) -> usize {
        *self as usize
    }

    pub fn general() -> [Register; 16] {
        [
            Register::R0,
            Register::R1,
            Register::R2,
            Register::R3,
            Register::R4,
            Register::R5,
            Register::R6,
            Register::R7,
            Register::R8,
            Register::R9,
            Register::R10,
            Register::R11,
            Register::R12,
            Register::R13,
            Register::R14,
            Register::R15,
        ]
    }

    pub fn small_general() -> [Register; 4] {
        [Register::R0, Register::R1, Register::R2, Register::R3]
    }

    pub fn all() -> [Register; 19] {
        [
            Register::R0,
            Register::R1,
            Register::R2,
            Register::R3,
            Register::R4,
            Register::R5,
            Register::R6,
            Register::R7,
            Register::R8,
            Register::R9,
            Register::R10,
            Register::R11,
            Register::R12,
            Register::R13,
            Register::R14,
            Register::R15,
            Register::Rsp,
            Register::Rbp,
            Register::Rv,
        ]
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::R0 => write!(f, "r0"),
            Register::R1 => write!(f, "r1"),
            Register::R2 => write!(f, "r2"),
            Register::R3 => write!(f, "r3"),
            Register::R4 => write!(f, "r4"),
            Register::R5 => write!(f, "r5"),
            Register::R6 => write!(f, "r6"),
            Register::R7 => write!(f, "r7"),
            Register::R8 => write!(f, "r8"),
            Register::R9 => write!(f, "r9"),
            Register::R10 => write!(f, "r10"),
            Register::R11 => write!(f, "r11"),
            Register::R12 => write!(f, "r12"),
            Register::R13 => write!(f, "r13"),
            Register::R14 => write!(f, "r14"),
            Register::R15 => write!(f, "r15"),
            Register::Rsp => write!(f, "rsp"),
            Register::Rbp => write!(f, "rbp"),
            Register::Rv => write!(f, "rv"),
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Primitive {
    #[default]
    Null,
    Boolean(bool),
    Byte(u8),
    Integer(i64),
    Float(f64),
    Char(char),
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Boolean(b) => write!(f, "{b}"),
            Primitive::Byte(b) => write!(f, "{b}"),
            Primitive::Integer(i) => write!(f, "{i}"),
            Primitive::Float(ff) => write!(f, "{ff}"),
            Primitive::Char(c) => write!(f, "{c}"),
            Primitive::Null => write!(f, "null"),
        }
    }
}

impl From<bool> for Primitive {
    fn from(value: bool) -> Self {
        Primitive::Boolean(value)
    }
}

impl From<i64> for Primitive {
    fn from(value: i64) -> Self {
        Primitive::Integer(value)
    }
}

impl From<f64> for Primitive {
    fn from(value: f64) -> Self {
        Primitive::Float(value)
    }
}

impl From<char> for Primitive {
    fn from(value: char) -> Self {
        Primitive::Char(value)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Constant {
    String(Arc<String>),
}

impl From<&str> for Constant {
    fn from(value: &str) -> Self {
        Constant::String(Arc::new(value.to_string()))
    }
}

impl From<String> for Constant {
    fn from(value: String) -> Self {
        Constant::String(Arc::new(value.to_string()))
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::String(s) => write!(f, "\"{s}\""),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ConstantId(u32);

impl ConstantId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn as_isize(&self) -> isize {
        self.0 as isize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct FunctionId(u32);

impl FunctionId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn as_isize(&self) -> isize {
        self.0 as isize
    }
}
