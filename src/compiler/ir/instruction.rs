use std::fmt;

use crate::bytecode::{ConstantId, FunctionId, Opcode, Operand, Primitive};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(usize);

impl BlockId {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn id(&self) -> usize {
        self.0
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block{}", self.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable(usize);

impl Variable {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn id(&self) -> usize {
        self.0
    }

    pub fn as_usize(&self) -> usize {
        self.0
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl Eq for Primitive {}

impl std::hash::Hash for Primitive {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Primitive::Boolean(b) => b.hash(state),
            Primitive::Byte(b) => b.hash(state),
            Primitive::Integer(i) => i.hash(state),
            Primitive::Float(f) => f.to_bits().hash(state),
            Primitive::Char(c) => c.hash(state),
            Primitive::Null => 1.hash(state),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Hash)]
pub enum Value {
    /// A primitive
    Primitive(Primitive),
    /// A variable
    Variable(Variable),
    /// A constant
    Constant(ConstantId),
    /// A function
    Function(FunctionId),
    /// A block
    Block(BlockId),
}

impl Value {
    pub fn new(id: Variable) -> Self {
        Value::Variable(id)
    }

    pub fn as_variable(&self) -> Option<Variable> {
        match self {
            Value::Variable(id) => Some(*id),
            _ => None,
        }
    }

    pub fn to_variable(&self) -> Variable {
        match self {
            Value::Variable(id) => *id,
            _ => panic!("Variable::id() called on non-variable({self:?})"),
        }
    }

    pub fn as_block(&self) -> Option<BlockId> {
        match self {
            Value::Block(id) => Some(*id),
            _ => None,
        }
    }

    pub fn to_block(self) -> BlockId {
        match self {
            Value::Block(id) => id,
            _ => panic!("Value::to_block() called on non-block({self:?})"),
        }
    }

    pub fn as_function(&self) -> Option<FunctionId> {
        match self {
            Value::Function(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_constant(&self) -> Option<ConstantId> {
        match self {
            Value::Constant(id) => Some(*id),
            _ => None,
        }
    }

    pub fn to_operand(self) -> Operand {
        match self {
            Value::Constant(id) => Operand::new_immd(id.as_isize()),
            Value::Function(id) => Operand::new_immd(id.as_isize()),
            Value::Primitive(prim) => Operand::new_primitive(prim),
            _ => panic!("Value::to_operand() called on non-value({self:?})"),
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Value::Variable(_))
    }
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Variable(id) => write!(f, "v{}", id.as_usize()),
            Value::Function(id) => write!(f, "function{}", id.as_usize()),
            Value::Block(id) => write!(f, "block{}", id.as_usize()),
            Value::Primitive(prim) => write!(f, "{prim}"),
            Value::Constant(id) => write!(f, "const{}", id.as_usize()),
        }
    }
}

impl From<Primitive> for Value {
    fn from(prim: Primitive) -> Self {
        Value::Primitive(prim)
    }
}

impl From<ConstantId> for Value {
    fn from(id: ConstantId) -> Self {
        Value::Constant(id)
    }
}

impl From<Variable> for Value {
    fn from(id: Variable) -> Self {
        Value::Variable(id)
    }
}

impl From<FunctionId> for Value {
    fn from(id: FunctionId) -> Self {
        Value::Function(id)
    }
}

impl From<BlockId> for Value {
    fn from(id: BlockId) -> Self {
        Value::Block(id)
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    // Load and Move Instructions
    LoadArg {
        dst: Value,
        index: usize,
    },
    LoadConst {
        dst: Value,
        const_id: Value,
    },
    LoadEnv {
        dst: Value,
        name: Value,
    },
    Move {
        dst: Value,
        src: Value,
    },

    // Unary and Binary Operators
    UnaryOp {
        op: Opcode,
        dst: Value,
        src: Value,
    },
    BinaryOp {
        op: Opcode,
        dst: Value,
        lhs: Value,
        rhs: Value,
    },

    // Async Support
    Await {
        promise: Value,
        dst: Value,
    },

    // Function Call Instructions
    Call {
        func: Value,
        args: Vec<Value>,
        result: Value,
    },
    CallEx {
        callable: Value,
        args: Vec<Value>,
        result: Value,
    },
    CallNative {
        func: Value,
        args: Vec<Value>,
        result: Value,
    },
    PropertyCall {
        object: Value,
        property: Value,
        args: Vec<Value>,
        result: Value,
    },

    // Property and Index Access
    PropertyGet {
        dst: Value,
        object: Value,
        property: Value,
    },
    PropertySet {
        object: Value,
        property: Value,
        value: Value,
    },
    IndexGet {
        dst: Value,
        object: Value,
        index: Value,
    },
    IndexSet {
        object: Value,
        index: Value,
        value: Value,
    },

    // Collection / Structural Operations
    MakeArray {
        dst: Value,
    },
    ArrayPush {
        array: Value,
        value: Value,
    },
    MakeMap {
        dst: Value,
    },
    MakeSlice {
        dst: Value,
        object: Value,
        range: Value,
    },
    MakeStruct {
        dst: Value,
    },
    MakeStructField {
        object: Value,
        field: Value,
        value: Value,
    },

    // Iteration Instructions
    MakeIterator {
        dst: Value,
        src: Value,
    },
    IterateNext {
        item: Value,
        has_next: Value,
        iter: Value,
    },

    // Control Flow Instructions
    Return {
        value: Option<Value>,
    },
    BrIf {
        condition: Value,
        true_blk: Value,
        false_blk: Value,
        true_args: Vec<Value>,
        false_args: Vec<Value>,
    },
    Jump {
        dst: Value,
        args: Vec<Value>,
    },
    Halt,

    // Range Instructions
    MakeRange {
        op: Opcode,
        begin: Option<Value>,
        end: Option<Value>,
        result: Value,
    },
}

impl Instruction {
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Instruction::Halt
                | Instruction::Return { .. }
                | Instruction::Jump { .. }
                | Instruction::BrIf { .. }
        )
    }

    pub fn is_call(&self) -> bool {
        matches!(
            self,
            Instruction::Call { .. }
                | Instruction::CallEx { .. }
                | Instruction::CallNative { .. }
                | Instruction::PropertyCall { .. }
        )
    }

    pub fn defined_and_used_vars(&self) -> (Vec<Variable>, Vec<Variable>) {
        let (defined, used) = self.defined_and_used();
        (
            defined
                .iter()
                .filter(|v| v.is_var())
                .map(|v| v.to_variable())
                .collect::<Vec<_>>(),
            used.iter()
                .filter(|v| v.is_var())
                .map(|v| v.to_variable())
                .collect::<Vec<_>>(),
        )
    }
    fn defined_and_used(&self) -> (Vec<Value>, Vec<Value>) {
        match self {
            Instruction::LoadArg { dst, .. } => (vec![*dst], vec![]),
            Instruction::LoadConst { dst, .. } => (vec![*dst], vec![]),
            Instruction::LoadEnv { dst, .. } => (vec![*dst], vec![]),
            Instruction::Move { dst, src } => (vec![*dst], vec![*src]),
            Instruction::UnaryOp { op: _, dst, src } => (vec![*dst], vec![*src]),
            Instruction::BinaryOp {
                op: _,
                dst,
                lhs,
                rhs,
            } => (vec![*dst], vec![*lhs, *rhs]),
            Instruction::Await { promise, dst } => (vec![*dst], vec![*promise]),
            Instruction::Call { func, args, result } => {
                let mut used = vec![*func];
                used.extend(args.iter().cloned());
                (vec![*result], used)
            }
            Instruction::CallEx {
                callable,
                args,
                result,
            } => {
                let mut used = vec![*callable];
                used.extend(args.iter().cloned());
                (vec![*result], used)
            }
            Instruction::CallNative { func, args, result } => {
                let mut used = vec![*func];
                used.extend(args.iter().cloned());
                (vec![*result], used)
            }
            Instruction::PropertyGet {
                dst,
                object,
                property,
            } => (vec![*dst], vec![*object, *property]),
            Instruction::PropertySet {
                object,
                property,
                value,
            } => (vec![], vec![*object, *property, *value]),
            Instruction::PropertyCall {
                object,
                property,
                args,
                result,
            } => {
                let mut used = vec![*object, *property];
                used.extend(args.iter().cloned());
                (vec![*result], used)
            }
            Instruction::Return { value } => (vec![], value.iter().cloned().collect()),
            Instruction::BrIf {
                condition,
                true_blk,
                false_blk,
                true_args,
                false_args,
            } => {
                let mut used = vec![*condition, *true_blk, *false_blk];
                used.extend(true_args.iter().cloned());
                used.extend(false_args.iter().cloned());
                (vec![], used)
            }
            Instruction::Jump { dst, args } => {
                let mut used = vec![*dst];
                used.extend(args.iter().cloned());
                (vec![], used)
            }
            Instruction::MakeIterator {
                src: iter,
                dst: result,
            } => (vec![*result], vec![*iter]),
            Instruction::IterateNext {
                iter,
                item,
                has_next,
            } => (vec![*item, *has_next], vec![*iter]),
            Instruction::MakeRange {
                begin, end, result, ..
            } => match (begin, end) {
                (Some(begin), Some(end)) => (vec![*result], vec![*begin, *end]),
                (Some(begin), None) => (vec![*result], vec![*begin]),
                (None, Some(end)) => (vec![*result], vec![*end]),
                (None, None) => (vec![*result], vec![]),
            },
            Instruction::MakeArray { dst } => (vec![*dst], vec![]),
            Instruction::ArrayPush { array, value } => (vec![], vec![*array, *value]),
            Instruction::MakeMap { dst } => (vec![*dst], vec![]),
            Instruction::IndexGet { dst, object, index } => (vec![*dst], vec![*object, *index]),
            Instruction::IndexSet {
                object,
                index,
                value,
            } => (vec![], vec![*object, *index, *value]),
            Instruction::MakeSlice { dst, object, range } => (vec![*dst], vec![*object, *range]),
            Instruction::MakeStruct { dst } => (vec![*dst], vec![]),
            Instruction::MakeStructField {
                object,
                field,
                value,
            } => (vec![], vec![*object, *field, *value]),
            Instruction::Halt => (vec![], vec![]),
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LoadArg { dst, index } => {
                write!(f, "{dst} = load_arg {index}")
            }
            Instruction::LoadConst { dst, const_id: src } => {
                write!(f, "{dst} = load_const {src}")
            }
            Instruction::LoadEnv { dst, name } => {
                write!(f, "{dst} = load_env {name}")
            }
            Instruction::Move { dst, src } => {
                write!(f, "{dst} = move {src}")
            }
            Instruction::UnaryOp { op, dst, src } => {
                write!(f, "{dst} = {op} {src}")
            }
            Instruction::BinaryOp { op, dst, lhs, rhs } => {
                write!(f, "{dst} = {op} {lhs}, {rhs}")
            }

            Instruction::Await { promise, dst } => {
                write!(f, "{dst} = await {promise}")
            }
            Instruction::Call {
                func,
                args,
                result: dst,
            } => {
                write!(f, "{dst} = call {func} ")?;
                for arg in args {
                    write!(f, ", {arg}")?;
                }
                Ok(())
            }
            Instruction::CallEx {
                callable,
                args,
                result: dst,
            } => {
                write!(f, "{dst} = call_ex {callable} ")?;
                for arg in args {
                    write!(f, ", {arg}")?;
                }
                Ok(())
            }
            Instruction::CallNative {
                func,
                args,
                result: dst,
            } => {
                write!(f, "{dst} = call_native {func}")?;
                for arg in args {
                    write!(f, ", {arg}")?;
                }
                Ok(())
            }
            Instruction::PropertyGet {
                dst,
                object,
                property,
            } => {
                write!(f, "{dst} = prop_get {object}.{property}")
            }
            Instruction::PropertySet {
                object,
                property,
                value,
            } => {
                write!(f, "prop_set {object}.{property} {value}")
            }
            Instruction::PropertyCall {
                object,
                property,
                args,
                result: dst,
            } => {
                write!(f, "{dst} = prop_call {object}.{property}")?;
                for arg in args {
                    write!(f, " ,{arg}")?;
                }
                Ok(())
            }
            Instruction::Return { value } => {
                write!(f, "return")?;
                if let Some(value) = value {
                    write!(f, " {value}")?;
                }
                Ok(())
            }
            Instruction::Jump { dst, args } => {
                write!(f, "jump {dst}")?;

                if !args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        write!(f, "{arg}")?;
                        if i != args.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")?;
                }

                Ok(())
            }
            Instruction::BrIf {
                condition,
                true_blk,
                false_blk,
                true_args,
                false_args,
            } => {
                write!(f, "br_if {condition}, ")?;
                write!(f, "{true_blk}")?;
                if !true_args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in true_args.iter().enumerate() {
                        write!(f, "{arg}")?;
                        if i != true_args.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")?;
                }
                write!(f, ", ")?;
                write!(f, "{false_blk}")?;
                if !false_args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in false_args.iter().enumerate() {
                        write!(f, "{arg}")?;
                        if i != false_args.len() - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Instruction::MakeIterator { src, dst } => {
                write!(f, "{dst} = make_iterator {src}")
            }
            Instruction::IterateNext {
                iter,
                item,
                has_next,
            } => {
                write!(f, "{item}, {has_next} = iterate_next {iter}")
            }
            Instruction::MakeRange {
                op,
                begin,
                end,
                result,
            } => {
                write!(f, "{result} = make_range {op} ")?;
                if let Some(begin) = begin {
                    write!(f, "{begin}")?;
                }
                write!(f, "..",)?;
                if let Some(end) = end {
                    write!(f, " {end}")?;
                }
                Ok(())
            }
            Instruction::MakeArray { dst: array } => {
                write!(f, "{array} = make_array")?;
                Ok(())
            }
            Instruction::ArrayPush { array, value } => {
                write!(f, "array_push {array}, {value}")
            }
            Instruction::MakeMap { dst } => {
                write!(f, "{dst} = make_map")
            }
            Instruction::IndexGet { dst, object, index } => {
                write!(f, "{dst} = index_get {object}[{index}]")
            }
            Instruction::IndexSet {
                object,
                index,
                value,
            } => {
                write!(f, "{object}[{index}] = index_set {value}")
            }
            Instruction::MakeSlice { dst, object, range } => {
                write!(f, "{dst} = make_slice {object}[{range}]")
            }
            Instruction::MakeStruct { dst } => {
                write!(f, "{dst} = make_struct")
            }
            Instruction::MakeStructField {
                object,
                field,
                value,
            } => {
                write!(f, "{object}.{field} = {value}")
            }
            Instruction::Halt => write!(f, "halt"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instructions {
    pub insts: Vec<Instruction>,
}

impl Instructions {
    pub fn new(insts: Vec<Instruction>) -> Self {
        Self { insts }
    }

    pub fn push(&mut self, instruction: Instruction) {
        self.insts.push(instruction);
    }

    pub fn iter(&self) -> InstructionsIterator<'_> {
        InstructionsIterator {
            iter: self.insts.iter(),
        }
    }
}

pub struct InstructionsIterator<'a> {
    iter: std::slice::Iter<'a, Instruction>,
}

impl<'a> Iterator for InstructionsIterator<'a> {
    type Item = &'a Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl IntoIterator for Instructions {
    type Item = Instruction;
    type IntoIter = std::vec::IntoIter<Instruction>;

    fn into_iter(self) -> Self::IntoIter {
        self.insts.into_iter()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Name(pub(crate) Option<String>);

impl Name {
    pub fn new(name: impl ToString) -> Self {
        Self(Some(name.to_string()))
    }

    pub fn anonymous() -> Self {
        Self(None)
    }

    pub fn is_anonymous(&self) -> bool {
        self.0.is_none()
    }
}

impl From<Option<String>> for Name {
    fn from(value: Option<String>) -> Self {
        Self(value)
    }
}

impl From<String> for Name {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

impl Default for Name {
    fn default() -> Self {
        Self::anonymous()
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.as_deref().unwrap_or("<anonymous>"))
    }
}
