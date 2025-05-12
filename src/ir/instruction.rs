use std::{collections::HashMap, fmt};

use petgraph::graph::{DiGraph, NodeIndex};

use crate::bytecode::{Constant, ConstantId, FunctionId, Opcode, Operand, Primitive};

macro_rules! id_entity {
    ($name: ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(usize);

        impl $name {
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

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}<{}>", std::any::type_name::<$name>(), self.0)
            }
        }
    };
}

id_entity!(VariableId);

id_entity!(BlockId);

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
    Variable(VariableId),
    /// A constant
    Constant(ConstantId),
    /// A function
    Function(FunctionId),
    /// A block
    Block(BlockId),
}

impl Value {
    pub fn new(id: VariableId) -> Self {
        Value::Variable(id)
    }

    pub fn as_variable(&self) -> VariableId {
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
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Variable(id) => write!(f, "%{}", id.as_usize()),
            Value::Function(id) => write!(f, "@function({})", id.as_usize()),
            Value::Block(id) => write!(f, "@block({})", id.as_usize()),
            Value::Primitive(prim) => write!(f, "{prim}"),
            Value::Constant(id) => write!(f, "#{}", id.as_usize()),
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

impl From<VariableId> for Value {
    fn from(id: VariableId) -> Self {
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
    Await {
        promise: Value,
        dst: Value,
    },
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
    PropertyCall {
        object: Value,
        property: Value,
        args: Vec<Value>,
        result: Value,
    },
    Return {
        value: Option<Value>,
    },
    BrIf {
        condition: Value,
        true_blk: Value,
        false_blk: Value,
    },
    Br {
        dst: Value,
    },
    /// Create an iterator from an object.
    /// The iterator will be stored in `result`.
    MakeIterator {
        dst: Value,
        src: Value,
    },
    /// Check if the iterator has another item.
    IteratorHasNext {
        dst: Value,
        iter: Value,
    },
    /// Get the next value from an iterator.
    /// The next value will be stored in `next`.
    IterateNext {
        dst: Value,
        iter: Value,
    },
    /// Create a range iterator.
    MakeRange {
        op: Opcode,
        begin: Option<Value>,
        end: Option<Value>,
        result: Value,
    },
    /// Create an array.
    MakeArray {
        dst: Value,
    },
    /// Push a value to an array.
    ArrayPush {
        array: Value,
        value: Value,
    },
    /// Create a map.
    MakeMap {
        dst: Value,
    },
    /// Get a value from an indexable object.
    IndexGet {
        dst: Value,
        object: Value,
        index: Value,
    },
    /// Set a value to an indexable object.
    IndexSet {
        object: Value,
        index: Value,
        value: Value,
    },
    /// Create a slice.
    MakeSlice {
        dst: Value,
        object: Value,
        range: Value,
    },
    Halt,
}

impl Instruction {
    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            Instruction::Halt
                | Instruction::Return { .. }
                | Instruction::Br { .. }
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

    pub fn defined_and_used_vars(&self) -> (Vec<Value>, Vec<Value>) {
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
            } => (vec![], vec![*condition, *true_blk, *false_blk]),
            Instruction::Br { dst } => (vec![], vec![*dst]),
            Instruction::MakeIterator {
                src: iter,
                dst: result,
            } => (vec![*result], vec![*iter]),
            Instruction::IterateNext { iter, dst: next } => (vec![*next], vec![*iter]),
            Instruction::IteratorHasNext { iter, dst: result } => (vec![*result], vec![*iter]),
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
                write!(f, "{dst} = call_ex {callable}")?;
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
            Instruction::Br { dst } => {
                write!(f, "br {dst}")
            }
            Instruction::BrIf {
                condition,
                true_blk,
                false_blk,
            } => {
                write!(f, "br_if {condition}, {true_blk}, {false_blk}")
            }
            Instruction::MakeIterator { src, dst } => {
                write!(f, "{dst} = make_iterator {src}")
            }
            Instruction::IteratorHasNext { iter, dst } => {
                write!(f, "{dst} = iterator_has_next {iter}")
            }
            Instruction::IterateNext { iter, dst } => {
                write!(f, "{dst} = iterate_next {iter}")
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
            // Instruction::MakeRangeInclusive {
            //     op,
            //     begin,
            //     end,
            //     result,
            // } => {
            //     write!(f, "{result} = make_range {op} ")?;
            //     if let Some(begin) = begin {
            //         write!(f, "{}", begin)?;
            //     }
            //     write!(f, "..=",)?;
            //     if let Some(end) = end {
            //         write!(f, " {}", end)?;
            //     }
            //     Ok(())
            // }
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

    pub fn iter(&self) -> InstructionsIterator {
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

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub(crate) blocks: Vec<Block>,
    pub(crate) entry: Option<BlockId>,
    pub(crate) current_block: Option<BlockId>,
    pub(crate) variables: Vec<VariableId>,
    pub(crate) graph: DiGraph<BlockId, Instruction>,
    pub(crate) block_node_map: HashMap<BlockId, NodeIndex>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            entry: None,
            current_block: None,
            variables: Vec::new(),
            graph: DiGraph::new(),
            block_node_map: HashMap::new(),
        }
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn create_block(&mut self, label: impl Into<Name>) -> BlockId {
        let id = BlockId::new(self.blocks.len());
        self.blocks.push(Block::new(id, label));
        let node = self.graph.add_node(id);
        self.block_node_map.insert(id, node);
        id
    }

    pub(crate) fn create_block_with_instructions(
        &mut self,
        label: impl Into<Name>,
        instructions: Vec<Instruction>,
    ) -> BlockId {
        let id = BlockId::new(self.blocks.len());
        let block = Block::new(id, label).with_instructions(instructions);
        self.blocks.push(block);
        let node = self.graph.add_node(id);
        self.block_node_map.insert(id, node);
        id
    }

    pub fn entry(&self) -> Option<BlockId> {
        self.entry
    }

    pub fn set_entry(&mut self, entry: BlockId) {
        self.entry = Some(entry);
    }

    pub fn current_block(&self) -> Option<BlockId> {
        self.current_block
    }

    pub fn emit(&mut self, inst: Instruction) {
        match &inst {
            Instruction::Br { dst } => {
                let curr = self.current_block.expect("no current block");
                let dst = dst.as_block().expect("not a block");

                self.block_append_successor(curr, dst, inst.clone());
            }
            Instruction::BrIf {
                true_blk,
                false_blk,
                ..
            } => {
                let curr = self.current_block.expect("no current block");
                let then_blk = true_blk.as_block().expect("not a block");
                let else_blk = false_blk.as_block().expect("not a block");

                self.block_append_successor(curr, then_blk, inst.clone());
                self.block_append_successor(curr, else_blk, inst.clone());
            }
            _ => {}
        }

        self.current_block
            .and_then(|curr| self.blocks.get_mut(curr.as_usize()))
            .expect("no current block")
            .emit(inst);
    }

    pub fn create_variable(&mut self) -> Value {
        let id = VariableId::new(self.variables.len());
        self.variables.push(id);
        Value::new(id)
    }

    pub(crate) fn get_block(&self, id: BlockId) -> Option<&Block> {
        self.blocks.get(id.as_usize())
    }

    pub fn block_append_successor(
        &mut self,
        block: BlockId,
        successors: BlockId,
        inst: Instruction,
    ) {
        self.graph.add_edge(
            self.block_node_map[&block],
            self.block_node_map[&successors],
            inst,
        );
    }

    pub fn block_remove_successor(&mut self, block: BlockId, successors: BlockId) {
        if let Some(edge) = self.graph.find_edge(
            self.block_node_map[&block],
            self.block_node_map[&successors],
        ) {
            self.graph.remove_edge(edge);
        }
    }
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        Self::new()
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

#[derive(Debug, Clone)]
pub struct Block {
    pub(crate) id: BlockId,
    pub(crate) label: Name,
    pub(crate) instructions: Vec<Instruction>,
}

impl Block {
    pub fn new(id: BlockId, label: impl Into<Name>) -> Self {
        Self {
            id,
            label: label.into(),
            instructions: Vec::new(),
        }
    }

    pub fn emit(&mut self, instruction: Instruction) {
        self.instructions.push(instruction);
    }

    pub fn with_instructions(mut self, instructions: Vec<Instruction>) -> Self {
        self.instructions = instructions;
        self
    }
}

#[derive(Debug, Clone)]
pub struct FuncParam {
    pub name: Name,
}

impl FuncParam {
    pub fn new(name: impl Into<Name>) -> Self {
        Self { name: name.into() }
    }
}

#[derive(Debug, Clone)]
pub struct FuncSignature {
    pub name: Name,
    pub params: Vec<FuncParam>,
}

impl FuncSignature {
    pub fn new(name: impl Into<Name>, params: Vec<FuncParam>) -> Self {
        Self {
            name: name.into(),
            params,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IrFunction {
    pub id: FunctionId,
    pub signature: FuncSignature,
    pub control_flow_graph: ControlFlowGraph,
}

impl IrFunction {
    pub fn new(id: FunctionId, signature: FuncSignature) -> Self {
        Self {
            id,
            signature,
            control_flow_graph: ControlFlowGraph::new(),
        }
    }
}

#[derive(Debug)]
pub struct IrUnit {
    pub constants: Vec<Constant>,
    pub functions: Vec<IrFunction>,
    pub control_flow_graph: ControlFlowGraph,
}

impl IrUnit {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            functions: Vec::new(),
            control_flow_graph: ControlFlowGraph::new(),
        }
    }

    pub fn get_function(&self, id: FunctionId) -> Option<&IrFunction> {
        self.functions.get(id.as_usize())
    }

    pub fn find_function(&self, name: &str) -> Option<&IrFunction> {
        self.functions.iter().find(|f| match f.signature.name.0 {
            Some(ref n) => n == name,
            None => false,
        })
    }

    pub fn make_constant(&mut self, value: Constant) -> ConstantId {
        match self.constants.iter().position(|item| item == &value) {
            Some(index) => ConstantId::new(index as u32),
            None => {
                let id = ConstantId::new(self.constants.len() as u32);
                self.constants.push(value);
                id
            }
        }
    }

    pub fn declare_function(&mut self, signature: FuncSignature) -> FunctionId {
        let id = FunctionId::new(self.functions.len() as u32);
        self.functions.push(IrFunction::new(id, signature));
        id
    }

    pub fn define_function(&mut self, id: FunctionId, func: IrFunction) {
        let old = &mut self.functions[id.as_usize()];

        let _ = std::mem::replace(old, func);
    }

    pub fn get_block(&self, id: BlockId) -> Option<&Block> {
        self.control_flow_graph.blocks.get(id.as_usize())
    }

    pub fn with_control_flow_graph(mut self, control_flow_graph: ControlFlowGraph) -> Self {
        self.control_flow_graph = control_flow_graph;
        self
    }
}
