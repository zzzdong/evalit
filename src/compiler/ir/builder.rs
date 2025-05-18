use crate::bytecode::Constant;
use crate::bytecode::FunctionId;
use crate::bytecode::Opcode;

use super::instruction::*;

pub trait InstBuilder {
    fn module(&self) -> &IrUnit;

    fn module_mut(&mut self) -> &mut IrUnit;

    fn control_flow_graph(&self) -> &ControlFlowGraph;

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph;

    fn set_entry(&mut self, entry: BlockId) {
        self.control_flow_graph_mut().set_entry(entry);
    }

    fn emit(&mut self, inst: Instruction) {
        self.control_flow_graph_mut().emit(inst);
    }

    fn make_halt(&mut self) {
        self.emit(Instruction::Halt);
    }

    fn alloc(&mut self) -> Value {
        self.control_flow_graph_mut().create_variable()
    }

    fn make_constant(&mut self, value: Constant) -> Value {
        let const_id = self.module_mut().make_constant(value);
        Value::Constant(const_id)
    }

    fn load_constant(&mut self, value: Constant) -> Value {
        let const_id = self.module_mut().make_constant(value);
        let dst = self.alloc();
        self.emit(Instruction::LoadConst {
            dst,
            const_id: Value::Constant(const_id),
        });
        dst
    }

    fn load_arg(&mut self, index: usize) -> Value {
        let addr = self.alloc();
        self.emit(Instruction::LoadArg { dst: addr, index });
        addr
    }

    fn create_block(&mut self, label: Name) -> BlockId {
        self.control_flow_graph_mut().create_block(label)
    }

    fn current_block(&self) -> BlockId {
        self.control_flow_graph()
            .current_block()
            .expect("no current block")
    }

    fn switch_to_block(&mut self, block: BlockId) {
        self.control_flow_graph_mut().switch_to_block(block);
    }

    fn unaryop(&mut self, op: Opcode, src: Value) -> Value {
        let result = self.alloc();

        self.emit(Instruction::UnaryOp {
            op,
            dst: result,
            src,
        });

        result
    }

    fn binop(&mut self, op: Opcode, lhs: Value, rhs: Value) -> Value {
        let result = self.alloc();

        self.emit(Instruction::BinaryOp {
            op,
            dst: result,
            lhs,
            rhs,
        });

        result
    }

    fn assign(&mut self, dst: Value, src: Value) {
        self.emit(Instruction::Move { dst, src })
    }

    fn get_property(&mut self, object: Value, property: &str) -> Value {
        let result = self.alloc();

        let property = self.make_constant(property.into());

        self.emit(Instruction::PropertyGet {
            dst: result,
            object,
            property,
        });

        result
    }

    fn set_property(&mut self, object: Value, property: &str, value: Value) {
        let property = self.make_constant(property.into());

        self.emit(Instruction::PropertySet {
            object,
            property,
            value,
        });
    }

    fn call_property(&mut self, object: Value, property: &str, args: Vec<Value>) -> Value {
        let dst = self.alloc();

        let property = self.make_constant(property.into());

        self.emit(Instruction::PropertyCall {
            object,
            property,
            args,
            result: dst,
        });

        dst
    }

    fn load_external_variable(&mut self, name: String) -> Value {
        let result = self.alloc();

        let name = self.make_constant(name.into());

        self.emit(Instruction::LoadEnv { dst: result, name });

        result
    }

    fn call_function(&mut self, func: FunctionId, args: Vec<Value>) -> Value {
        let result = self.alloc();

        if args.len() > 8 {
            panic!("too many arguments");
        }

        self.emit(Instruction::Call {
            func: Value::Function(func),
            args,
            result,
        });

        result
    }

    fn make_call(&mut self, func: Value, args: Vec<Value>) -> Value {
        let result = self.alloc();

        if args.len() > 8 {
            panic!("too many arguments");
        }

        self.emit(Instruction::CallEx {
            callable: func,
            args,
            result,
        });

        result
    }

    fn make_call_native(&mut self, func: Value, args: Vec<Value>) -> Value {
        let result = self.alloc();

        if args.len() > 8 {
            panic!("too many arguments");
        }

        self.emit(Instruction::CallNative { func, args, result });

        result
    }

    fn br(&mut self, dst_blk: BlockId) {
        self.emit(Instruction::Br {
            dst: Value::Block(dst_blk),
        });
    }

    fn br_if(&mut self, condition: Value, true_blk: BlockId, false_blk: BlockId) {
        self.emit(Instruction::BrIf {
            condition,
            true_blk: Value::Block(true_blk),
            false_blk: Value::Block(false_blk),
        });
    }
    fn return_(&mut self, value: Option<Value>) {
        self.emit(Instruction::Return { value });
    }

    fn make_iterator(&mut self, iter: Value) -> Value {
        let result = self.alloc();

        self.emit(Instruction::MakeIterator {
            src: iter,
            dst: result,
        });

        result
    }

    fn iterate_next(&mut self, iter: Value) -> Value {
        let next = self.alloc();
        self.emit(Instruction::IterateNext { iter, dst: next });

        next
    }

    fn make_range(&mut self, op: Opcode, begin: Option<Value>, end: Option<Value>) -> Value {
        let result = self.alloc();

        self.emit(Instruction::MakeRange {
            op,
            begin,
            end,
            result,
        });

        result
    }

    // fn make_range_inclusive(&mut self, op: Opcode, begin: Option<Value>, end: Option<Value>) -> Value {
    //     let result = self.alloc();

    //     self.emit(Instruction::MakeRangeInclusive {
    //         op,
    //         begin,
    //         end,
    //         result,
    //     });

    //     result
    // }

    fn make_array(&mut self) -> Value {
        let array = self.alloc();
        self.emit(Instruction::MakeArray { dst: array });
        array
    }

    fn array_push(&mut self, array: Value, value: Value) -> Value {
        self.emit(Instruction::ArrayPush { array, value });
        array
    }

    fn make_map(&mut self) -> Value {
        let map = self.alloc();
        self.emit(Instruction::MakeMap { dst: map });
        map
    }

    fn index_get(&mut self, object: Value, index: Value) -> Value {
        let dst = self.alloc();
        self.emit(Instruction::IndexGet { dst, object, index });
        dst
    }

    fn index_set(&mut self, object: Value, index: Value, value: Value) {
        self.emit(Instruction::IndexSet {
            object,
            index,
            value,
        });
    }

    fn make_slice(&mut self, object: Value, range: Value) -> Value {
        let dst = self.alloc();
        self.emit(Instruction::MakeSlice { dst, object, range });
        dst
    }

    fn await_promise(&mut self, promise: Value) -> Value {
        let dst = self.alloc();
        self.emit(Instruction::Await { dst, promise });
        dst
    }
}

#[derive(Debug)]
pub struct IrBuilder<'a> {
    module: &'a mut IrUnit,
}

impl<'a> IrBuilder<'a> {
    pub fn new(module: &'a mut IrUnit) -> Self {
        Self { module }
    }
}

impl InstBuilder for IrBuilder<'_> {
    fn module(&self) -> &IrUnit {
        self.module
    }

    fn module_mut(&mut self) -> &mut IrUnit {
        self.module
    }

    fn control_flow_graph(&self) -> &ControlFlowGraph {
        &self.module.control_flow_graph
    }

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph {
        &mut self.module.control_flow_graph
    }
}

pub struct FunctionBuilder<'long, 'short: 'long> {
    module: &'long mut IrUnit,
    function: &'short mut IrFunction,
}

impl<'long, 'short: 'long> FunctionBuilder<'long, 'short> {
    pub fn new(module: &'long mut IrUnit, function: &'short mut IrFunction) -> Self {
        Self { module, function }
    }
}

impl<'long, 'short: 'long> InstBuilder for FunctionBuilder<'long, 'short> {
    fn module(&self) -> &IrUnit {
        self.module
    }

    fn module_mut(&mut self) -> &mut IrUnit {
        self.module
    }

    fn control_flow_graph(&self) -> &ControlFlowGraph {
        &self.function.control_flow_graph
    }

    fn control_flow_graph_mut(&mut self) -> &mut ControlFlowGraph {
        &mut self.function.control_flow_graph
    }
}
