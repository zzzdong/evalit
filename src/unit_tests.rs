//! 分模块的纯函数单元测试。
//!
//! 这里覆盖 crate 内可见（`pub` / `pub(crate)`）的无副作用构造、转换、
//! 判定与格式化逻辑；私有项的单测仍留在各自源文件内
//! （`compiler/parser.rs`、`compiler/regalloc.rs`、`runtime/vm.rs`）。
//! 每个子模块对应一个实现模块，便于定位失败点。

mod bytecode {
    use std::collections::HashSet;

    use crate::bytecode::*;

    #[test]
    fn general_registers_are_sixteen_unique() {
        let regs = Register::general();
        assert_eq!(regs.len(), 16);
        assert_eq!(regs.iter().collect::<HashSet<_>>().len(), 16);
        assert_eq!(regs[0], Register::R0);
        assert_eq!(regs[15], Register::R15);
        assert_eq!(RETURN_REGISTER, Register::R0);
    }

    #[test]
    fn register_indices_and_display() {
        assert_eq!(Register::R0.as_usize(), 0);
        assert_eq!(Register::R15.as_usize(), 15);
        assert_eq!(
            Register::small_general(),
            [Register::R0, Register::R1, Register::R2, Register::R3]
        );
        assert_eq!(Register::all().len(), 18);
        assert_eq!(Register::R0.to_string(), "r0");
        assert_eq!(Register::Rsp.to_string(), "rsp");
        assert_eq!(Register::Rbp.to_string(), "rbp");
        assert_eq!(MIN_REQUIRED_REGISTER, 3);
    }

    #[test]
    fn bytecode_constructors_pad_unused_operands() {
        let reg = Operand::new_register(Register::R1);

        let empty = Bytecode::empty(Opcode::Halt);
        assert_eq!(empty.operands, [Operand::Immd(0); 3]);

        let single = Bytecode::single(Opcode::Push, reg);
        assert_eq!(single.operands[0], reg);
        assert_eq!(single.operands[1], Operand::Immd(0));
        assert_eq!(single.operands[2], Operand::Immd(0));

        let double = Bytecode::double(Opcode::Mov, reg, Operand::new_immd(7));
        assert_eq!(double.operands[0], reg);
        assert_eq!(double.operands[1], Operand::new_immd(7));
        assert_eq!(double.operands[2], Operand::Immd(0));

        let triple = Bytecode::triple(Opcode::Addx, reg, reg, reg);
        assert_eq!(triple.operands, [reg, reg, reg]);
    }

    #[test]
    fn operand_display_is_stable() {
        assert_eq!(Operand::Stack(3).to_string(), "[rbp+3]");
        assert_eq!(Operand::Stack(-1).to_string(), "[rbp-1]");
        assert_eq!(Operand::Symbol(7).to_string(), "sym_7");
        assert_eq!(Operand::Immd(-5).to_string(), "-5");
        assert_eq!(Operand::new_primitive(Primitive::Null).to_string(), "null");
        assert_eq!(Operand::new_immd(4).as_immd(), 4);
    }

    #[test]
    fn opcode_display_aliases() {
        assert_eq!(Opcode::Jump.to_string(), "br");
        assert_eq!(Opcode::BrIf.to_string(), "br_if");
        assert_eq!(Opcode::AddC.to_string(), "addc");
        assert_eq!(Opcode::SubC.to_string(), "subc");
        assert_eq!(Opcode::Call.to_string(), "call");
        assert_eq!(Opcode::Ret.to_string(), "ret");
        assert_eq!(Opcode::Halt.to_string(), "halt");
    }

    #[test]
    fn primitive_conversions_and_display() {
        assert_eq!(Primitive::from(42i64), Primitive::Integer(42));
        assert_eq!(Primitive::from(true), Primitive::Boolean(true));
        assert_eq!(Primitive::from('x'), Primitive::Char('x'));
        assert_eq!(Primitive::from(1.5f64), Primitive::Float(1.5));
        assert_eq!(Primitive::Float(1.5).to_string(), "1.5");
        assert_eq!(Primitive::Null.to_string(), "null");
        assert_eq!(Primitive::Byte(7).to_string(), "7");
    }

    #[test]
    fn constant_ids_and_display() {
        assert_eq!(Constant::from("hi").to_string(), "\"hi\"");
        assert_eq!(Constant::from(String::from("hi")), Constant::from("hi"));
        assert_eq!(ConstantId::new(3).as_usize(), 3);
        assert_eq!(ConstantId::new(3).as_isize(), 3);
        assert_eq!(FunctionId::new(9).as_usize(), 9);
        assert_eq!(FunctionId::new(9).to_string(), "9");
    }

    #[test]
    fn bytecode_display_uses_a_fixed_shape() {
        let inst = Bytecode::double(
            Opcode::Mov,
            Operand::new_register(Register::R0),
            Operand::Stack(2),
        );
        assert!(inst.to_string().starts_with("mov r0, [rbp+2]"));
    }
}

mod ir_instruction {
    use crate::bytecode::{ConstantId, FunctionId, Opcode, Operand, Primitive};
    use crate::compiler::ir::instruction::{BlockId, Instruction, Value as IrValue, Variable};

    fn var(id: usize) -> IrValue {
        IrValue::Variable(Variable::new(id))
    }

    #[test]
    fn value_conversions_are_consistent() {
        let v = var(7);
        assert!(v.is_var());
        assert_eq!(v.as_variable(), Some(Variable::new(7)));
        assert_eq!(v.to_variable(), Variable::new(7));
        assert_eq!(v.as_block(), None);
        assert_eq!(v.as_function(), None);
        assert_eq!(v.as_constant(), None);

        let block = IrValue::Block(BlockId::new(3));
        assert_eq!(block.as_block(), Some(BlockId::new(3)));
        assert_eq!(block.to_block(), BlockId::new(3));

        let func = IrValue::Function(FunctionId::new(2));
        assert_eq!(func.as_function(), Some(FunctionId::new(2)));
        let constant = IrValue::Constant(ConstantId::new(5));
        assert_eq!(constant.as_constant(), Some(ConstantId::new(5)));
        assert_eq!(constant.to_operand(), Operand::new_immd(5));
        assert_eq!(
            IrValue::Primitive(Primitive::Integer(9)).to_operand(),
            Operand::new_primitive(Primitive::Integer(9))
        );
        assert_eq!(IrValue::new(Variable::new(1)), var(1));
    }

    #[test]
    #[should_panic(expected = "to_operand")]
    fn to_operand_rejects_non_value() {
        var(0).to_operand();
    }

    #[test]
    fn value_display_is_stable() {
        assert_eq!(var(2).to_string(), "v2");
        assert_eq!(IrValue::Block(BlockId::new(4)).to_string(), "block4");
        assert_eq!(
            IrValue::Function(FunctionId::new(4)).to_string(),
            "function4"
        );
        assert_eq!(IrValue::Constant(ConstantId::new(4)).to_string(), "const4");
        assert_eq!(
            IrValue::Primitive(Primitive::Boolean(true)).to_string(),
            "true"
        );
    }

    #[test]
    fn terminator_and_call_classification() {
        let terminators = vec![
            Instruction::Halt,
            Instruction::Return { value: None },
            Instruction::Jump {
                dst: IrValue::Block(BlockId::new(1)),
                args: vec![],
            },
            Instruction::BrIf {
                condition: var(0),
                true_blk: IrValue::Block(BlockId::new(1)),
                false_blk: IrValue::Block(BlockId::new(2)),
                true_args: vec![],
                false_args: vec![],
            },
        ];
        for inst in &terminators {
            assert!(inst.is_terminator(), "{inst:?}");
            assert!(!inst.is_call(), "{inst:?}");
        }

        let calls = vec![
            Instruction::Call {
                func: IrValue::Function(FunctionId::new(0)),
                args: vec![],
                result: var(1),
            },
            Instruction::CallEx {
                callable: var(0),
                args: vec![],
                result: var(1),
            },
            Instruction::CallNative {
                func: var(0),
                args: vec![],
                result: var(1),
            },
            Instruction::PropertyCall {
                object: var(0),
                property: var(1),
                args: vec![],
                result: var(2),
            },
        ];
        for inst in &calls {
            assert!(inst.is_call(), "{inst:?}");
            assert!(!inst.is_terminator(), "{inst:?}");
        }

        let ordinary = Instruction::Move {
            dst: var(0),
            src: var(1),
        };
        assert!(!ordinary.is_terminator());
        assert!(!ordinary.is_call());
    }

    #[test]
    fn def_use_sets_match_each_instruction_shape() {
        let (defined, used) = Instruction::Move {
            dst: var(0),
            src: var(1),
        }
        .defined_and_used_vars();
        assert_eq!(defined, vec![Variable::new(0)]);
        assert_eq!(used, vec![Variable::new(1)]);

        let (defined, used) = Instruction::BinaryOp {
            op: Opcode::Addx,
            dst: var(0),
            lhs: var(1),
            rhs: var(2),
        }
        .defined_and_used_vars();
        assert_eq!(defined, vec![Variable::new(0)]);
        assert_eq!(used, vec![Variable::new(1), Variable::new(2)]);

        let (defined, used) = Instruction::PropertySet {
            object: var(0),
            property: var(1),
            value: var(2),
        }
        .defined_and_used_vars();
        assert!(defined.is_empty());
        assert_eq!(
            used,
            vec![Variable::new(0), Variable::new(1), Variable::new(2)]
        );

        // Jump 的目标块不是变量，只有跳转实参计入 use。
        let (defined, used) = Instruction::Jump {
            dst: IrValue::Block(BlockId::new(1)),
            args: vec![var(3)],
        }
        .defined_and_used_vars();
        assert!(defined.is_empty());
        assert_eq!(used, vec![Variable::new(3)]);

        let (defined, used) = Instruction::Return {
            value: Some(var(4)),
        }
        .defined_and_used_vars();
        assert!(defined.is_empty());
        assert_eq!(used, vec![Variable::new(4)]);

        let (defined, used) = Instruction::Halt.defined_and_used_vars();
        assert!(defined.is_empty() && used.is_empty());
    }
}

mod ir_cfg {
    use std::collections::BTreeMap;

    use crate::compiler::ir::cfg::{Block, BlockLayout, ControlFlowGraph};
    use crate::compiler::ir::instruction::{BlockId, Instruction, Value as IrValue, Variable};

    #[test]
    fn block_params_and_seal() {
        let mut block = Block::new(BlockId::new(0), "entry");
        assert_eq!(block.id(), BlockId::new(0));
        assert!(block.params().is_empty());
        block.append_block_param(Variable::new(1));
        block.append_block_param(Variable::new(2));
        assert_eq!(block.params(), &[Variable::new(1), Variable::new(2)]);

        block.emit(Instruction::Halt);
        assert_eq!(block.instructions().len(), 1);
        block.seal();
        block.emit(Instruction::Halt);
        assert_eq!(block.instructions().len(), 1, "sealed block drops emits");

        block.set_block_params(vec![Variable::new(9)]);
        assert_eq!(block.params(), &[Variable::new(9)]);
    }

    #[test]
    fn cfg_ids_are_monotonic_and_jump_edges_are_recorded() {
        let mut cfg = ControlFlowGraph::new();
        let entry = cfg.create_block("entry");
        let other = cfg.create_block("other");
        assert_eq!(entry.as_usize(), 0);
        assert_eq!(other.as_usize(), 1);

        cfg.set_entry(entry);
        assert_eq!(cfg.entry(), Some(entry));
        cfg.switch_to_block(entry);
        assert_eq!(cfg.current_block(), Some(entry));

        assert_eq!(cfg.create_variable().as_variable(), Some(Variable::new(0)));
        assert_eq!(cfg.create_variable().as_variable(), Some(Variable::new(1)));

        cfg.emit(Instruction::Jump {
            dst: IrValue::Block(other),
            args: vec![],
        });
        assert_eq!(cfg.get_successors(entry), &vec![other]);
        assert_eq!(cfg.get_precedences(other), &vec![entry]);
        assert_eq!(cfg.get_block(entry).unwrap().instructions().len(), 1);
    }

    #[test]
    fn br_if_records_two_successors() {
        let mut cfg = ControlFlowGraph::new();
        let entry = cfg.create_block("entry");
        let t = cfg.create_block("true");
        let f = cfg.create_block("false");
        cfg.set_entry(entry);
        cfg.switch_to_block(entry);
        cfg.emit(Instruction::BrIf {
            condition: IrValue::Variable(Variable::new(0)),
            true_blk: IrValue::Block(t),
            false_blk: IrValue::Block(f),
            true_args: vec![],
            false_args: vec![],
        });
        assert_eq!(cfg.get_successors(entry), &vec![t, f]);
        assert_eq!(cfg.get_precedences(t), &vec![entry]);
        assert_eq!(cfg.get_precedences(f), &vec![entry]);
    }

    #[test]
    fn block_layout_iter_rev_and_positions() {
        let mut cfg = ControlFlowGraph::new();
        let a = cfg.create_block("a");
        let b = cfg.create_block("b");
        let layout = BlockLayout::new(vec![a, b], BTreeMap::from([(a, 0usize), (b, 1usize)]));
        let forward: Vec<_> = layout.iter(&cfg).map(|block| block.id()).collect();
        let backward: Vec<_> = layout.iter_rev(&cfg).map(|block| block.id()).collect();
        assert_eq!(forward, vec![a, b]);
        assert_eq!(backward, vec![b, a]);
        assert_eq!(layout.get_block_pos(a), 0);
        assert_eq!(layout.get_block_pos(b), 1);
        assert_eq!(layout.blocks(), &[a, b]);
    }
}

mod ir_unit {
    use crate::bytecode::{Constant, FunctionId};
    use crate::compiler::ir::{ControlFlowGraph, FuncParam, FuncSignature, IrUnit, Name};

    #[test]
    fn constants_are_deduplicated_with_stable_ids() {
        let mut unit = IrUnit::new();
        let a = unit.make_constant(Constant::from("a"));
        assert_eq!(a.as_usize(), 0);
        assert_eq!(unit.make_constant(Constant::from("a")), a);
        let b = unit.make_constant(Constant::from("b"));
        assert_eq!(b.as_usize(), 1);
        assert_eq!(unit.constants.len(), 2);
    }

    #[test]
    fn function_signatures_are_deduplicated() {
        let mut unit = IrUnit::new();
        let sig = FuncSignature::new("f", vec![FuncParam::new("x")]);
        let first = unit.declare_function(sig.clone());
        assert_eq!(first, FunctionId::new(0));
        assert_eq!(unit.declare_function(sig.clone()), first);
        assert_eq!(unit.functions.len(), 1);

        let other = FuncSignature::new("g", vec![FuncParam::new("x")]);
        let second = unit.declare_function(other);
        assert_eq!(second, FunctionId::new(1));
        assert_eq!(unit.functions.len(), 2);
    }

    #[test]
    fn anonymous_closures_with_identical_signatures_get_distinct_ids() {
        let mut unit = IrUnit::new();
        let sig = FuncSignature::new(Name::anonymous(), vec![FuncParam::new("x")]);
        let first = unit.declare_function(sig.clone());
        let second = unit.declare_function(sig);
        assert_ne!(first, second, "两个闭包不能共用同一份函数体");
        assert_eq!(unit.functions.len(), 2);
    }

    #[test]
    fn function_lookup_by_id_and_name() {
        let mut unit = IrUnit::new();
        let id = unit.declare_function(FuncSignature::new("f", vec![]));
        assert_eq!(unit.get_function(id).unwrap().id, id);
        assert_eq!(unit.find_function("f").unwrap().id, id);
        assert!(unit.find_function("missing").is_none());
        assert!(unit.get_function(FunctionId::new(99)).is_none());
    }

    #[test]
    fn control_flow_graph_can_be_replaced() {
        let unit = IrUnit::new();
        assert_eq!(unit.control_flow_graph.blocks().len(), 0);
        let replaced = unit.with_control_flow_graph(ControlFlowGraph::new());
        assert_eq!(replaced.control_flow_graph.blocks().len(), 0);
    }
}

mod syntax {
    use std::str::FromStr;

    use crate::compiler::ast::syntax::{BinOp, PrefixOp, Span};

    #[test]
    fn span_merge_and_flags() {
        let a = Span::new(1, 3);
        let b = Span::new(0, 2);
        let merged = a.merge(&b);
        assert_eq!((merged.start, merged.end), (0, 3));
        assert!(!a.is_zero());
        assert!(!a.is_empty());
        assert!(Span::new(0, 0).is_zero());
        assert!(Span::new(4, 4).is_empty());
        assert!(!Span::new(4, 4).is_zero());
    }

    #[test]
    fn span_line_col_on_a_single_line() {
        assert_eq!(Span::new(0, 1).line_col("abc"), Some((1, 1)));
        assert_eq!(Span::new(2, 3).line_col("abc"), Some((1, 3)));
        assert_eq!(Span::new(3, 3).line_col("abc"), Some((1, 4)));
        assert_eq!(Span::new(0, 0).line_col(""), Some((1, 1)));
    }

    #[test]
    fn span_line_col_handles_multiple_lines_and_utf8() {
        // 旧实现用 lines() 累加长度，漏掉换行符，第二行起列号逐行漂移。
        assert_eq!(Span::new(4, 4).line_col("abc\ndef"), Some((2, 1)));
        assert_eq!(Span::new(6, 6).line_col("abc\ndef"), Some((2, 3)));
        assert_eq!(Span::new(3, 3).line_col("a\r\nb"), Some((2, 1)));
        // 列号按字符计，不是字节；多字节字符后的偏移仍然有效。
        assert_eq!(Span::new(4, 4).line_col("中x"), Some((1, 3)));
        assert_eq!(Span::new(1, 1).line_col("中x"), None);
        assert_eq!(Span::new(99, 99).line_col("abc"), None);
    }

    #[test]
    fn binop_display_and_from_str_round_trip() {
        for text in [
            "+", "-", "*", "/", "%", "&&", "||", "<", "<=", ">", ">=", "==", "!=", "as", "..",
            "..=", "::",
        ] {
            let op = BinOp::from_str(text).unwrap_or_else(|e| panic!("{text}: {e}"));
            assert_eq!(op.to_string(), text);
        }
        assert!(BinOp::from_str("??").is_err());
    }

    #[test]
    fn prefixop_from_str() {
        assert_eq!(PrefixOp::from_str("-").unwrap(), PrefixOp::Neg);
        assert_eq!(PrefixOp::from_str("!").unwrap(), PrefixOp::Not);
        assert!(PrefixOp::from_str("+").is_err());
    }
}

mod symbol {
    use crate::compiler::symbol::SymbolTable;

    #[test]
    fn nested_scopes_shadow_then_restore() {
        let mut table = SymbolTable::<i32>::new();
        assert_eq!(table.lookup("x"), None);
        table.insert("x", 1);
        assert_eq!(table.lookup("x"), Some(&1));

        table.enter_scope();
        table.insert("x", 2);
        table.insert("y", 3);
        assert_eq!(table.lookup("x"), Some(&2));
        assert_eq!(table.lookup("y"), Some(&3));

        table.leave_scope();
        assert_eq!(table.lookup("x"), Some(&1));
        assert_eq!(table.lookup("y"), None);
    }

    #[test]
    fn clone_keeps_scope_contents() {
        let mut table = SymbolTable::<String>::new();
        table.insert("k", "v".to_string());
        let copy = table.clone();
        assert_eq!(copy.lookup("k").map(String::as_str), Some("v"));
        table.insert("k", "changed".to_string());
        assert_eq!(copy.lookup("k").map(String::as_str), Some("v"));
    }
}

mod environment {
    use crate::runtime::{EnvVariable, Environment};

    #[test]
    fn insert_get_and_remove_as() {
        let mut env = Environment::new();
        assert!(env.get("a").is_none());
        env.insert("a", 42i64);
        assert!(matches!(env.get("a"), Some(EnvVariable::Value(_))));
        assert_eq!(env.remove_as::<i64>("a"), Some(42));
        assert!(env.get("a").is_none());
        assert_eq!(env.remove_as::<i64>("a"), None);
    }

    #[test]
    fn keys_are_sorted_and_builders_chain() {
        let env = Environment::new()
            .with_variable("b", 2i64)
            .with_variable("a", 1i64);
        let keys: Vec<_> = env.keys().map(String::as_str).collect();
        assert_eq!(keys, vec!["a", "b"]);
        assert_eq!(env.values().count(), 2);
    }

    #[test]
    fn default_matches_new() {
        let env = Environment::default();
        assert!(env.keys().next().is_none());
    }
}

mod values {
    use crate::bytecode::{Constant, Primitive};
    use crate::runtime::{Null, Value, ValueRef};

    #[test]
    fn take_replaces_with_null_but_returns_the_old_value() {
        let slot = ValueRef::new(42i64);
        let first = slot.take();
        assert_eq!(first.downcast_ref::<i64>(), Some(&42));
        let second = slot.take();
        assert_eq!(second, Null);
    }

    #[test]
    fn from_primitive_covers_every_variant() {
        assert_eq!(
            ValueRef::from_primitive(Primitive::Integer(7))
                .take()
                .downcast_ref::<i64>(),
            Some(&7)
        );
        assert_eq!(
            ValueRef::from_primitive(Primitive::Boolean(true))
                .take()
                .downcast_ref::<bool>(),
            Some(&true)
        );
        assert_eq!(
            ValueRef::from_primitive(Primitive::Char('z'))
                .take()
                .downcast_ref::<char>(),
            Some(&'z')
        );
        assert_eq!(
            ValueRef::from_primitive(Primitive::Float(1.5))
                .take()
                .downcast_ref::<f64>(),
            Some(&1.5)
        );
        assert_eq!(ValueRef::from_primitive(Primitive::Null).take(), Null);
    }

    #[test]
    fn value_construction_and_downcast() {
        let value = Value::new(9i64);
        assert_eq!(value.downcast_ref::<i64>(), Some(&9));
        assert_eq!(value.into_inner::<i64>().ok(), Some(9));
        assert_eq!(
            ValueRef::from_constant(&Constant::from("hi"))
                .take()
                .downcast_ref::<String>()
                .map(String::as_str),
            Some("hi")
        );
        assert_eq!(
            ValueRef::from(Value::new(5i64))
                .take()
                .downcast_ref::<i64>(),
            Some(&5)
        );
    }
}

mod errors {
    use crate::bytecode::Operand;
    use crate::error::Error;
    use crate::runtime::RuntimeError;

    #[test]
    fn runtime_error_constructors_and_display() {
        assert_eq!(
            RuntimeError::internal("boom").to_string(),
            "Internal error: boom"
        );
        assert_eq!(
            RuntimeError::symbol_not_found("x").to_string(),
            "Symbol `x` not found"
        );
        assert!(
            RuntimeError::invalid_argument_count(2, 3)
                .to_string()
                .contains("expected `2`")
        );
        assert!(
            RuntimeError::invalid_type::<i64>("oops")
                .to_string()
                .contains("Invalid type")
        );
        assert!(
            RuntimeError::invalid_operand(Operand::Immd(1))
                .to_string()
                .contains("Invalid operand")
        );
        assert_eq!(RuntimeError::Overflow.to_string(), "Overflow");
        assert_eq!(RuntimeError::StackOverflow.to_string(), "Stack overflow");
    }

    #[test]
    fn error_wraps_runtime_and_compile_errors() {
        let err = Error::from(RuntimeError::Overflow);
        assert!(matches!(err, Error::Runtime(RuntimeError::Overflow)));
        assert!(format!("{err:?}").contains("Overflow"));
    }
}
