# CODEBUDDY.md This file provides guidance to CodeBuddy when working with code in this repository.

## Commands

命令在仓库根目录执行；表格中的环境变量按所用 shell 设置，这里给 POSIX 写法（CI 为 ubuntu-latest）。

| 命令 | 说明 |
| --- | --- |
| `cargo build` | 构建库与默认 target，也是 CI 的第一步。 |
| `cargo test` | CI 的默认测试步骤，覆盖 lib 单测与集成测试。 |
| `cargo test --no-default-features` | CI 的第二步。关闭默认 `async` feature 后，`ValueRef` 改用 `Rc<RefCell<Value>>`，`VM::run` 变成同步函数；新增的 `futures`/`tokio` 用法必须用 `#[cfg(feature = "async")]` 包住。 |
| `cargo test --all-targets --no-fail-fast` | 本地全量回归，跨所有测试 target。 |
| `cargo test --lib` | 分模块纯函数单测：`src/unit_tests.rs` 以及 `parser.rs`、`regalloc.rs`、`vm.rs`、`ast/interpreter.rs` 内的模块内测试。 |
| `cargo test --test test_smoke` | 修改验证用的冒烟套件：16 个表驱动脚本加输入边界。 |
| `cargo test --test test_system` | 多模块系统测试（编译器→Module→VM→宿主）。 |
| `cargo test --test test_correctness` | 实现正确性套件：字节码不变量、编译确定性、与参考实现对照。 |
| `cargo test --lib ir_cfg::block_layout_iter_rev_and_positions` | 按完整路径跑单个单测；集成测试同理，如 `cargo test --test test_smoke smoke_suite`。 |
| `EVALIT_UPDATE_GOLDEN=1 cargo test --test test_bytecode_golden` | 重新生成 `tests/golden/bytecode/` 下的 8 份字节码快照；普通运行前取消该环境变量。 |
| `cargo fmt` / `cargo fmt -- --check` | 格式化 / 校验格式。 |
| `cargo clippy --lib --tests` | 静态检查。`--all-targets` 目前会被 `tests/test_basic_types.rs` 里既有的 `approx_constant` 告警拦住。 |
| `cargo bench --bench vm_benchmark` | criterion 基准，Cargo.toml 里声明为 `harness = false`。 |

## Architecture

### 入口与编译产物

`evalit` 是自有脚本语法的 toy 解释器（edition 2024，Cargo.toml `default = ["async"]`）。`src/lib.rs` 是唯一对外出口：`Compiler`/`compile`、`Interpreter`/`eval::<T>`、`eval_async`（仅 async）、`Module`、`VM`/`Environment`/`Value`/`ValueRef`/`Object`/`Null`/`NativeFunction`、`Error`/`RuntimeError`。`Interpreter::eval` 与 `eval::<T>` 内部完成 `Compiler::compile` → `Arc<Module>` → `VM::new(...).run()`；需要复用编译产物时直接 `compile()` 再自己建 `VM`。`Environment` 通过 `insert`/`with_variable`/`define_function`/`with_function` 注入宿主值与 native 函数，`remove_as::<T>()` 取回。

README.md 的示例已过时：它用 `Interpreter::eval_script`、`Evaluator`，且没有导入 `Environment`。以 `src/lib.rs` 的导出和 `examples/eval.rs` 为准，不要照抄 README 片段。README 的 syntax 章节（primitive type、variable、expression/operator、control flow、fn item）仍是语言语法的有效索引。

### 编译流水线

`src/compiler.rs::compile_inner` 是一条固定顺序的流水线，改动任一阶段都要意识到下游依赖：pest `parse_file` → `TypeContext::check_type_def` → `SemanticAnalyzer::analyze_program` → `TypeChecker::check_program` → `ASTLower::lower_program` 产出 `IrUnit`（一个主 CFG + 每个函数一个 CFG）→ **主 CFG 与每个函数 CFG 各跑一次** `SSABuilder::convert_to_ssa` → `Codegen::generate_code` → 拼装 `Module`。

`Module.instructions` 里主程序代码在前，各函数代码按 `unit.functions` 顺序追加；`symtab` 记录每个函数的起始 pc，`Jump`/`BrIf` 用相对 pc，`debug_instructions` 用 `func_offset` 重定位后再追加。这个「先主后函数」的布局是 `compile_inner` 与 `Module::Display`（排序后输出 symtab）共同约定的，也是字节码 golden 快照的比对基础：`tests/test_bytecode_golden.rs` 只 dump constants / symtab / instructions，IR 克隆不进快照。

IR 用**块参数表达 phi**（`Block.params` 配 `Jump/BrIf.args`），支配边界 + RPO 布局，`Instruction::defined_and_used_vars` 是活跃性分析的依据。SSA 阶段刻意全部使用有序容器并排序 phi 放置点、`handle_jump_args` 的实参与块参数顺序对齐——`HashSet` 迭代顺序会让同一份源码跨进程生成不同字节码，golden 会假失败。新增一条 IR 指令要同步 `defined_and_used`、`Display`、codegen 的 match 和 `Opcode` 四处。

### 寄存器分配与调用约定（reg + stack，为 AOT 保留）

用户已明确裁决：**保留寄存器分配与 reg+stack 混合模式，不走 biujs 的 `FrameSlots` 全内存驻留**，因为后续要做 AOT。不要「顺手简化」成纯栈帧。

`src/compiler/regalloc.rs` 的契约（文件头有完整不变量说明）：每个变量一个永不变更的固定栈槽，栈槽是跨块权威值；寄存器只是块内加速副本。块入口 `begin_block()` 清空 `reg_set`，块出口 `spill_all_bound()` 全量落栈，phi 由 `store_jump_args` 每条边写槽并 `invalidate` 旧副本——单趟按布局顺序的 codegen 无法知道真正走的是哪条前驱，编译期 `reg_set` 在块入口不可信。`alloc` 返回**按序**的 `Vec<Action>`（先 Spill 被驱逐者、再 Restore 目标），`codegen.emit_actions` 必须全部发射；同一条 IR 指令内已生成的操作数寄存器进入 `locked`，驱逐与固定寄存器分配都不得抢占。活跃区间用逐块后向存活扫描构造精确 `ranges`，按 `ranges` 判重叠而非整体 start/end。

寄存器模型向真实机器靠拢：`Register::general()` 是 16 个虚拟通用寄存器 R0..R15，`RETURN_REGISTER = Register::R0`（类比 x86-64 RAX / AArch64 x0），**没有专用 Rv**；`Rsp`/`Rbp` 是栈寻址伪寄存器，不在 16 槽寄存器文件里。`Return` 无论有无值都写 R0（无值写 `Primitive::Null`），顶层 `Ret` 从 R0 取结果，`Halt` 返回 null。R0 是 caller-clobbered：调用点 `spill_return_register()` 把它上面的活跃值落槽，`in_use_registers()` 排除 R0 以免 `Pop` 备份时覆盖返回值，result 目标走 `alloc_def()` 避免 Restore 抢先覆写 R0。四类调用序列（`gen_call`/`gen_call_ex`/`gen_call_native`/`gen_prop_call`）共享这套约定。

预留但**不要实现**：`regalloc.rs` 头部注明 AOT 阶段把寄存器/栈槽抽象成 `Location`；异常边引入后 handler 入口复用「清空 reg_set + 从栈槽 Restore」。

### 运行时：单栈 + 显式帧

`src/runtime/vm.rs::State` 只有一个数据栈：`data_stack: [ValueRef; STACK_MAX]`（值、实参、局部槽、寄存器备份都在其中）+ `registers: [ValueRef; 16]` + `frames: Vec<Frame>` + `rsp/rbp/pc`。独立的 `ctrl_stack` 与 `PushC/PopC/MovC` 指令已删除，控制信息收进 `Frame { return_pc, base }`。

`open_frame(callee_pc)` 保存返回地址、令 `rbp = rsp`（参数顶）并跳到被调函数；`close_frame()` 一次回退 `rsp`/`rbp`/`pc`，返回 `false` 表示已是最外层。不变量：`rbp` 只在开帧写，退帧时由帧派生。`Operand::Stack(offset)` 是 `[rbp+offset]`：函数参数在负偏移（`LoadArg` 用 `[rbp-1-i]`），局部槽在正偏移（函数入口 `AddC rsp += stack_size`）；native/method 调用不改 rbp，实参按 `rsp-1-i` 由 `State::call_arg` 读取。`AddC/SubC` 只操作 rsp 且已做越界检查。调用方 cleanup 是 `SubC rsp -= argc`、逆序 `Pop` 备份寄存器、`Mov result, R0`。

`VM::run` 在 async feature 下是 `async fn`（`futures::executor::block_on` 或直接用 `eval`/`Interpreter::eval` 包装），关闭 feature 时是同步 fn；`eval` 返回类型在两种 feature 下分别为 `Error<'_>` / `Error`，编写条件编译代码时注意。`ValueRef` 对应 `Arc<RwLock<Value>>` 或 `Rc<RefCell<Value>>`。

### 错误边界与未实现特性

`Error` 只有 `Compile(CompileError)` 与 `Runtime(RuntimeError)` 两层；编译期错误由 `ErrorKind` 分类，语义层 `ErrKind::Unsupported` 明确拒绝 `?` 表达式和 enum 声明（避免落到 `lowering.rs` 的 `unimplemented!`），VM 未支持 opcode 返回 `RuntimeError::internal`。`try/catch/finally/throw` 在 grammar 里只是保留字，**没有语法、AST、lowering 或 VM 实现**。异常处理方案调研结论（推荐 Exception Region 静态区域表、不搬 biujs 的 `seh_stack`）已存档，用户明确「暂时不开工」；AOT 同样只预留了 `src/bytecode.rs::Module` 注释里的 `eh_regions` 侧表与 pc→native offset 重定位入口。除非用户改口，不要动手实现这两块。

### 测试体系与仓库约定

四层：`src/unit_tests.rs` 按模块分组测 crate 内可见的纯函数（它用 `#[path]` 挂在 `compiler` 模块下，因为 `ir`/`ast`/`symbol` 是私有子模块，不要为此放宽生产代码可见性）；`vm.rs`/`regalloc.rs` 内的模块内测试覆盖私有状态；`tests/test_system.rs`、`test_smoke.rs`、`test_correctness.rs` 分别对应系统、冒烟、正确性；`tests/test_bytecode_golden.rs` + 8 份 golden 快照防止编译产物非确定性变化，另有 `test_regalloc.rs`/`test_stack.rs`/`test_abi.rs` 等回归护栏。

仓库中的文本文件以 **LF** 行尾存储；Windows 上 git 的自动换行转换会把工作区显示为 CRLF，提交时再转回 LF，因此新写文件用 LF，不要按工作区显示去「统一」行尾（编辑已被转换的现有文件时保持其工作区行尾，可避免整文件 diff 噪声）。golden `.txt` 同为 LF，golden 测试读取时会归一化行尾。CI（`.github/workflows/rust.yml`）只有 `cargo build`、`cargo test`、`cargo test --no-default-features` 三步，没有 fmt/clippy/bench 步骤。用户偏好「修复好一处，提交一次代码」，提交无需 push。
