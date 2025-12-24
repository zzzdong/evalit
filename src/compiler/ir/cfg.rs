use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    fmt,
};

use petgraph::{
    Direction,
    algo::{dominators::Dominators, kosaraju_scc, toposort},
    graph::{DiGraph, NodeIndex},
    visit::{DfsPostOrder, EdgeRef},
};

use super::instruction::*;

#[derive(Debug, Clone)]
pub(crate) struct NaturalLoop {
    pub(crate) header: BlockId,
    pub(crate) blocks: Vec<BlockId>,
}

#[derive(Debug, Clone)]
pub struct Block {
    id: BlockId,
    label: Name,
    instructions: Vec<Instruction>,
    params: Vec<Variable>,
    seal: bool,
}

impl Block {
    pub fn new(id: BlockId, label: impl Into<Name>) -> Self {
        Self {
            id,
            label: label.into(),
            instructions: Vec::new(),
            params: Vec::new(),
            seal: false,
        }
    }

    pub fn id(&self) -> BlockId {
        self.id
    }

    pub fn params(&self) -> &[Variable] {
        &self.params
    }

    pub fn params_mut(&mut self) -> &mut Vec<Variable> {
        &mut self.params
    }

    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    pub fn instructions_mut(&mut self) -> &mut Vec<Instruction> {
        &mut self.instructions
    }

    pub fn emit(&mut self, instruction: Instruction) {
        if !self.seal {
            self.instructions.push(instruction);
        }
    }

    pub fn seal(&mut self) {
        self.seal = true;
    }

    pub fn set_block_params(&mut self, params: Vec<Variable>) {
        self.params = params;
    }

    pub fn append_block_param(&mut self, param: Variable) {
        self.params.push(param);
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "block{}", self.id.as_usize())?;
        if !self.params.is_empty() {
            write!(f, "(")?;
            for (i, param) in self.params.iter().enumerate() {
                write!(f, "{param}")?;
                if i < self.params.len() - 1 {
                    write!(f, ",")?;
                }
            }
            write!(f, ")")?;
        }
        writeln!(f)?;
        for (i, inst) in self.instructions.iter().enumerate() {
            writeln!(f, "{i}\t{inst}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    blocks: Vec<Block>,
    entry: Option<BlockId>,
    current_block: Option<BlockId>,
    variables: Vec<Variable>,
    graph: DiGraph<BlockId, ()>,
    block_node_map: BTreeMap<BlockId, NodeIndex>,
    precedences: BTreeMap<BlockId, Vec<BlockId>>,
    successors: BTreeMap<BlockId, Vec<BlockId>>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            entry: None,
            current_block: None,
            variables: Vec::new(),
            graph: DiGraph::new(),
            block_node_map: BTreeMap::new(),
            precedences: BTreeMap::new(),
            successors: BTreeMap::new(),
        }
    }

    pub fn switch_to_block(&mut self, block: BlockId) {
        self.current_block = Some(block);
    }

    pub fn create_block(&mut self, label: impl Into<Name>) -> BlockId {
        let id = BlockId::new(self.blocks.len());
        self.blocks.push(Block::new(id, label));
        let node_index = self.graph.add_node(id);
        self.block_node_map.insert(id, node_index);
        self.precedences.insert(id, Vec::new());
        self.successors.insert(id, Vec::new());
        id
    }

    pub fn seal_block(&mut self, block: BlockId) {
        self.blocks[block.as_usize()].seal();
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

    pub fn blocks(&self) -> &[Block] {
        &self.blocks
    }

    pub fn set_block_params(&mut self, block: BlockId, params: Vec<Variable>) {
        self.blocks
            .get_mut(block.as_usize())
            .expect("no current block")
            .set_block_params(params);
    }

    pub fn append_block_param(&mut self, block: BlockId, param: Variable) {
        self.blocks
            .get_mut(block.as_usize())
            .expect("no current block")
            .append_block_param(param);
    }

    pub fn emit(&mut self, inst: Instruction) {
        let curr = self.current_block.expect("no current block");
        match &inst {
            Instruction::Jump { dst, .. } => {
                let dst = dst.to_block();
                self.successors.entry(curr).or_default().push(dst);
                self.precedences.entry(dst).or_default().push(curr);

                let curr_node = self.block_node_map[&curr];
                let dst_node = self.block_node_map[&dst];
                self.graph.add_edge(curr_node, dst_node, ());
            }
            Instruction::BrIf {
                true_blk,
                false_blk,
                ..
            } => {
                let true_blk = true_blk.to_block();
                let false_blk = false_blk.to_block();
                self.successors.entry(curr).or_default().push(true_blk);
                self.successors.entry(curr).or_default().push(false_blk);
                self.precedences.entry(true_blk).or_default().push(curr);
                self.precedences.entry(false_blk).or_default().push(curr);

                let curr_node = self.block_node_map[&curr];
                let true_node = self.block_node_map[&true_blk];
                let false_node = self.block_node_map[&false_blk];
                self.graph.add_edge(curr_node, true_node, ());
                self.graph.add_edge(curr_node, false_node, ());
            }
            _ => {}
        }

        self.current_block
            .and_then(|curr| self.blocks.get_mut(curr.as_usize()))
            .expect("no current block")
            .emit(inst);
    }

    pub fn create_variable(&mut self) -> Value {
        let id = Variable::new(self.variables.len());
        self.variables.push(id);
        Value::new(id)
    }

    pub(crate) fn get_block(&self, id: BlockId) -> Option<&Block> {
        self.blocks.get(id.as_usize())
    }

    pub(crate) fn get_block_mut(&mut self, id: BlockId) -> Option<&mut Block> {
        self.blocks.get_mut(id.as_usize())
    }

    pub(crate) fn get_block_params(&self, id: BlockId) -> Vec<Variable> {
        self.blocks
            .get(id.as_usize())
            .map(|block| block.params.clone())
            .unwrap_or_default()
    }

    pub fn get_precedences(&self, block_id: BlockId) -> &Vec<BlockId> {
        &self.precedences[&block_id]
    }

    pub fn get_successors(&self, block_id: BlockId) -> &Vec<BlockId> {
        &self.successors[&block_id]
    }

    pub(crate) fn dominators(&self) -> Dominators<NodeIndex> {
        let entry = self.block_node_map[&self.entry.unwrap()];
        petgraph::algo::dominators::simple_fast(&self.graph, entry)
    }

    pub(crate) fn dominance_frontier(
        &self,
        dominators: &Dominators<NodeIndex>,
    ) -> BTreeMap<BlockId, Vec<BlockId>> {
        let mut df: BTreeMap<BlockId, Vec<BlockId>> = BTreeMap::new();

        for block_id in self.block_node_map.keys().cloned() {
            let block_node = self.block_node_map[&block_id];
            let idom_opt = dominators.immediate_dominator(block_node);

            for pred_id in self.get_precedences(block_id) {
                let mut runner_node = self.block_node_map[pred_id];

                while Some(runner_node) != idom_opt {
                    let runner_id = *self.graph.node_weight(runner_node).unwrap();
                    df.entry(runner_id).or_default().push(block_id);

                    if let Some(next) = dominators.immediate_dominator(runner_node) {
                        runner_node = next;
                    } else {
                        break;
                    }
                }
            }
        }

        // 去重
        for (_, list) in df.iter_mut() {
            list.sort_unstable();
            list.dedup();
        }
        df
    }

    /// 计算反向后序遍历布局
    ///
    /// RPO (Reverse Postorder) 的关键特性：
    /// - 从入口块开始进行后序遍历，然后反转
    /// - 循环头（循环的支配者）会出现在循环体之前
    /// - 这确保了在生成代码时，循环结构保持正确的顺序
    pub fn reverse_post_order_layout2(&self) -> BlockLayout {
        let entry = self.block_node_map[&self.entry.unwrap()];
        let dominators = self.dominators();

        // 构建支配树：从支配关系构建父-子映射
        let mut dom_tree: BTreeMap<NodeIndex, Vec<NodeIndex>> = BTreeMap::new();
        for node in self.graph.node_indices() {
            if let Some(idom) = dominators.immediate_dominator(node) {
                dom_tree.entry(idom).or_default().push(node);
            } else if node != entry {
                // entry 的 immediate_dominator 是 None
                dom_tree.entry(entry).or_default().push(node);
            }
        }

        // 使用后序遍历支配树收集节点（子节点先于父节点）
        let mut postorder = Vec::new();

        fn dfs_dom_tree(
            node: NodeIndex,
            dom_tree: &BTreeMap<NodeIndex, Vec<NodeIndex>>,
            graph: &DiGraph<BlockId, ()>,
            postorder: &mut Vec<BlockId>,
        ) {
            // 先访问所有子节点
            if let Some(children) = dom_tree.get(&node) {
                for &child in children {
                    dfs_dom_tree(child, dom_tree, graph, postorder);
                }
            }
            // 后序：记录当前节点
            postorder.push(graph[node]);
        }

        dfs_dom_tree(entry, &dom_tree, &self.graph, &mut postorder);

        // 反转得到 RPO：父节点先于子节点
        postorder.reverse();

        let mut pos = 0;
        let mut block_pos_map = BTreeMap::new();
        for block_id in &postorder {
            block_pos_map.insert(*block_id, pos);
            pos += self
                .get_block(*block_id)
                .expect("block not found")
                .instructions()
                .len();
        }

        BlockLayout::new(postorder, block_pos_map)
    }

    pub fn loop_root_reverse_postorder_layout(&self) -> BlockLayout {
        let entry = self.entry.expect("no entry");
        let entry_node = self.block_node_map[&entry];

        // 1. 计算SCC
        let sccs = kosaraju_scc(&self.graph);

        // 2. 构建SCC图并找到循环SCC
        let mut scc_map = BTreeMap::new(); // 节点 -> SCC索引
        let mut scc_nodes = Vec::new(); // SCC索引 -> 节点集合

        for (scc_idx, nodes) in sccs.iter().enumerate() {
            for &node in nodes {
                scc_map.insert(node, scc_idx);
            }
            scc_nodes.push(nodes.clone());
        }

        // 3. 构建SCC之间的边
        let mut scc_graph = DiGraph::<usize, ()>::new();
        let mut scc_node_indices = Vec::new();

        // 为每个SCC创建节点
        for i in 0..scc_nodes.len() {
            scc_node_indices.push(scc_graph.add_node(i));
        }

        // 添加SCC之间的边
        for (from_scc, nodes) in scc_nodes.iter().enumerate() {
            for &node in nodes {
                for neighbor in self.graph.neighbors_directed(node, Direction::Outgoing) {
                    let to_scc = scc_map[&neighbor];
                    if from_scc != to_scc {
                        scc_graph.update_edge(
                            scc_node_indices[from_scc],
                            scc_node_indices[to_scc],
                            (),
                        );
                    }
                }
            }
        }

        // 4. 对SCC图进行拓扑排序
        let _scc_order = match toposort(&scc_graph, None) {
            Ok(order) => order,
            Err(_) => {
                // 如果有环（但SCC图应该无环），则使用任意顺序
                scc_node_indices.clone()
            }
        };

        // 5. 按照SCC拓扑顺序收集块
        let mut post_order = Vec::new();
        let mut visited = BTreeSet::new();

        // 按照SCC拓扑顺序处理（从入口所在SCC开始）
        let entry_scc = scc_map[&entry_node];
        let mut order_to_process = Vec::new();

        // 重新排序SCC，让包含入口的SCC及其可达的SCC优先
        let mut dfs_scc = DfsPostOrder::new(&scc_graph, scc_node_indices[entry_scc]);
        while let Some(scc_node) = dfs_scc.next(&scc_graph) {
            order_to_process.push(scc_graph[scc_node]);
        }
        order_to_process.reverse(); // 变为拓扑顺序

        // 添加其他未访问的SCC
        for i in 0..scc_nodes.len() {
            if !order_to_process.contains(&i) {
                order_to_process.push(i);
            }
        }

        // 6. 对每个SCC进行内部DFS后序遍历
        for scc_idx in order_to_process {
            let nodes = &scc_nodes[scc_idx];

            // 对于循环SCC（多个节点），找到合适的起点
            let start_node = if nodes.len() > 1 {
                // 对于循环SCC，选择具有最多前驱的节点作为"循环头"
                nodes
                    .iter()
                    .max_by_key(|&&node| {
                        self.graph
                            .neighbors_directed(node, Direction::Incoming)
                            .count()
                    })
                    .copied()
                    .unwrap_or(nodes[0])
            } else {
                nodes[0]
            };

            // 对SCC内部进行DFS后序
            let mut dfs = DfsPostOrder::new(&self.graph, start_node);
            while let Some(node) = dfs.next(&self.graph) {
                // 只添加当前SCC中的节点，且未访问过
                if scc_map[&node] == scc_idx && visited.insert(node) {
                    post_order.push(self.graph[node]);
                }
            }
        }

        // 7. 后序 -> 逆后序
        post_order.reverse();

        // 8. 计算指令偏移
        let mut pos_map = BTreeMap::new();
        let mut pos = 0;
        for &bid in &post_order {
            pos_map.insert(bid, pos);
            pos += self.get_block(bid).unwrap().instructions().len();
        }

        BlockLayout::new(post_order, pos_map)
    }

    pub fn loop_aware_reverse_postorder(&self) -> BlockLayout {
        let entry = self.entry.expect("no entry");
        let entry_node = self.block_node_map[&entry];

        // 标准的逆后序遍历
        let mut rpo = Vec::new();
        let mut visited = std::collections::HashSet::new();
        let mut stack = vec![(entry_node, false)];

        while let Some((node, processed)) = stack.pop() {
            if processed {
                rpo.push(node);
            } else if visited.insert(node) {
                // 记录访问顺序
                stack.push((node, true));

                // 获取后继节点
                let mut successors: Vec<NodeIndex> = self
                    .graph
                    .neighbors_directed(node, Direction::Outgoing)
                    .collect();

                // 按BlockId排序保证确定性
                successors.sort_by_key(|&n| self.graph[n]);

                // 逆序入栈，保证顺序遍历
                for succ in successors.into_iter().rev() {
                    if !visited.contains(&succ) {
                        stack.push((succ, false));
                    }
                }
            }
        }

        // 后序 -> 逆后序
        rpo.reverse();

        // 检测并重新排序循环
        let blocks = self.reorder_for_loops(rpo);

        // 计算指令偏移
        let mut pos_map = BTreeMap::new();
        let mut pos = 0;
        for &bid in &blocks {
            pos_map.insert(bid, pos);
            pos += self.get_block(bid).unwrap().instructions().len();
        }

        BlockLayout::new(blocks, pos_map)
    }

    /// 检测循环并重新排序
    fn reorder_for_loops(&self, rpo: Vec<NodeIndex>) -> Vec<BlockId> {
        let mut result = Vec::new();
        let mut processed = std::collections::HashSet::new();

        for &node in &rpo {
            if processed.contains(&node) {
                continue;
            }

            let block_id = self.graph[node];

            // 检查是否有后向边（循环）
            let mut worklist = vec![node];
            let mut loop_blocks = Vec::new();
            let mut loop_visited = std::collections::HashSet::new();

            while let Some(current) = worklist.pop() {
                if !loop_visited.insert(current) {
                    continue;
                }

                // 检查当前节点是否是循环头
                let mut is_loop_head = false;
                for pred in self.graph.neighbors_directed(current, Direction::Incoming) {
                    if rpo.iter().position(|&n| n == pred) > rpo.iter().position(|&n| n == current)
                    {
                        is_loop_head = true;
                        break;
                    }
                }

                if is_loop_head {
                    loop_blocks.push(current);

                    // 添加循环体节点
                    for succ in self.graph.neighbors_directed(current, Direction::Outgoing) {
                        if !loop_visited.contains(&succ) {
                            worklist.push(succ);
                        }
                    }
                }
            }

            if loop_blocks.len() > 1 {
                // 循环：排序循环体
                loop_blocks.sort_by_key(|&n| self.graph[n]);
                for &loop_node in &loop_blocks {
                    if processed.insert(loop_node) {
                        result.push(self.graph[loop_node]);
                    }
                }
            } else {
                // 非循环块
                if processed.insert(node) {
                    result.push(block_id);
                }
            }
        }

        result
    }

    pub fn loop_root_reverse_postorder_layout2(&self) -> BlockLayout {
        // 1. 获取 SCC 并反转为拓扑正序（source first）
        let mut sccs = kosaraju_scc(&self.graph);
        sccs.reverse();

        // 2. 构建 SCC 集合（用于快速查找）
        let scc_sets: Vec<HashSet<BlockId>> = sccs
            .into_iter()
            .map(|nodes| nodes.into_iter().map(|n| self.graph[n]).collect())
            .collect();

        // 3. 全局已访问块
        let mut visited = BTreeSet::new();
        let mut layout = Vec::new();

        // 4. 按拓扑正序处理每个 SCC
        for scc_set in &scc_sets {
            // 找到一个未访问的块作为起点（通常是 header）
            let Some(&first_blk) = scc_set.iter().next() else {
                continue;
            };

            if visited.contains(&first_blk) {
                continue;
            }

            if scc_set.len() == 1 {
                // 非循环块
                visited.insert(first_blk);
                layout.push(first_blk);
            } else {
                // 循环 SCC：选 header = min BlockId
                let mut header_candidates: Vec<BlockId> = scc_set.iter().cloned().collect();
                header_candidates.sort_unstable();
                let header_blk = header_candidates[0];
                let header_node = self.block_node_map[&header_blk];

                // 输出 header
                visited.insert(header_blk);
                layout.push(header_blk);

                // DFS 前序遍历其余块（仅限 SCC 内）
                self.dfs_preorder_scc(header_node, scc_set, &mut visited, &mut layout, header_blk);
            }
        }

        // 5. 构建偏移
        let mut pos_map = BTreeMap::new();
        let mut pos = 0;
        for &bid in &layout {
            pos_map.insert(bid, pos);
            pos += self.get_block(bid).unwrap().instructions().len();
        }

        BlockLayout::new(layout, pos_map)
    }

    // SCC 内部 DFS 前序（跳过 header）
    fn dfs_preorder_scc(
        &self,
        node: NodeIndex,
        scc_set: &HashSet<BlockId>,
        visited: &mut BTreeSet<BlockId>,
        layout: &mut Vec<BlockId>,
        header_blk: BlockId,
    ) {
        for neighbor in self.graph.neighbors_directed(node, Direction::Outgoing) {
            let blk_id = self.graph[neighbor];

            // 只处理同一 SCC 且未访问的块（跳过 header）
            if blk_id != header_blk && scc_set.contains(&blk_id) && visited.insert(blk_id) {
                layout.push(blk_id);
                self.dfs_preorder_scc(neighbor, scc_set, visited, layout, header_blk);
            }
        }
    }

    /// 简单版本的逆后序遍历（不考虑循环）
    pub fn simple_reverse_postorder(&self) -> Vec<BlockId> {
        let mut order = Vec::new();

        if let Some(entry_block) = self.entry
            && let Some(&entry_node) = self.block_node_map.get(&entry_block) {
                let mut dfs = DfsPostOrder::new(&self.graph, entry_node);

                while let Some(node) = dfs.next(&self.graph) {
                    order.push(self.graph[node]);
                }
            }

        order.reverse(); // DfsPostOrder得到的是后序，反转得到逆后序
        order
    }
    pub fn graph_node(&self, block: BlockId) -> NodeIndex {
        self.block_node_map[&block]
    }

    pub fn graph_node_indices(&self) -> Vec<NodeIndex> {
        self.block_node_map.values().cloned().collect()
    }

    pub fn graph_node_weight(&self, node: NodeIndex) -> BlockId {
        self.graph[node]
    }
}

impl Default for ControlFlowGraph {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for ControlFlowGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for block in self.blocks.iter() {
            write!(f, "{}", block)?
        }
        Ok(())
    }
}

pub struct BlockLayout {
    blocks: Vec<BlockId>,
    block_pos_map: BTreeMap<BlockId, usize>,
}

impl BlockLayout {
    pub fn new(blocks: Vec<BlockId>, block_pos_map: BTreeMap<BlockId, usize>) -> Self {
        Self {
            blocks,
            block_pos_map,
        }
    }

    pub fn blocks(&self) -> &[BlockId] {
        &self.blocks
    }

    pub fn get_block_pos(&self, block_id: BlockId) -> usize {
        *self.block_pos_map
            .get(&block_id)
            .expect("block not found")
    }

    pub fn iter<'a>(&self, cfg: &'a ControlFlowGraph) -> impl Iterator<Item = &'a Block> {
        self.blocks
            .iter()
            .map(|block| cfg.get_block(*block).expect("block not found"))
    }

    pub fn iter_rev<'a>(&self, cfg: &'a ControlFlowGraph) -> impl Iterator<Item = &'a Block> {
        self.blocks
            .iter()
            .rev()
            .map(|block| cfg.get_block(*block).expect("block not found"))
    }

    pub fn for_each_mut<F>(&self, cfg: &mut ControlFlowGraph, mut f: F)
    where
        F: FnMut(&mut Block),
    {
        for &id in &self.blocks {
            f(cfg.get_block_mut(id).expect("block not found"));
        }
    }
    pub fn debug(&self, cfg: &ControlFlowGraph) -> String {
        let mut output = String::new();
        for block in self.iter(cfg) {
            output.push_str(&format!("{}\n", block));
        }
        output
    }
}

pub trait DomExt {
    /// 返回 true 当且仅当 `descendant` 被 `ancestor` 严格支配
    fn is_dominated(&self, descendant: NodeIndex, ancestor: NodeIndex) -> bool;
}

impl DomExt for Dominators<NodeIndex> {
    fn is_dominated(&self, descendant: NodeIndex, ancestor: NodeIndex) -> bool {
        if descendant == ancestor {
            return false; // 严格支配：不能相等
        }
        let mut cur = descendant;
        while let Some(idom) = self.immediate_dominator(cur) {
            if idom == ancestor {
                return true;
            }
            cur = idom;
        }
        false
    }
}
