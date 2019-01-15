use matches::*;
use quickcheck::*;

use super::*;

impl Shape {
    fn find_child_shape(&self, shape_id: ShapeId) -> Option<&Shape> {
        if self.id == shape_id {
            return Some(self);
        }

        match &self.kind {
            ShapeKind::SimpleFused(_, MultiShape { handled_shapes, .. }) => {
                for s in handled_shapes.values() {
                    if let Some(s) = s.find_child_shape(shape_id) {
                        return Some(s);
                    }
                }
            }
            ShapeKind::Loop(LoopShape { inner }) => {
                if let Some(s) = inner.find_child_shape(shape_id) {
                    return Some(s);
                }
            }
            ShapeKind::Multi(MultiShape { handled_shapes, .. }) => {
                for s in handled_shapes.values() {
                    if let Some(s) = s.find_child_shape(shape_id) {
                        return Some(s);
                    }
                }
            }
            _ => {}
        }

        self.next
            .as_ref()
            .and_then(|s| s.find_child_shape(shape_id))
    }

    // Given a processed branch, such as:
    //   continue Shape$1 to reach Node$3
    //   break    Shape$2 to reach Node$4
    //   from Shape$4 diretly goto Node$5
    //   check whether this shape encodes this branching behavior
    fn contains_branch<E>(&self, branch: &ProcessedBranch<E>) -> bool {
        let shape = match self.find_child_shape(branch.ancestor) {
            Some(s) => s,
            _ => return false,
        };

        match branch.flow_type {
            FlowType::Direct => shape
                .next
                .as_ref()
                .map(|s| s.target_directly_reachable(branch.target))
                .unwrap_or(false),
            FlowType::Break => shape.target_reachable_by_break(branch.target),
            FlowType::Continue => {
                shape.target_reachable_by_continue(branch.target)
            }
        }
    }
}

// impl this for all shapes
//
// If a processed branch from a previous shape comes into this shape,
// can it reach its target?
trait TargetReachable {
    // if incoming branch is direct
    fn target_directly_reachable(&self, target: NodeId) -> bool;

    // if incoming branch is a break (shoud find its target in next)
    fn target_reachable_by_break(&self, target: NodeId) -> bool;

    // if incoming branch is a continue (shoud find its target in this shape,
    // given it is a loop)
    fn target_reachable_by_continue(&self, target: NodeId) -> bool;
}

impl TargetReachable for Shape {
    fn target_directly_reachable(&self, target: NodeId) -> bool {
        let reachable_here = match &self.kind {
            ShapeKind::Simple(s) => return s.target_directly_reachable(target),
            ShapeKind::SimpleFused(s, _) => {
                return s.target_directly_reachable(target)
            }
            ShapeKind::Loop(s) => return s.target_directly_reachable(target),
            // can fall through multi
            ShapeKind::Multi(s) => s.target_directly_reachable(target),
        };

        if !reachable_here {
            self.next
                .as_ref()
                .map(|s| s.target_directly_reachable(target))
                .unwrap_or(false)
        } else {
            true
        }
    }

    fn target_reachable_by_break(&self, target: NodeId) -> bool {
        // we can break a LoopShape or a MultiShape
        match &self.kind {
            ShapeKind::Simple(_) => return false,
            ShapeKind::SimpleFused(..) => return false,
            _ => {}
        }

        // after break, the target should be directly reachable in next
        self.next
            .as_ref()
            .map(|s| s.target_directly_reachable(target))
            .unwrap_or(false)
    }

    fn target_reachable_by_continue(&self, target: NodeId) -> bool {
        // can only continue a loop
        if let ShapeKind::Loop(ref s) = self.kind {
            s.target_reachable_by_continue(target)
        } else {
            false
        }
    }
}

impl TargetReachable for SimpleShape {
    fn target_directly_reachable(&self, target: NodeId) -> bool {
        // incoming branch reaches this shape only if we are its target
        self.internal == target
    }

    fn target_reachable_by_break(&self, target: NodeId) -> bool {
        false
    }

    fn target_reachable_by_continue(&self, target: NodeId) -> bool {
        false
    }
}

impl TargetReachable for LoopShape {
    fn target_directly_reachable(&self, target: NodeId) -> bool {
        self.inner.target_directly_reachable(target)
    }
    fn target_reachable_by_break(&self, target: NodeId) -> bool {
        false
    }
    fn target_reachable_by_continue(&self, target: NodeId) -> bool {
        self.inner.target_directly_reachable(target)
    }
}

impl TargetReachable for MultiShape {
    fn target_directly_reachable(&self, target: NodeId) -> bool {
        // reachable if any of the handled block matches target
        for (id, shape) in self.handled_shapes.iter() {
            if *id == target {
                return shape.target_directly_reachable(target);
            }
        }

        false
    }
    fn target_reachable_by_break(&self, target: NodeId) -> bool {
        false
    }

    fn target_reachable_by_continue(&self, target: NodeId) -> bool {
        false
    }
}

// This tests relooper structured CFG is identical to input unstructured
// CFG, the rendering logic of relooper is not tested here.
//
// If this test always passes and rendering logic is correct, we know for
// certain, relooper creates correct control flow
fn test_with_cfg(entry: i32, graph: &StableGraph<i32, i32>) -> bool {
    let mut relooper: Relooper<i32, i32> = Relooper::new();

    let mut nodes_mapping: HashMap<i32, NodeId> = HashMap::new();

    // copy input CFG into relooper
    for node in graph.node_indices() {
        let u = graph.node_weight(node).cloned().unwrap();

        let b_u = relooper.add_block(u);
        nodes_mapping.insert(u, b_u);
    }
    for edge_id in graph.edge_indices() {
        let cond = graph.edge_weight(edge_id).cloned().unwrap();

        let (u, v) = graph.edge_endpoints(edge_id).unwrap();
        let u = graph.node_weight(u).cloned().unwrap();
        let v = graph.node_weight(v).cloned().unwrap();
        let b_u = nodes_mapping.get(&u).cloned().unwrap();
        let b_v = nodes_mapping.get(&v).cloned().unwrap();

        relooper.add_branch(b_u, b_v, Some(cond));
    }

    // find entry NodeId
    let entry = nodes_mapping.get(&entry).cloned().unwrap();
    // relooper core logic, gets a resulting shape
    let shape = relooper.calculate(entry).unwrap();

    // for all branches in original CFG
    for edge_id in graph.edge_indices() {
        let (u, v) = graph.edge_endpoints(edge_id).unwrap();
        let u = graph.node_weight(u).cloned().unwrap();
        let v = graph.node_weight(v).cloned().unwrap();
        // to relooper node_id
        let b_u = nodes_mapping.get(&u).cloned().unwrap();
        let b_v = nodes_mapping.get(&v).cloned().unwrap();

        // relooper may remove unreachable node
        if !relooper.graph.contains_node(b_u) {
            continue;
        }
        // if source node u is not removed, target node v must
        // be reachable and thus should remain in relooper graph
        let branch = relooper.graph.find_edge(b_u, b_v).unwrap();
        let branch = relooper.graph.edge_weight(branch).unwrap();
        // relooper should have processed every edge
        let branch = match branch {
            Branch::Raw(_) => panic!("Branch not solipsized"),
            Branch::Processed(b) => b,
        };

        let orig_edge = graph.edge_weight(edge_id).cloned().unwrap();
        // check if the data attached to edge is unchanged (usually
        // the conditional expression, here it's a random i32)
        if branch.data != Some(orig_edge) {
            eprintln!("|> Branch condition/data differ");
            eprintln!("{:?}", shape);
            return false;
        }

        // Verify relooper models every single branch in source CFG
        if !shape.contains_branch(branch) {
            eprintln!("|> Branch not captured in shape");
            eprintln!("Shape: {:?}", shape);
            eprintln!("Branch: {:?}", branch);
            eprintln!();
            eprintln!();
            return false;
        }
    }

    true
}

#[derive(Debug, Clone)]
struct ArbitraryBranches {
    inner: HashSet<(i32, i32, i32)>,
}

impl Arbitrary for ArbitraryBranches {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        // limit of number of nodes
        const SIZE_LIMIT: i32 = 10;

        let from = i32::arbitrary(g) % SIZE_LIMIT;
        let to = i32::arbitrary(g) % SIZE_LIMIT;

        let weight = i32::arbitrary(g);
        let n_branches = usize::arbitrary(g) % 100 + 10;

        let mut inner = HashSet::new();

        for _ in 0..n_branches {
            inner.insert((from, to, weight));
        }

        ArbitraryBranches { inner }
    }
}

quickcheck! {
    fn cfg_equiv(branches: ArbitraryBranches) -> bool {
        let branches = branches.inner;

        if branches.is_empty() {
            return true;
        }
        if branches.len() > 1000 {
            return true;
        }

        // (from id, to id, weight/cond) tuple
        let mut graph = StableGraph::new();
        let mut nodes = HashMap::new();

        for (a, b, _) in branches.iter().cloned() {
            if nodes.get(&a).is_none() {
                let id = graph.add_node(a);
                nodes.insert(a, id);
            }
            if nodes.get(&b).is_none() {
                let id = graph.add_node(b);
                nodes.insert(b, id);
            }
        }

        for (a, b, w) in branches.iter() {
            let a_id = nodes.get(a).cloned().unwrap();
            let b_id = nodes.get(b).cloned().unwrap();

            graph.add_edge(a_id, b_id, *w);
        }

        let entry = nodes.keys().next().cloned().unwrap();

        // will panic on failed test
        test_with_cfg(entry, &graph)
    }
}
