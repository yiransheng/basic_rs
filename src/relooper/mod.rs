use std::cmp::{Eq, PartialEq};
use std::collections::HashSet;
use std::hash::Hash;

use petgraph::graph::NodeIndex;
use petgraph::stable_graph::StableGraph;
use petgraph::Direction;

#[derive(Debug, Copy, Clone)]
enum FlowType {
    Direct, // We will directly reach the right location through other means, no need for continue or break
    Break,
    Continue,
}

struct Branch {
    ty: FlowType,
}

type Link<L> = Option<Box<Shape<L>>>;

// TODO: non-recursive Drop
#[derive(Debug)]
enum Shape<L> {
    Simple {
        internal: L,
        next: Link<L>,
    },
    Loop {
        inner: Box<Shape<L>>,
        next: Link<L>,
    },
    Multi {
        handled_shapes: Vec<Shape<L>>,
        next: Link<L>,
    },
}

impl<L> Shape<L> {
    pub fn get_next(&self) -> Option<&Shape<L>> {
        use std::ops::Deref;

        match self {
            Shape::Simple { next, .. } => next.as_ref().map(Deref::deref),
            Shape::Loop { next, .. } => next.as_ref().map(Deref::deref),
            Shape::Multi { next, .. } => next.as_ref().map(Deref::deref),
        }
    }
    // returns previous next shape if set
    pub fn set_next(&mut self, next: Shape<L>) -> Option<Shape<L>> {
        let link = self.next();

        ::std::mem::replace(link, Some(Box::new(next))).map(|boxed| *boxed)
    }

    fn next(&mut self) -> &mut Link<L> {
        match self {
            Shape::Simple { next, .. } => next,
            Shape::Loop { next, .. } => next,
            Shape::Multi { next, .. } => next,
        }
    }
}

// drops Drain iter after we get zero or one
// item out
macro_rules! pop_set {
    ($set: expr) => {{
        let mut iter = $set.drain();
        iter.next()
    }};
}

type NodeId = NodeIndex<u32>;

struct Relooper<L, E> {
    graph: StableGraph<L, E>,
}

impl<L, E> Relooper<L, E>
where
    L: Clone + ::std::fmt::Debug,
    E: ::std::fmt::Debug,
{
    fn add_block(&mut self, label: L) -> NodeId {
        self.graph.add_node(label)
    }
    fn add_branch(&mut self, a: NodeId, b: NodeId, data: E) {
        self.graph.add_edge(a, b, data);
    }

    fn process(
        &mut self,
        blocks: &mut HashSet<NodeId>,
        initial_entries: &mut HashSet<NodeId>,
    ) -> Option<Shape<L>> {
        let entries = initial_entries;

        let mut ret: Option<Shape<L>> = None;
        // borrow-chk wound around
        let mut has_ret = false;
        let mut prev: &mut Link<L> = &mut None;

        macro_rules! make {
            ($call: expr) => {{
                let shape = $call;
                if !has_ret {
                    has_ret = true;
                    ret = Some(shape);
                    prev = (&mut ret).as_mut().map(|s| s.next()).unwrap();
                } else {
                    *prev = Some(Box::new(shape));
                    prev = prev.as_mut().map(|s| s.next()).unwrap();
                }

                if entries.is_empty() {
                    return ret;
                } else {
                    continue;
                }
            }};
        }

        loop {
            if entries.len() == 0 {
                return None;
            }
            if entries.len() == 1 {
                let node_id = entries.iter().next().cloned().unwrap();
                println!("Node id: {:?}", node_id);

                if self
                    .graph
                    .neighbors_directed(node_id, Direction::Incoming)
                    .filter(|id| blocks.contains(&id))
                    .count()
                    == 0
                {
                    make!(self.make_simple(node_id, blocks, entries));
                } else {
                    make!(self.make_loop(blocks, entries));
                }
            }

            println!("Size: {:?}", entries);

            panic!("Multi block not supported yet");
        }
    }

    fn make_simple(
        &mut self,
        internal_id: NodeId,
        blocks: &mut HashSet<NodeId>,
        entries: &mut HashSet<NodeId>,
    ) -> Shape<L> {
        let internal = self.graph.node_weight(internal_id).cloned().unwrap();
        let shape = Shape::Simple {
            internal,
            next: None,
        };
        blocks.remove(&internal_id);

        let next_entries: HashSet<NodeId> = self
            .graph
            .neighbors_directed(internal_id, Direction::Outgoing)
            .filter(|id| blocks.contains(&id))
            .collect();

        // Solipsize
        for next_id in next_entries.iter().cloned() {
            let edge = self.graph.find_edge(internal_id, next_id).unwrap();
            self.graph.remove_edge(edge);
        }

        *entries = next_entries;

        shape
    }

    fn make_loop(
        &mut self,
        blocks: &mut HashSet<NodeId>,
        entries: &mut HashSet<NodeId>,
    ) -> Shape<L> {
        let mut inner_blocks: HashSet<NodeId> = HashSet::new();

        let mut queue = entries.clone();

        while let Some(curr_id) = pop_set!(queue) {
            if !inner_blocks.contains(&curr_id) {
                blocks.remove(&curr_id);
                inner_blocks.insert(curr_id);

                for prev in
                    self.graph.neighbors_directed(curr_id, Direction::Incoming)
                {
                    queue.insert(prev);
                }
            }
        }

        assert!(!inner_blocks.is_empty());

        let mut next_entries = HashSet::new();

        for curr_id in inner_blocks.iter().cloned() {
            for possible in
                self.graph.neighbors_directed(curr_id, Direction::Outgoing)
            {
                if !inner_blocks.contains(&possible) {
                    next_entries.insert(possible);
                }
            }
        }

        // Solipsize
        for entry in entries.iter().cloned() {
            let mut edges_to_remove = vec![];
            for node_id in
                self.graph.neighbors_directed(entry, Direction::Incoming)
            {
                if inner_blocks.contains(&node_id) {
                    let edge = self.graph.find_edge(node_id, entry).unwrap();
                    edges_to_remove.push(edge);
                }
            }

            for edge in edges_to_remove {
                self.graph.remove_edge(edge);
            }
        }

        println!("B/e: {:?} // {:?}", inner_blocks, entries);
        println!("G: {:?}", self.graph);

        let inner = self.process(&mut inner_blocks, entries).unwrap();

        println!("Recur Done: {:?}", inner);

        let loop_shape = Shape::Loop {
            inner: Box::new(inner),
            next: None,
        };

        *entries = next_entries;

        loop_shape
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_relooper() {
        let mut relooper: Relooper<i32, bool> = Relooper {
            graph: StableGraph::new(),
        };

        let a = relooper.add_block(0);
        let b = relooper.add_block(1);
        let c = relooper.add_block(2);
        let d = relooper.add_block(3);

        relooper.add_branch(a, b, true);
        relooper.add_branch(b, c, false);
        relooper.add_branch(b, d, true);
        relooper.add_branch(c, b, false);

        println!("{:?}", relooper.graph);

        let mut blocks = [a, b, c].iter().cloned().collect();
        let mut entries = [a].iter().cloned().collect();

        let shape = relooper.process(&mut blocks, &mut entries);

        println!("{:?}", shape);

        assert!(false);
    }
}
