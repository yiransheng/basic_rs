use std::cmp::{Eq, PartialEq};
use std::collections::HashSet;
use std::fmt;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

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

impl<L> Shape<L>
where
    L: fmt::Debug,
{
    fn fmt(&self, indent: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Shape::Simple { internal, next } => {
                writeln!(f, "{}Simple", "  ".repeat(indent))?;
                writeln!(f, "{}{:?}", "  ".repeat(indent + 1), internal)?;
                if let Some(ref next) = next {
                    next.fmt(indent + 1, f)
                } else {
                    Ok(())
                }
            }
            Shape::Loop { inner, next } => {
                writeln!(f, "{}Loop", "  ".repeat(indent))?;
                inner.fmt(indent + 1, f)?;
                if let Some(ref next) = next {
                    next.fmt(indent + 1, f)
                } else {
                    Ok(())
                }
            }
            Shape::Multi { .. } => Ok(()),
        }
    }
}

// prints Shape in a similar style to emscripten paper page 9
impl<L> fmt::Debug for Shape<L>
where
    L: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt(0, f)
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

struct DblBuffer<T> {
    swapped: bool,
    first: T,
    second: T,
}
impl<T> DblBuffer<T>
where
    T: Default,
{
    fn new() -> Self {
        DblBuffer {
            swapped: false,
            first: T::default(),
            second: T::default(),
        }
    }
    fn swap(&mut self) {
        self.swapped = !self.swapped;
    }
}
impl<T> Deref for DblBuffer<T> {
    type Target = T;

    fn deref(&self) -> &T {
        if self.swapped {
            &self.first
        } else {
            &self.second
        }
    }
}
impl<T> DerefMut for DblBuffer<T> {
    fn deref_mut(&mut self) -> &mut T {
        if self.swapped {
            &mut self.first
        } else {
            &mut self.second
        }
    }
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
        let mut next_entries: DblBuffer<HashSet<NodeId>> = DblBuffer::new();

        let mut ret: Option<Shape<L>> = None;
        // borrow-chk work around
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

                if next_entries.is_empty() {
                    return ret;
                } else {
                    ::std::mem::swap(entries, next_entries.deref_mut());
                    continue;
                }
            }};
        }

        loop {
            next_entries.swap();
            next_entries.clear();

            if entries.len() == 0 {
                return None;
            }
            if entries.len() == 1 {
                let node_id = entries.iter().next().cloned().unwrap();
                println!("Node id: {:?}", node_id);
                println!("Entries {:?}", entries);

                if self
                    .graph
                    .neighbors_directed(node_id, Direction::Incoming)
                    .filter(|id| blocks.contains(&id))
                    .count()
                    == 0
                {
                    make!(self.make_simple(
                        node_id,
                        blocks,
                        entries,
                        &mut next_entries
                    ));
                } else {
                    make!(self.make_loop(blocks, entries, &mut next_entries));
                }
            }

            println!("Size: {:?}", entries);

            panic!("Multi block not supported yet");
        }
    }

    fn make_simple<N: DerefMut<Target = HashSet<NodeId>>>(
        &mut self,
        internal_id: NodeId,
        blocks: &mut HashSet<NodeId>,
        entries: &mut HashSet<NodeId>,
        next_entries: &mut N,
    ) -> Shape<L> {
        println!(" -> Make Simple");
        let internal = self.graph.node_weight(internal_id).cloned().unwrap();
        let shape = Shape::Simple {
            internal,
            next: None,
        };
        blocks.remove(&internal_id);

        next_entries.extend(
            self.graph
                .neighbors_directed(internal_id, Direction::Outgoing)
                .filter(|id| blocks.contains(&id)),
        );

        // Solipsize
        for next_id in next_entries.iter().cloned() {
            let edge = self.graph.find_edge(internal_id, next_id).unwrap();
            self.graph.remove_edge(edge);
        }

        shape
    }

    fn make_loop<N: DerefMut<Target = HashSet<NodeId>>>(
        &mut self,
        blocks: &mut HashSet<NodeId>,
        entries: &mut HashSet<NodeId>,
        next_entries: &mut N,
    ) -> Shape<L> {
        println!(" -> Make Loop");

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

        let inner = self
            .process(&mut inner_blocks, entries)
            .expect("Inner block empty for some reason");

        println!("Recur Done: {:?}", inner);

        let loop_shape = Shape::Loop {
            inner: Box::new(inner),
            next: None,
        };

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

        println!("{:?}", shape.unwrap());

        assert!(false);
    }
}
