use std::collections::{HashMap, HashSet};
use std::fmt;
use std::ops::{Deref, DerefMut};

use petgraph::graph::NodeIndex;
use petgraph::stable_graph::StableGraph;
use petgraph::visit::Dfs;
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
            Shape::Multi {
                handled_shapes,
                next,
            } => {
                writeln!(f, "{}Multi [", "  ".repeat(indent))?;
                for s in handled_shapes {
                    s.fmt(indent + 1, f)?;
                }
                writeln!(f, "{}]", "  ".repeat(indent))?;
                if let Some(ref next) = next {
                    next.fmt(indent + 1, f)
                } else {
                    Ok(())
                }
            }
        }
    }
}

// prints Shape in a similar style to emscripten paper page 9
impl<L> fmt::Debug for Shape<L>
where
    L: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f)?;
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

        let inner = self
            .process(&mut inner_blocks, entries)
            .expect("Inner block empty for some reason");

        let loop_shape = Shape::Loop {
            inner: Box::new(inner),
            next: None,
        };

        loop_shape
    }

    // not efficient, get it correct first
    fn find_independent_groups(
        &self,
        blocks: &HashSet<NodeId>,
        entries: &HashSet<NodeId>,
    ) -> HashMap<NodeId, HashSet<NodeId>> {
        let mut indep_group: HashMap<NodeId, HashSet<NodeId>> = entries
            .iter()
            .cloned()
            .map(|entry| (entry, HashSet::new()))
            .collect();

        let mut reachable: HashMap<NodeId, Option<NodeId>> = HashMap::new();

        for entry in entries.iter().cloned() {
            let mut dfs = Dfs::new(&self.graph, entry);

            while let Some(node_id) = dfs.next(&self.graph) {
                if !blocks.contains(&node_id) {
                    continue;
                }
                match reachable.get(&node_id) {
                    Some(Some(e)) if *e == entry => {}
                    Some(Some(e)) if *e != entry => {
                        reachable.insert(node_id, None);
                    }
                    None => {
                        reachable.insert(node_id, Some(entry));
                    }
                    _ => {}
                }
            }
        }

        println!("INDEP: {:?}", reachable);

        for (k, v) in reachable.drain() {
            if let Some(entry) = v {
                indep_group.get_mut(&entry).map(|set| set.insert(k));
            }
        }

        indep_group
    }

    fn make_multiple<N: DerefMut<Target = HashSet<NodeId>>>(
        &mut self,
        blocks: &mut HashSet<NodeId>,
        entries: &mut HashSet<NodeId>,
        next_entries: &mut N,
        indep_groups: &mut HashMap<NodeId, HashSet<NodeId>>,
        // checked: bool, // TODO: enum
    ) -> Shape<L> {
        println!(" -> Make Multi");

        let mut handled_shapes = vec![];
        let mut edges = vec![];

        for (entry, targets) in indep_groups.iter_mut() {
            for inner_id in targets.iter().cloned() {
                edges.clear();
                blocks.remove(&inner_id);

                for node_id in self
                    .graph
                    .neighbors_directed(inner_id, Direction::Outgoing)
                    .filter(|id| blocks.contains(&id))
                {
                    if !targets.contains(&node_id) {
                        next_entries.insert(node_id);
                        // Solipsize
                        edges.push(
                            self.graph.find_edge(inner_id, node_id).unwrap(),
                        );
                    }
                }

                for edge in edges.drain(..) {
                    self.graph.remove_edge(edge);
                }
            }

            let mut inner_entries = HashSet::new();
            inner_entries.insert(*entry);

            let shape = self.process(targets, &mut inner_entries);
            if let Some(shape) = shape {
                handled_shapes.push(shape);
            }
        }

        Shape::Multi {
            handled_shapes,
            next: None,
        }
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

            println!("E/B {:?} <= {:?}", entries, blocks);

            if entries.len() == 0 {
                return None;
            }
            if entries.len() == 1 {
                let node_id = entries.iter().next().cloned().unwrap();

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

            let mut indep_groups =
                self.find_independent_groups(blocks, entries);

            println!("Indep set: {:?}", indep_groups);

            let indep_count =
                indep_groups.values().filter(|set| !set.is_empty()).count();

            if indep_count > 0 {
                make!(self.make_multiple(
                    blocks,
                    entries,
                    &mut next_entries,
                    &mut indep_groups
                ));
            } else {
                make!(self.make_loop(blocks, entries, &mut next_entries));
            }
        }
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
        relooper.add_branch(b, b, true);
        relooper.add_branch(a, c, false);
        relooper.add_branch(b, d, true);
        relooper.add_branch(c, d, false);

        let mut blocks = [a, b, c, d].iter().cloned().collect();
        let mut entries = [a].iter().cloned().collect();

        let shape = relooper.process(&mut blocks, &mut entries);

        println!("Found shape:");
        println!("{:?}", shape.unwrap());

        assert!(false);
    }
}
