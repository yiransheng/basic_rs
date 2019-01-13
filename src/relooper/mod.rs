use std::collections::{HashMap, HashSet};
use std::fmt;
use std::ops::{Deref, DerefMut};

use petgraph::graph::NodeIndex;
use petgraph::stable_graph::StableGraph;
use petgraph::visit::Dfs;
use petgraph::Direction;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FlowType {
    Direct, // We will directly reach the right location through other means, no need for continue or break
    Break,
    Continue,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct ShapeId(pub u32);

#[derive(Debug, Clone)]
pub enum Branch<E> {
    // Option used only for take operation
    Raw(Option<E>),
    Processed(ProcessedBranch<E>),
}
#[derive(Debug, Clone)]
pub struct ProcessedBranch<E> {
    ancestor: ShapeId,
    target: NodeId,
    flow_type: FlowType,
    data: Option<E>,
}

type Link = Option<Box<Shape>>;

pub type NodeId = NodeIndex<u32>;

enum ShapeKind {
    Simple {
        internal: NodeId,
    },
    Loop {
        inner: Box<Shape>,
    },
    Multi {
        handled_shapes: HashMap<NodeId, Shape>,
        needs_loop: usize,
    },
}

// TODO: non-recursive Drop
pub struct Shape {
    id: ShapeId,
    kind: ShapeKind,
    next: Link,
}

impl Shape {
    fn fmt(&self, indent: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ShapeKind::Simple { internal } => {
                writeln!(f, "{}Simple({})", "  ".repeat(indent), self.id.0)?;
                writeln!(f, "{}{:?}", "  ".repeat(indent + 1), internal)?;
            }
            ShapeKind::Loop { inner } => {
                writeln!(f, "{}Loop({})", "  ".repeat(indent), self.id.0)?;
                inner.fmt(indent + 1, f)?;
            }
            ShapeKind::Multi { handled_shapes } => {
                writeln!(f, "{}Multi({}) [", "  ".repeat(indent), self.id.0)?;
                for s in handled_shapes.values() {
                    s.fmt(indent + 1, f)?;
                }
                writeln!(f, "{}]", "  ".repeat(indent))?;
            }
        }

        if let Some(ref next) = self.next {
            next.fmt(indent + 1, f)
        } else {
            Ok(())
        }
    }
}

// prints Shape in a similar style to emscripten paper page 9
impl fmt::Debug for Shape {
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
macro_rules! peek_set {
    ($set: expr) => {{
        let mut iter = $set.iter();
        iter.next().cloned()
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

pub struct Relooper<L, E> {
    shape_id_counter: u32,
    graph: StableGraph<L, Branch<E>>,
}

impl<L, E> Relooper<L, E>
// where
// L: ::std::fmt::Debug,
// E: ::std::fmt::Debug,
{
    pub fn new() -> Self {
        Relooper {
            shape_id_counter: 0,
            graph: StableGraph::new(),
        }
    }
    pub fn add_block(&mut self, label: L) -> NodeId {
        self.graph.add_node(label)
    }
    pub fn add_branch(&mut self, a: NodeId, b: NodeId, data: Option<E>) {
        self.graph.add_edge(a, b, Branch::Raw(data));
    }

    pub fn processed_branches_out<'a>(
        &'a self,
        node_id: NodeId,
    ) -> impl Iterator<Item = &'a ProcessedBranch<E>> + 'a {
        self.graph
            .neighbors_directed(node_id, Direction::Outgoing)
            .filter_map(move |id| {
                let edge = match self.graph.find_edge(node_id, id) {
                    Some(edge) => edge,
                    _ => return None,
                };
                let edge = self.graph.edge_weight(edge);
                if let Some(Branch::Processed(e)) = edge {
                    Some(e)
                } else {
                    None
                }
            })
    }

    pub fn processed_branches_in<'a>(
        &'a self,
        node_id: NodeId,
    ) -> impl Iterator<Item = &'a ProcessedBranch<E>> + 'a {
        self.graph
            .neighbors_directed(node_id, Direction::Incoming)
            .filter_map(move |id| {
                let edge = match self.graph.find_edge(id, node_id) {
                    Some(edge) => edge,
                    _ => return None,
                };
                let edge = self.graph.edge_weight(edge);
                if let Some(Branch::Processed(e)) = edge {
                    Some(e)
                } else {
                    None
                }
            })
    }

    fn next_shape_id(&mut self) -> ShapeId {
        let id = ShapeId(self.shape_id_counter);
        self.shape_id_counter += 1;
        id
    }

    fn solipsize(
        &mut self,
        target: NodeId,
        flow_type: FlowType,
        ancestor: NodeId,
        ancester_shape: ShapeId,
    ) -> Option<()> {
        let edge = self.graph.find_edge(ancestor, target)?;
        let branch = self.graph.edge_weight_mut(edge)?;

        if let Branch::Raw(data) = branch {
            let data = data.take();
            ::std::mem::replace(
                branch,
                Branch::Processed(ProcessedBranch {
                    ancestor: ancester_shape,
                    target,
                    flow_type,
                    data,
                }),
            );

            Some(())
        } else {
            None
        }
    }

    fn nodes_in<'a>(
        &'a self,
        node_id: NodeId,
    ) -> impl Iterator<Item = NodeId> + 'a {
        self.graph
            .neighbors_directed(node_id, Direction::Incoming)
            .filter(move |id| {
                let edge = match self.graph.find_edge(*id, node_id) {
                    Some(edge) => edge,
                    _ => return false,
                };
                if let Some(Branch::Raw(_)) = self.graph.edge_weight(edge) {
                    true
                } else {
                    false
                }
            })
    }

    fn nodes_out<'a>(
        &'a self,
        node_id: NodeId,
    ) -> impl Iterator<Item = NodeId> + 'a {
        self.graph
            .neighbors_directed(node_id, Direction::Outgoing)
            .filter(move |id| {
                let edge = match self.graph.find_edge(node_id, *id) {
                    Some(edge) => edge,
                    _ => return false,
                };
                if let Some(Branch::Raw(_)) = self.graph.edge_weight(edge) {
                    true
                } else {
                    false
                }
            })
    }

    fn make_simple<N: DerefMut<Target = HashSet<NodeId>>>(
        &mut self,
        internal_id: NodeId,
        blocks: &mut HashSet<NodeId>,
        entries: &mut HashSet<NodeId>,
        next_entries: &mut N,
    ) -> Shape {
        let shape = Shape {
            id: self.next_shape_id(),
            kind: ShapeKind::Simple {
                internal: internal_id,
            },
            next: None,
        };
        blocks.remove(&internal_id);

        next_entries.extend(
            self.nodes_out(internal_id)
                .filter(|id| blocks.contains(&id)),
        );

        for next_id in next_entries.iter().cloned() {
            self.solipsize(next_id, FlowType::Break, internal_id, shape.id);
        }

        shape
    }

    fn make_loop<N: DerefMut<Target = HashSet<NodeId>>>(
        &mut self,
        blocks: &mut HashSet<NodeId>,
        entries: &mut HashSet<NodeId>,
        next_entries: &mut N,
    ) -> Shape {
        let mut inner_blocks: HashSet<NodeId> = HashSet::new();

        let mut queue = entries.clone();

        while let Some(curr_id) = pop_set!(queue) {
            if !inner_blocks.contains(&curr_id) {
                blocks.remove(&curr_id);
                inner_blocks.insert(curr_id);

                for prev in self.nodes_in(curr_id) {
                    queue.insert(prev);
                }
            }
        }

        assert!(!inner_blocks.is_empty());

        for curr_id in inner_blocks.iter().cloned() {
            for possible in self.nodes_out(curr_id) {
                if !inner_blocks.contains(&possible) {
                    next_entries.insert(possible);
                }
            }
        }

        let shape_id = self.next_shape_id();

        for entry in entries.iter().cloned() {
            for from_id in inner_blocks.iter().cloned() {
                self.solipsize(entry, FlowType::Continue, from_id, shape_id);
            }
        }

        for entry in next_entries.iter().cloned() {
            for from_id in inner_blocks.iter().cloned() {
                self.solipsize(entry, FlowType::Break, from_id, shape_id);
            }
        }

        let inner = self
            .process(&mut inner_blocks, entries)
            .expect("Inner block empty for some reason");

        Shape {
            id: shape_id,
            kind: ShapeKind::Loop {
                inner: Box::new(inner),
            },
            next: None,
        }
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
        // checked: bool, // TODO
    ) -> Shape {
        let mut handled_shapes = HashMap::new();
        let mut next_targets = vec![];
        let mut needs_loop = 0;
        let shape_id = self.next_shape_id();

        for (entry, targets) in indep_groups.iter_mut() {
            for inner_id in targets.iter().cloned() {
                blocks.remove(&inner_id);

                next_targets.clear();
                next_targets.extend(
                    self.nodes_out(inner_id).filter(|id| blocks.contains(&id)),
                );

                for node_id in &next_targets {
                    if !targets.contains(node_id) {
                        next_entries.insert(*node_id);
                        self.solipsize(
                            *node_id,
                            FlowType::Break,
                            inner_id,
                            shape_id,
                        );
                        needs_loop += 1;
                    }
                }
            }

            let mut inner_entries = HashSet::new();
            inner_entries.insert(*entry);

            let shape = self.process(targets, &mut inner_entries);
            if let Some(shape) = shape {
                handled_shapes.insert(*entry, shape);
            }
        }

        Shape {
            id: shape_id,
            kind: ShapeKind::Multi {
                handled_shapes,
                needs_loop,
            },
            next: None,
        }
    }

    pub fn calculate(&mut self, entry: NodeId) -> Option<Shape> {
        self.remove_dead(entry);

        let mut initial_entries = HashSet::new();
        initial_entries.insert(entry);

        self.process(
            &mut self.graph.node_indices().collect(),
            &mut initial_entries,
        )
    }

    fn remove_dead(&mut self, entry: NodeId) {
        let mut dead: HashSet<NodeId> = self.graph.node_indices().collect();
        let mut dfs = Dfs::new(&self.graph, entry);

        while let Some(node_id) = dfs.next(&self.graph) {
            dead.remove(&node_id);
        }

        for node_id in dead.drain() {
            self.graph.remove_node(node_id);
        }
    }

    fn process(
        &mut self,
        blocks: &mut HashSet<NodeId>,
        initial_entries: &mut HashSet<NodeId>,
    ) -> Option<Shape> {
        let entries = initial_entries;
        let mut next_entries: DblBuffer<HashSet<NodeId>> = DblBuffer::new();

        let mut ret: Option<Shape> = None;
        // borrow-chk work around
        let mut has_ret = false;
        let mut prev: &mut Link = &mut None;

        macro_rules! make {
            ($call: expr) => {{
                let shape = $call;

                if !has_ret {
                    has_ret = true;
                    ret = Some(shape);
                    prev = (&mut ret).as_mut().map(|s| &mut s.next).unwrap();
                } else {
                    *prev = Some(Box::new(shape));
                    prev = prev.as_mut().map(|s| &mut s.next).unwrap();
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
                let node_id = peek_set!(entries).unwrap();

                if self
                    .nodes_in(node_id)
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

struct SimpleBlock<'a, L, E> {
    shape: &'a Shape,
    relooper: &'a Relooper<L, E>,
}
impl<'a, L, E, S> Render<S> for SimpleBlock<'a, L, E>
where
    L: Render<S>,
    E: Render<S>,
    S: RenderSink,
{
    fn render(&self, ctx: LoopCtx, sink: &mut S) {
        let internal = match self.shape.kind {
            ShapeKind::Simple { internal } => internal,
            _ => return,
        };

        self.relooper.graph.node_weight(internal).map(|raw| {
            raw.render(ctx, sink);
        });

        let mut default_target: Option<NodeId> = None;
        let mut branches: Vec<&ProcessedBranch<E>> = vec![];

        for b in self.relooper.processed_branches_out(internal) {
            if b.data.is_none() {
                assert!(
                    default_target.is_none(),
                    "Can only have one default target"
                );
                default_target = Some(b.target);
            } else {
                branches.push(b);
            }
        }

        if default_target.is_none() && branches.is_empty() {
            return;
        }

        let default_target = default_target.expect("Missing default target");

        for (i, b) in branches.drain(..).enumerate() {
            let has_content = b.flow_type != FlowType::Direct;
            let cond = b.data.as_ref().unwrap();
            if i == 0 {
                sink.render_condition(ctx, Cond::If(cond), |sink| {
                    sink.render_flow(ctx, b.flow_type, None);
                });
            } else {
                sink.render_condition(ctx, Cond::ElseIf(cond), |sink| {
                    sink.render_flow(ctx, b.flow_type, None);
                });
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum LoopCtx {
    InLoop,
    Outside,
}

#[derive(Debug)]
pub enum Cond<C> {
    If(C),
    ElseIf(C),
    Else,
}

pub trait RenderSink {
    fn render_loop<F>(&mut self, f: F)
    where
        F: FnMut(&mut Self);

    fn render_multi_loop<F>(&mut self, f: F)
    where
        F: FnMut(&mut Self);

    fn render_condition<C: Render<Self>, F>(
        &mut self,
        ctx: LoopCtx,
        cond: Cond<&C>,
        f: F,
    ) where
        F: FnMut(&mut Self),
        Self: Sized;

    fn render_shape_id(&mut self, shape_id: ShapeId);

    fn render_flow(
        &mut self,
        cfx: LoopCtx,
        flow_type: FlowType,
        shape_id: Option<ShapeId>,
    );
}

pub trait Render<S: RenderSink> {
    fn render(&self, ctx: LoopCtx, sink: &mut S);
}

impl<L, E> Relooper<L, E> {
    pub fn render<S>(&mut self, entry: NodeId, sink: &mut S)
    where
        L: Render<S> + fmt::Debug,
        E: Render<S>,
        S: RenderSink,
    {
        let shape = self.calculate(entry).unwrap();

        println!("{:?}", shape);

        self.render_shape(&shape, LoopCtx::Outside, sink);
    }

    fn render_shape<S>(&mut self, shape: &Shape, ctx: LoopCtx, sink: &mut S)
    where
        L: Render<S> + fmt::Debug,
        E: Render<S>,
        S: RenderSink,
    {
        match &shape.kind {
            ShapeKind::Simple { internal } => {
                let block = SimpleBlock {
                    shape,
                    relooper: &*self,
                };
                block.render(ctx, sink);
            }
            ShapeKind::Loop { inner } => {
                let shape = &*inner;

                sink.render_loop(|sink| {
                    self.render_shape(shape, LoopCtx::InLoop, sink)
                })
            }
            ShapeKind::Multi {
                handled_shapes,
                needs_loop,
            } => {}
        }

        if let Some(ref next) = shape.next {
            self.render_shape(next, ctx, sink);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_relooper() {
        let mut relooper: Relooper<&'static str, bool> = Relooper::new();

        let a = relooper.add_block("a");
        let b = relooper.add_block("b");
        let c = relooper.add_block("c");
        let d = relooper.add_block("d");
        let e = relooper.add_block("e");

        relooper.add_branch(a, b, None);
        relooper.add_branch(b, c, None);
        relooper.add_branch(b, d, None);
        relooper.add_branch(c, b, None);

        let shape = relooper.calculate(a);
        let shape = shape.unwrap();

        println!("Found shape:");
        println!("{:?}", shape);

        for b in relooper.processed_branches_in(c) {
            println!("A: {:?}", b);
        }

        assert!(true);
    }
}
