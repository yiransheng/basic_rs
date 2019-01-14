use std::cell::RefCell;
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
    pub ancestor: ShapeId,
    pub target: NodeId,
    pub flow_type: FlowType,
    pub data: Option<E>,
}

type Link = Option<Box<Shape>>;

pub type NodeId = NodeIndex<u32>;

pub struct Shape {
    id: ShapeId,
    kind: ShapeKind,
    next: Link,
}

// TODO: non-recursive Drop
enum ShapeKind {
    Simple(SimpleShape),
    SimpleFused(SimpleShape, MultiShape),
    Loop(LoopShape),
    Multi(MultiShape),
}

#[derive(Copy, Clone)]
pub struct SimpleShape {
    internal: NodeId,
}
pub struct LoopShape {
    inner: Box<Shape>,
}
pub struct MultiShape {
    handled_shapes: HashMap<NodeId, Shape>,
    needs_loop: usize,
}
impl Shape {
    fn fuse(&mut self) {
        match self.kind {
            ShapeKind::Simple(_) => self.fuse_simple(),
            ShapeKind::SimpleFused(_, ref mut multi) => {
                for (_, shape) in multi.handled_shapes.iter_mut() {
                    shape.fuse();
                }
            }
            ShapeKind::Multi(ref mut multi) => {
                for (_, shape) in multi.handled_shapes.iter_mut() {
                    shape.fuse();
                }
            }
            ShapeKind::Loop(ref mut loop_shape) => {
                loop_shape.inner.fuse();
            }
        }

        if let Some(ref mut next) = self.next {
            next.fuse();
        }
    }
    fn fuse_simple(&mut self) {
        let next = self.next.take();
        if let ShapeKind::Simple(SimpleShape { ref internal }) = self.kind {
            // next exists
            if let Some(next) = next {
                // and is multi
                if let ShapeKind::Multi(multi_shape) = next.kind {
                    self.kind = ShapeKind::SimpleFused(
                        SimpleShape {
                            internal: *internal,
                        },
                        multi_shape,
                    );
                    self.next = next.next;
                } else {
                    // put back the take
                    self.next = Some(next);
                }
            }
        }
    }
}

impl Shape {
    fn fmt(&self, indent: usize, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.kind {
            ShapeKind::Simple(SimpleShape { internal }) => {
                writeln!(f, "{}Simple({})", "  ".repeat(indent), self.id.0)?;
                writeln!(f, "{}{:?}", "  ".repeat(indent + 1), internal)?;
            }
            ShapeKind::SimpleFused(
                SimpleShape { internal },
                MultiShape { handled_shapes, .. },
            ) => {
                writeln!(
                    f,
                    "{}SimpleFused({}) [",
                    "  ".repeat(indent),
                    self.id.0
                )?;
                writeln!(f, "{}{:?}", "  ".repeat(indent + 1), internal)?;
                for s in handled_shapes.values() {
                    s.fmt(indent + 2, f)?;
                }
                writeln!(f, "{}]", "  ".repeat(indent + 1))?;
            }
            ShapeKind::Loop(LoopShape { inner }) => {
                writeln!(f, "{}Loop({})", "  ".repeat(indent), self.id.0)?;
                inner.fmt(indent + 1, f)?;
            }
            ShapeKind::Multi(MultiShape {
                handled_shapes,
                needs_loop,
            }) => {
                writeln!(
                    f,
                    "{}Multi({}) Loop({}) [",
                    "  ".repeat(indent),
                    self.id.0,
                    needs_loop,
                )?;
                for (target, s) in handled_shapes.iter() {
                    writeln!(
                        f,
                        "{}Entry: {:?}",
                        "  ".repeat(indent + 1),
                        target
                    )?;
                    s.fmt(indent + 2, f)?;
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

macro_rules! pop_set {
    ($set: expr) => {{
        let x = peek_set!($set);
        x.map(|x| {
            $set.remove(&x);
            x
        })
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
where
    L: ::std::fmt::Debug,
    E: ::std::fmt::Debug,
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
            kind: ShapeKind::Simple(SimpleShape {
                internal: internal_id,
            }),
            next: None,
        };
        blocks.remove(&internal_id);

        next_entries.extend(
            self.nodes_out(internal_id)
                .filter(|id| blocks.contains(&id)),
        );

        for next_id in next_entries.iter().cloned() {
            // is Direct wrong here?
            self.solipsize(next_id, FlowType::Direct, internal_id, shape.id);
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

        for b in inner_blocks.iter() {
            blocks.remove(b);
        }

        Shape {
            id: shape_id,
            kind: ShapeKind::Loop(LoopShape {
                inner: Box::new(inner),
            }),
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

        let mut reachable_by: HashMap<NodeId, Option<NodeId>> =
            entries.iter().cloned().map(|e| (e, (Some(e)))).collect();
        let mut visited = HashSet::new();
        let mut queue = HashSet::new();

        for entry in entries.iter().cloned() {
            visited.clear();
            queue.clear();

            queue.insert(entry);

            while let Some(v) = pop_set!(queue) {
                // ignore labels not in blocks
                if !blocks.contains(&v) {
                    continue;
                }
                visited.insert(v);

                match reachable_by.get(&v) {
                    Some(None) => {}
                    Some(Some(u)) if *u == entry => {}
                    Some(Some(u)) if *u != entry => {
                        reachable_by.insert(v, None);
                    }
                    None => {
                        reachable_by.insert(v, Some(entry));
                    }
                    _ => unreachable!(),
                }

                for u in self.nodes_out(v) {
                    if !visited.contains(&u) {
                        queue.insert(u);
                    }
                }
            }
        }

        for (k, v) in reachable_by.drain() {
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
            if targets.is_empty() {
                next_entries.insert(*entry);
                continue;
            }
            blocks.remove(&entry);
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
            kind: ShapeKind::Multi(MultiShape {
                handled_shapes,
                needs_loop,
            }),
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
            // println!("Entries: {:?}", entries);
            // println!("Blocks: {:?}", blocks);
            // println!();

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

            // println!("Indep g: {:?}", indep_groups);

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
    internal: NodeId,
    next: Option<&'a MultiShape>,
    relooper: RefCell<&'a mut Relooper<L, E>>,
}
impl<'a, L, E: Clone> SimpleBlock<'a, L, E> {
    fn render_branch<S>(
        &self,
        ctx: LoopCtx,
        branch: &ProcessedBranch<E>,
        sink: &mut S,
    ) where
        L: Render<S> + fmt::Debug,
        E: Render<S> + fmt::Debug,
        S: RenderSink,
    {
        match self.next {
            Some(MultiShape { handled_shapes, .. }) => {
                if let Some(shape) = handled_shapes.get(&branch.target) {
                    self.relooper.borrow_mut().render_shape(shape, ctx, sink);
                } else {
                    sink.render_branch(branch);
                }
            }
            None => sink.render_branch(branch),
        }
    }
}

impl<'a, L, E, S> Render<S> for SimpleBlock<'a, L, E>
where
    L: Render<S> + fmt::Debug,
    E: Render<S> + Clone + fmt::Debug,
    S: RenderSink,
{
    fn render(&self, ctx: LoopCtx, sink: &mut S) {
        let internal = self.internal;

        {
            self.relooper
                .borrow()
                .graph
                .node_weight(internal)
                .map(|raw| {
                    raw.render(ctx, sink);
                });
        }

        let mut default_branch: Option<ProcessedBranch<E>> = None;
        let mut branches: Vec<ProcessedBranch<E>> = vec![];

        {
            let relooper = self.relooper.borrow();
            for b in relooper.processed_branches_out(internal) {
                if b.data.is_none() {
                    assert!(
                        default_branch.is_none(),
                        "Can only have one default target"
                    );
                    default_branch = Some(b.clone());
                } else {
                    branches.push(b.clone());
                }
            }
        }

        if branches.is_empty() {
            // has default target
            if let Some(default_branch) = default_branch {
                if default_branch.flow_type != FlowType::Direct {
                    self.render_branch(ctx, &default_branch, sink);
                }
            }
            return;
        }

        for (i, b) in branches.drain(..).enumerate() {
            let cond = b.data.as_ref().unwrap();
            let cond = if i == 0 {
                Cond::If(cond)
            } else {
                Cond::ElseIf(cond)
            };
            sink.render_condition(ctx, cond, |sink| {
                self.render_branch(ctx, &b, sink);
            });
        }

        if let Some(default_branch) = default_branch {
            // branches not empty use "else" for default is ok
            sink.render_condition::<E, _>(ctx, Cond::Else, |sink| {
                self.render_branch(ctx, &default_branch, sink);
            });
        } else {
            sink.render_trap();
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
    IfLabel(NodeId),
    ElseIf(C),
    ElseIfLabel(NodeId),
    Else,
}

pub trait RenderSink {
    fn render_loop<F>(&mut self, shape_id: Option<ShapeId>, f: F)
    where
        F: FnMut(&mut Self);

    fn render_multi_loop<F>(&mut self, shape_id: Option<ShapeId>, f: F)
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

    fn render_trap(&mut self);

    fn render_branch<E: Render<Self>>(
        &mut self,
        br: &ProcessedBranch<E>,
        // set_label: bool, for now alawys set label
    ) where
        Self: Sized;
}

pub trait Render<S: RenderSink> {
    fn render(&self, ctx: LoopCtx, sink: &mut S);
}

impl<L, E> Relooper<L, E> {
    pub fn render<S>(&mut self, entry: NodeId, sink: &mut S)
    where
        L: Render<S> + fmt::Debug,
        E: Render<S> + Clone + fmt::Debug,
        S: RenderSink,
    {
        use petgraph::dot::{Config, Dot};

        let mut shape = self.calculate(entry).unwrap();
        // some multi uses loop, fuse cannot work with that yet
        // shape.fuse();

        // println!("{:?}", shape);

        // println!(
        // "{:?}",
        // Dot::with_config(&self.graph, &[Config::NodeIndexLabel])
        // );

        self.render_shape(&shape, LoopCtx::Outside, sink);
    }

    fn render_shape<S>(&mut self, shape: &Shape, ctx: LoopCtx, sink: &mut S)
    where
        L: Render<S> + fmt::Debug,
        E: Render<S> + Clone + fmt::Debug,
        S: RenderSink,
    {
        match &shape.kind {
            ShapeKind::Simple(SimpleShape { internal }) => {
                let block = SimpleBlock {
                    internal: *internal,
                    relooper: RefCell::new(self),
                    next: None,
                };
                block.render(ctx, sink);
            }
            ShapeKind::SimpleFused(SimpleShape { internal }, multi) => {
                let block = SimpleBlock {
                    internal: *internal,
                    relooper: RefCell::new(self),
                    next: Some(multi),
                };
                block.render(ctx, sink);
            }
            ShapeKind::Loop(LoopShape { inner }) => {
                let inner_shape = &*inner;

                sink.render_loop(Some(shape.id), |sink| {
                    self.render_shape(inner_shape, LoopCtx::InLoop, sink)
                })
            }
            ShapeKind::Multi(MultiShape {
                handled_shapes,
                needs_loop,
            }) => {
                let mut render_inner = |sink: &mut S| {
                    for (i, (entry, shape)) in handled_shapes.iter().enumerate()
                    {
                        let cond = if i == 0 {
                            Cond::IfLabel(*entry)
                        } else {
                            Cond::ElseIfLabel(*entry)
                        };
                        sink.render_condition::<E, _>(ctx, cond, |sink| {
                            self.render_shape(shape, ctx, sink);
                        });
                    }
                };
                if *needs_loop > 0 {
                    sink.render_multi_loop(Some(shape.id), render_inner);
                } else {
                    render_inner(sink);
                }
            }
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
        // let mut relooper: Relooper<&'static str, bool> = Relooper::new();

        // let a = relooper.add_block("a");
        // let b = relooper.add_block("b");
        // let c = relooper.add_block("c");
        // let d = relooper.add_block("d");
        // let e = relooper.add_block("e");

        // relooper.add_branch(a, b, None);
        // relooper.add_branch(b, c, None);
        // relooper.add_branch(b, d, None);
        // relooper.add_branch(c, b, None);

        // let shape = relooper.calculate(a);
        // let shape = shape.unwrap();

        // println!("Found shape:");
        // println!("{:?}", shape);

        // for b in relooper.processed_branches_in(c) {
        // println!("A: {:?}", b);
        // }

        assert!(true);
    }
}
