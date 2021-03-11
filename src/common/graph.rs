use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Eq, PartialEq)]
pub struct Graph<NodeT>
where
    NodeT: Eq + PartialEq + Hash,
{
    pub edges: HashMap<NodeT, HashSet<NodeT>>,
}

impl<NodeT> Graph<NodeT>
where
    NodeT: Sized + Eq + Clone + Hash + PartialEq,
{
    pub fn new() -> Graph<NodeT> {
        Graph {
            edges: HashMap::new(),
        }
    }

    pub fn new_edge(&mut self, n: NodeT) {
        self.edges.insert(n, HashSet::new());
    }

    pub fn update_edge(&mut self, a: &NodeT, b: &NodeT) {
        if !self.edges.contains_key(&a) {
            self.new_edge(a.clone())
        };
        if !self.edges.contains_key(&b) {
            self.new_edge(b.clone())
        };
        self.edges.get_mut(&a).unwrap().insert(b.clone());
        self.edges.get_mut(&b).unwrap().insert(a.clone());
    }

    pub fn get_connections(&self, key: &NodeT) -> HashSet<&NodeT> {
        self.edges.get(key).unwrap().into_iter().collect()
    }

    pub fn from_edges(edges: &[(NodeT, NodeT)]) -> Graph<NodeT> {
        edges
            .into_iter()
            .fold(Graph::new(), |mut acc, (ele_a, ele_b)| {
                acc.update_edge(ele_a, ele_b);
                acc
            })
    }
}

#[cfg(test)]
mod test_graph {
    use super::*;

    #[derive(Eq, PartialEq, Hash, Clone, Debug)]
    enum TestEnum {
        Foo(i64),
        Bar(bool),
    }

    #[test]
    fn test_graph() {
        let mut g = Graph::new();

        // Allow insert
        g.update_edge(&5, &6);
        assert_eq!(g.get_connections(&5), set![&6]);
        // No duplicates
        g.update_edge(&5, &6);
        assert_eq!(g.get_connections(&5), set![&6]);

        // Muliple edges
        g.update_edge(&5, &7);
        assert_eq!(g.get_connections(&5), set![&6, &7]);

        // Connections go both ways
        assert_eq!(g.get_connections(&6), set![&5]);

        let f = Graph::from_edges(&[(5, 6), (6, 7)]);
        assert_eq!(f.get_connections(&6), set![&5, &7]);
    }

    #[test]
    fn test_graph_enum() {
        let mut g = Graph::new();

        g.update_edge(&TestEnum::Foo(5), &TestEnum::Foo(3));
        assert_eq!(
            g.get_connections(&TestEnum::Foo(5)),
            set![&TestEnum::Foo(3)]
        );

        g.update_edge(&TestEnum::Foo(5), &TestEnum::Bar(true));
        assert_eq!(
            g.get_connections(&TestEnum::Foo(5)),
            set![&TestEnum::Foo(3), &TestEnum::Bar(true)]
        );

        // No duplicates
        g.update_edge(&TestEnum::Foo(5), &TestEnum::Bar(true));
        assert_eq!(
            g.get_connections(&TestEnum::Foo(5)),
            set![&TestEnum::Foo(3), &TestEnum::Bar(true)]
        );
    }
}
