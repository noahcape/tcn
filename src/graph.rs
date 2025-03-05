use petgraph;
use std::collections::{HashMap, HashSet};
use std::{fmt, vec};

use crate::subdivision::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Graph {
    adjacency_list: Vec<Vec<usize>>,
    row_counts: Vec<usize>,
    sprawling: Option<usize>,
}

impl fmt::Display for Graph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buffer = String::new();
        for (idx, adj_vertices) in self.adjacency_list.iter().enumerate() {
            for (adj_idx, is_adj) in adj_vertices.iter().enumerate() {
                for _ in 0..*is_adj {
                    fmt::write(&mut buffer, format_args!("({},{}) ", idx, adj_idx)).unwrap();
                }
            }
        }

        write!(f, "{buffer}")
    }
}

#[test]
fn test_subdivision_to_graph() {
    // 0:1:0:0
    // Skeleton: (4,7) (4,7) (4,7) (7,4) (7,4) (7,4)
    // Subdivision:
    // [(0, 1), (1, 2), (0, 2)] [(0, 2), (2, 3), (0, 3)] [(2, 3), (3, 5), (2, 5)] [(3, 5), (5, 6), (3, 6)] [(3, 6), (6, 7), (3, 7)]
    // [(3, 7), (7, 8), (3, 8)] [(5, 6), (6, 11), (5, 11)] [(6, 7), (7, 11), (6, 11)] [(7, 8), (8, 12), (7, 12)] [(7, 11), (11, 13), (7, 13)]
    // [(7, 12), (12, 13), (7, 13)] [(9, 10), (10, 13), (9, 13)] [(5, 10), (5, 11), (10, 13), (11, 13)] [(1, 2), (1, 4), (2, 5), (4, 5)] [(4, 5), (4, 9), (5, 10), (9, 10)]
    let polygons = vec![
        Polygon::Triangle(Edge::new(0, 1), Edge::new(1, 2), Edge::new(0, 2)),
        Polygon::Triangle(Edge::new(0, 2), Edge::new(2, 3), Edge::new(0, 3)),
        Polygon::Triangle(Edge::new(2, 3), Edge::new(3, 5), Edge::new(2, 5)),
        Polygon::Triangle(Edge::new(3, 5), Edge::new(5, 6), Edge::new(3, 6)),
        Polygon::Triangle(Edge::new(3, 6), Edge::new(6, 7), Edge::new(3, 7)),
        Polygon::Triangle(Edge::new(3, 7), Edge::new(7, 8), Edge::new(3, 8)),
        Polygon::Triangle(Edge::new(5, 6), Edge::new(6, 11), Edge::new(5, 11)),
        Polygon::Triangle(Edge::new(6, 7), Edge::new(7, 11), Edge::new(6, 11)),
        Polygon::Triangle(Edge::new(7, 8), Edge::new(8, 12), Edge::new(7, 12)),
        Polygon::Triangle(Edge::new(7, 11), Edge::new(11, 13), Edge::new(7, 13)),
        Polygon::Triangle(Edge::new(7, 12), Edge::new(12, 13), Edge::new(7, 13)),
        Polygon::Triangle(Edge::new(9, 10), Edge::new(10, 13), Edge::new(9, 13)),
        Polygon::Parallelogram(
            Edge::new(5, 10),
            Edge::new(5, 11),
            Edge::new(10, 13),
            Edge::new(11, 13),
        ),
        Polygon::Parallelogram(
            Edge::new(1, 2),
            Edge::new(1, 4),
            Edge::new(2, 5),
            Edge::new(4, 5),
        ),
        Polygon::Parallelogram(
            Edge::new(4, 5),
            Edge::new(4, 9),
            Edge::new(5, 10),
            Edge::new(9, 10),
        ),
    ];

    let subd = Subdivision::new_from_polygons(polygons);

    let next_tri = subd.next_triangle(
        Polygon::Parallelogram(
            Edge::new(1, 2),
            Edge::new(1, 4),
            Edge::new(2, 5),
            Edge::new(4, 5),
        ),
        Edge::new(1, 2),
    );

    assert_eq!(
        next_tri.unwrap(),
        Polygon::Triangle(Edge::new(9, 10), Edge::new(10, 13), Edge::new(9, 13))
    );

    let graph = Graph::from_subdivision(&subd);
    let mut skeleton_graph = graph.skeletonize(&subd);
    assert_eq!("0:0:2:0", skeleton_graph.categorizing_hash());
}

#[test]
fn test_remove_nodes() {
    let subdivision = Subdivision::new_from_polygons(vec![
        Polygon::Triangle(
            Edge {
                vertex_one: 0,
                vertex_two: 1,
            },
            Edge {
                vertex_one: 0,
                vertex_two: 2,
            },
            Edge {
                vertex_one: 1,
                vertex_two: 2,
            },
        ),
        Polygon::Triangle(
            Edge {
                vertex_one: 1,
                vertex_two: 2,
            },
            Edge {
                vertex_one: 1,
                vertex_two: 3,
            },
            Edge {
                vertex_one: 2,
                vertex_two: 3,
            },
        ),
        Polygon::Parallelogram(
            Edge {
                vertex_one: 2,
                vertex_two: 4,
            },
            Edge {
                vertex_one: 2,
                vertex_two: 3,
            },
            Edge {
                vertex_one: 3,
                vertex_two: 5,
            },
            Edge {
                vertex_one: 4,
                vertex_two: 5,
            },
        ),
        Polygon::Parallelogram(
            Edge {
                vertex_one: 4,
                vertex_two: 5,
            },
            Edge {
                vertex_one: 4,
                vertex_two: 6,
            },
            Edge {
                vertex_one: 5,
                vertex_two: 7,
            },
            Edge {
                vertex_one: 6,
                vertex_two: 7,
            },
        ),
    ]);

    // +-+-+-+
    // |\| | |
    // +-+-+-+
    let graph = Graph::from_subdivision(&subdivision);
    assert_eq!(graph.row_counts[0], 1);
    assert_eq!(graph.row_counts[1], 1);
}

struct DfsVecs<'a> {
    visited: &'a mut Vec<bool>,
    low: &'a mut Vec<i32>,
    time_in: &'a mut Vec<i32>,
}

impl Graph {
    fn edges(&self) -> impl Iterator<Item = (usize, usize)> {
        let mut edges = vec![];

        for (edge_idx, adj_list) in self.adjacency_list.iter().enumerate() {
            for (neighbor_idx, degree) in adj_list.iter().enumerate() {
                for _ in 0..*degree {
                    edges.push((edge_idx, neighbor_idx));
                }
            }
        }

        edges.into_iter()
    }

    pub fn new_skeleton(subdivision: &Subdivision) -> Self {
        let graph = Graph::from_subdivision(subdivision);
        graph.skeletonize(subdivision)
    }

    pub fn skeletonize(self, subd: &Subdivision) -> Self {
        self
            // .remove_nodes(subd)
            .remove_univalent()
            .smooth_over_bivalent(&subd.polygons)
    }

    pub fn categorizing_hash(&mut self) -> String {
        let sprawling = self.sprawling.unwrap_or(0);

        format!(
            "{}:{}:{}:{:?}",
            self.num_loops(),
            self.num_bridges(),
            self.num_bi_edges(),
            sprawling
        )
    }

    // build a graph from a subdivision - skipping over nodes
    pub fn from_subdivision(subdivision: &Subdivision) -> Self {
        let mut polygon_idx_map: HashMap<&Polygon, usize> = HashMap::new();
        for (idx, poly) in subdivision.polygons.iter().enumerate() {
            polygon_idx_map.insert(poly, idx);
        }

        let mut graph = Graph {
            adjacency_list: vec![vec![0; subdivision.polygons.len()]; subdivision.polygons.len()],
            row_counts: vec![0; subdivision.polygons.len()],
            sprawling: None,
        };

        for polygon in &subdivision.polygons {
            match polygon {
                Polygon::Triangle(..) => {
                    let idx = polygon_idx_map.get(&polygon).unwrap();
                    for edge in polygon.edge_itr() {
                        for adj_polygon in subdivision
                            .polygon_map
                            .get(edge)
                            .unwrap()
                            .iter()
                            .filter(|p| *p != polygon)
                        {
                            match adj_polygon {
                                Polygon::Parallelogram(..) => {
                                    match subdivision.next_triangle(*adj_polygon, *edge) {
                                        Some(next_polygon) => {
                                            if next_polygon != *polygon {
                                                let adj_idx =
                                                    polygon_idx_map.get(&next_polygon).unwrap();
                                                graph.row_counts[*idx] += 1;
                                                graph.adjacency_list[*idx][*adj_idx] = 1;
                                            }
                                        }
                                        None => continue,
                                    }
                                }
                                Polygon::Triangle(..) => {
                                    if adj_polygon != polygon {
                                        let adj_idx = polygon_idx_map.get(adj_polygon).unwrap();
                                        graph.row_counts[*idx] += 1;
                                        graph.adjacency_list[*idx][*adj_idx] = 1;
                                    }
                                }
                            };
                        }
                    }
                }
                Polygon::Parallelogram(..) => continue,
            }
        }

        graph
    }

    fn remove_univalent(mut self) -> Self {
        while let Some(idx) = self.row_counts.iter().position(|val| val == &1) {
            self.row_counts[idx] = 0;
            match self.adjacency_list[idx].iter().position(|val| val == &1) {
                Some(adj_idx) => {
                    self.adjacency_list[idx][adj_idx] = 0;
                    self.adjacency_list[adj_idx][idx] = 0;
                    self.row_counts[adj_idx] -= 1;
                }
                None => unreachable!("Error"),
            }
        }

        self
    }

    fn smooth_over_bivalent(mut self, polygons: &[Polygon]) -> Self {
        while let Some(idx) = self
            .row_counts
            .iter()
            .enumerate()
            .position(|(idx, val)| val == &2 && Polygon::is_triangle(&polygons[idx]))
        {
            self.row_counts[idx] = 0;

            let adjacent_polygons = self.adjacency_list[idx]
                .clone()
                .into_iter()
                .enumerate()
                .filter(|(_, val)| val >= &1)
                .collect::<Vec<_>>();

            for ((idx_a, deg_a), (idx_b, _)) in adjacent_polygons
                .clone()
                .into_iter()
                .zip(adjacent_polygons.into_iter().rev())
            {
                self.adjacency_list[idx_a][idx_b] += deg_a;
                self.adjacency_list[idx][idx_a] = 0;
                self.adjacency_list[idx_a][idx] = 0;
            }
        }

        self
    }

    fn num_loops(&self) -> usize {
        let mut num_loops = 0;
        for (idx, row) in self.adjacency_list.iter().enumerate() {
            (row[idx] == 2).then(|| num_loops += 1);
        }

        num_loops
    }

    fn num_bi_edges(&self) -> usize {
        let mut num_bi_edges = 0;
        for (idx, row) in self.adjacency_list.iter().enumerate() {
            for (adj_idx, deg) in row.iter().enumerate() {
                if idx != adj_idx {
                    (*deg == 2).then(|| num_bi_edges += 1);
                }
            }
        }

        num_bi_edges / 2
    }

    fn num_bridges(&mut self) -> usize {
        let mut timer = 0;
        let mut bridges: Vec<(usize, usize)> = vec![];

        let mut cur = 0;
        for (idx, deg) in self.row_counts.iter().enumerate() {
            if *deg != 0 {
                cur = idx;
                break;
            }
        }

        let mut dfs_vecs = DfsVecs {
            visited: &mut vec![false; self.row_counts.len()],
            low: &mut vec![0; self.row_counts.len()],
            time_in: &mut vec![0; self.row_counts.len()],
        };

        self.dfs_rec(&mut dfs_vecs, &mut timer, &mut bridges, cur, usize::MAX);

        let mut count = 0;
        let mut bridge_adj = vec![0; self.row_counts.len()];
        let dedup_bridges = bridges.iter().collect::<HashSet<_>>().into_iter();
        dedup_bridges.for_each(|(i, o)| {
            count += 1;
            bridge_adj[*i] += 1;
            bridge_adj[*o] += 1;
        });

        self.sprawling = Some(bridge_adj.into_iter().filter(|adj| *adj == 3).count());

        count
    }

    fn dfs_rec(
        &self,
        dfs_vecs: &mut DfsVecs,
        timer: &mut i32,
        bridges: &mut Vec<(usize, usize)>,
        cur: usize,
        parent: usize,
    ) {
        dfs_vecs.visited[cur] = true;
        dfs_vecs.time_in[cur] = *timer;
        dfs_vecs.low[cur] = *timer;
        *timer += 1;
        let mut parent_skipped = false;

        for (adj_node, deg) in self.adjacency_list[cur].iter().enumerate() {
            if adj_node == parent && !parent_skipped {
                parent_skipped = true;
                continue;
            }

            if *deg == 0 {
                continue;
            }

            if !dfs_vecs.visited[adj_node] {
                self.dfs_rec(dfs_vecs, timer, bridges, adj_node, cur);
                dfs_vecs.low[cur] = dfs_vecs.low[cur].min(dfs_vecs.low[adj_node]);

                if self.adjacency_list[adj_node][cur] == 2 {
                    continue;
                }

                if dfs_vecs.low[adj_node] > dfs_vecs.time_in[cur] {
                    if adj_node < cur {
                        bridges.push((adj_node, cur));
                    } else {
                        bridges.push((cur, adj_node));
                    }
                }
            } else {
                dfs_vecs.low[cur] = dfs_vecs.low[cur].min(dfs_vecs.time_in[adj_node]);
            }
        }
    }

    fn to_petgraph(&self) -> petgraph::Graph<i32, i32> {
        let mut node_map = HashMap::new();
        let nodes = self
            .row_counts
            .iter()
            .enumerate()
            .filter_map(|(idx, deg)| if *deg != 0 { Some(idx) } else { None })
            .collect::<Vec<_>>();

        let mut petgraph = petgraph::Graph::new();
        for node in nodes {
            node_map.insert(node, petgraph.add_node(0));
        }

        for (from, to) in self.edges() {
            let from_node = node_map.get(&from).unwrap();
            let to_node = node_map.get(&to).unwrap();

            petgraph.add_edge(*from_node, *to_node, 0);
        }

        petgraph
    }

    pub fn is_isomorphic(&self, other: &Graph) -> bool {
        use petgraph::algo;

        let this = self.to_petgraph();
        let other = other.to_petgraph();

        algo::is_isomorphic(&this, &other)
    }
}
