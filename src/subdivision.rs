use std::{cmp::Ordering, collections::HashMap, fmt};

use regex::Regex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Polygon {
    Triangle(Edge, Edge, Edge),
    Parallelogram(Edge, Edge, Edge, Edge),
}

impl fmt::Display for Polygon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Polygon::Triangle(edge, edge1, edge2) => write!(f, "[{}, {}, {}]", edge, edge1, edge2),
            Polygon::Parallelogram(edge, edge1, edge2, edge3) => {
                write!(f, "[{}, {}, {}, {}]", edge, edge1, edge2, edge3)
            }
        }
    }
}

#[test]
fn test_flip_edge() {
    let triangle_a = Polygon::Triangle(
        Edge {
            vertex_one: 0,
            vertex_two: 1,
        },
        Edge {
            vertex_one: 1,
            vertex_two: 2,
        },
        Edge {
            vertex_one: 0,
            vertex_two: 2,
        },
    );
    let triangle_b = Polygon::Triangle(
        Edge {
            vertex_one: 0,
            vertex_two: 2,
        },
        Edge {
            vertex_one: 2,
            vertex_two: 3,
        },
        Edge {
            vertex_one: 0,
            vertex_two: 3,
        },
    );

    let parrallelogram = Polygon::flip_edge(
        triangle_a,
        triangle_b,
        Edge {
            vertex_one: 0,
            vertex_two: 2,
        },
    )
    .unwrap();
    assert_eq!(
        parrallelogram,
        Polygon::Parallelogram(
            Edge {
                vertex_one: 0,
                vertex_two: 1
            },
            Edge {
                vertex_one: 1,
                vertex_two: 2
            },
            Edge {
                vertex_one: 2,
                vertex_two: 3
            },
            Edge {
                vertex_one: 0,
                vertex_two: 3
            }
        )
    )
}

impl Polygon {
    pub fn from_edges(mut edges: Vec<&Edge>) -> Option<Self> {
        edges.sort_by(|a, b| a.compare_to(b));

        match edges.len() {
            3 => Some(Polygon::Triangle(*edges[0], *edges[1], *edges[2])),
            4 => Some(Polygon::Parallelogram(
                *edges[0], *edges[1], *edges[2], *edges[3],
            )),
            _ => None,
        }
    }

    pub fn edge_itr(&self) -> impl Iterator<Item = &'_ Edge> {
        match self {
            Polygon::Triangle(edge, edge1, edge2) => vec![edge, edge1, edge2].into_iter(),
            Polygon::Parallelogram(edge, edge1, edge2, edge3) => {
                vec![edge, edge1, edge2, edge3].into_iter()
            }
        }
    }

    pub fn contains_edge(&self, edge: &Edge) -> bool {
        self.edge_itr().any(|e| e == edge)
    }

    pub fn flip_edge(polya: Polygon, polyb: Polygon, edge: Edge) -> Option<Polygon> {
        if !polya.contains_edge(&edge) && !polyb.contains_edge(&edge) {
            return None;
        }

        match (polya, polyb) {
            (Polygon::Triangle(..), Polygon::Triangle(..)) => Polygon::from_edges(
                polya
                    .edge_itr()
                    .chain(polyb.edge_itr())
                    .filter(|e| **e != edge)
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        }
    }

    pub fn is_triangle(&self) -> bool {
        match self {
            Polygon::Parallelogram(..) => false,
            Polygon::Triangle(..) => true,
        }
    }

    /// Returns two iterators each containing edges
    /// with disjoint vertices
    pub fn get_disjoint_edges(&self) -> Option<((Edge, Edge), (Edge, Edge))> {
        match self {
            Polygon::Parallelogram(edge1, edge2, edge3, edge4) => {
                let mut l1 = vec![];
                let mut l2 = vec![];
                if edge1.shares_vertex_with(edge2) {
                    l1.push(edge1);
                    l2.push(edge2);
                    if edge3.shares_vertex_with(edge1) {
                        l1.push(edge4);
                        l2.push(edge3);
                    } else {
                        l1.push(edge3);
                        l2.push(edge4);
                    }
                } else {
                    l1.push(edge1);
                    l1.push(edge2);

                    l2.push(edge3);
                    l2.push(edge4);
                }
                Some(((*l1[0], *l1[1]), (*l2[0], *l2[1])))
            }
            Polygon::Triangle(..) => None,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Edge {
    pub vertex_one: i16,
    pub vertex_two: i16,
}

impl fmt::Display for Edge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.vertex_one, self.vertex_two)
    }
}

impl Edge {
    fn compare_to(&self, other: &Edge) -> Ordering {
        self.vertex_two.cmp(&other.vertex_two)
    }

    fn shares_vertex_with(&self, other: &Edge) -> bool {
        self.vertex_one == other.vertex_one
            || self.vertex_one == other.vertex_two
            || self.vertex_two == other.vertex_one
            || self.vertex_two == other.vertex_two
    }
}

#[derive(Debug)]
pub struct Subdivision {
    pub polygons: Vec<Polygon>,
    pub polygon_map: HashMap<Edge, Vec<Polygon>>,
}

impl fmt::Display for Subdivision {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buffer = String::new();
        for polygon in &self.polygons {
            fmt::write(&mut buffer, format_args!("{} ", polygon)).unwrap();
        }

        write!(f, "{buffer}")
    }
}

/// A Flip is defined as going from
/// subdivision_one -> subdivision_two
/// by replacing edge_one by edge_two in all polygons
#[derive(Debug)]
pub struct Flip {
    pub edge_one: Option<Edge>,
    pub subdivision_one_idx: usize,
    pub edge_two: Option<Edge>,
    pub subdivision_two_idx: usize,
}

impl Subdivision {
    pub fn new(polygons: Vec<Polygon>) -> Self {
        let mut subdivision = Subdivision {
            polygon_map: HashMap::new(),
            polygons: vec![],
        };

        for polygon in polygons {
            for edge in polygon.edge_itr() {
                match subdivision.polygon_map.get_mut(edge) {
                    Some(list) => list.push(polygon),
                    None => {
                        subdivision.polygon_map.insert(*edge, vec![polygon]);
                    }
                }
            }

            subdivision.polygons.push(polygon);
        }

        subdivision
    }

    pub fn from_formatted_str(s: &str) -> Subdivision {
        let mut subdivision = Subdivision {
            polygon_map: HashMap::new(),
            polygons: vec![],
        };

        let re = Regex::new(r"\{\d+,\d+,\d+\}").unwrap();

        for mat in re.find_iter(s) {
            let verticies = s
                .get(mat.range())
                .unwrap()
                .split(",")
                .map(|s| {
                    s.trim_matches(|c| c == '}' || c == '{')
                        .parse::<i16>()
                        .unwrap()
                })
                .collect::<Vec<_>>();

            let tri = Polygon::from_edges(vec![
                &Edge {
                    vertex_one: verticies[0],
                    vertex_two: verticies[1],
                },
                &Edge {
                    vertex_one: verticies[1],
                    vertex_two: verticies[2],
                },
                &Edge {
                    vertex_one: verticies[0],
                    vertex_two: verticies[2],
                },
            ])
            .unwrap();

            for edge in tri.edge_itr() {
                match subdivision.polygon_map.get_mut(edge) {
                    Some(list) => list.push(tri),
                    None => {
                        subdivision.polygon_map.insert(*edge, vec![tri]);
                    }
                }
            }

            subdivision.polygons.push(tri);
        }

        subdivision
    }
}

impl Flip {
    pub fn new(s: &str) -> Option<Flip> {
        let mut flip = Flip {
            edge_one: None,
            edge_two: None,
            subdivision_one_idx: usize::MAX,
            subdivision_two_idx: usize::MAX,
        };

        let re = Regex::new(r"\{\d+,\d+\}").unwrap();

        let matches = re.find_iter(s).collect::<Vec<_>>();

        if matches.len() != 3 {
            return None;
        } else {
            for (idx, mat) in matches.iter().enumerate() {
                match idx {
                    0 => {
                        let idxs = s
                            .get(mat.range())
                            .unwrap()
                            .split(",")
                            .map(|s| {
                                s.trim_matches(|c| c == '}' || c == '{')
                                    .parse::<usize>()
                                    .unwrap()
                            })
                            .collect::<Vec<_>>();
                        flip.subdivision_one_idx = idxs[1];
                        flip.subdivision_two_idx = idxs[0];
                    }
                    1 => {
                        let edge = s
                            .get(mat.range())
                            .unwrap()
                            .split(",")
                            .map(|s| {
                                s.trim_matches(|c| c == '}' || c == '{')
                                    .parse::<i16>()
                                    .unwrap()
                            })
                            .collect::<Vec<_>>();
                        flip.edge_one = Some(Edge {
                            vertex_one: edge[0],
                            vertex_two: edge[1],
                        });
                    }
                    2 => {
                        let edge = s
                            .get(mat.range())
                            .unwrap()
                            .split(",")
                            .map(|s| {
                                s.trim_matches(|c| c == '}' || c == '{')
                                    .parse::<i16>()
                                    .unwrap()
                            })
                            .collect::<Vec<_>>();
                        flip.edge_two = Some(Edge {
                            vertex_one: edge[0],
                            vertex_two: edge[1],
                        });
                    }
                    _ => unreachable!("Should not be reachable because of if statement above."),
                }
            }
        }

        Some(flip)
    }
}
