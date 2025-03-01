use regex::Regex;
use std::{cmp::Ordering, collections::HashMap, fmt};

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

#[derive(Debug, Clone, PartialEq)]
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

#[test]
fn parse_subdivision() {
    let test_str = "T[63] := {{0,1,2},{0,1,3},{1,2,6},{1,3,7},{1,4,5},{1,4,7},{1,5,6},{3,7,9},{4,5,8},{4,7,8},{5,6,8},{7,8,9}};";

    let (subd, _) = Subdivision::new(test_str).unwrap();
    assert_eq!(subd, Subdivision::new_with_regex(test_str));
}

impl Subdivision {
    /// Given string of the form a,b,c parse as (a,b,c)
    /// Reject if not in this form
    fn parse_idxs(s: &str) -> Option<(i16, i16, i16)> {
        let split = s.split(",");

        if split.clone().count() != 3 {
            return None;
        }

        let idxs = split.map(|s| s.parse::<i16>().unwrap()).collect::<Vec<_>>();

        Some((idxs[0], idxs[1], idxs[2]))
    }

    pub fn new_from_polygons(polygons: Vec<Polygon>) -> Self {
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

    pub fn new(s: &str) -> Option<(Self, usize)> {
        let mut subdivision = Subdivision {
            polygon_map: HashMap::new(),
            polygons: vec![],
        };
        let mut subd_idx = 0;

        let mut cumul = String::new();
        let mut start_cumul = false;

        // because of the formatting of triangulations {{..}}; the last }; should be removed
        // this saves a conditional each time a '}' is seen
        for chr in s[..s.len() - 2].chars() {
            match chr {
                '[' => {
                    start_cumul = true;
                }
                ']' => {
                    subd_idx = cumul.parse::<usize>().unwrap();
                    start_cumul = false;
                    cumul = String::new();
                }
                '{' => {
                    start_cumul = true;
                }
                '}' => {
                    let (idx_one, idx_two, idx_three) = match Subdivision::parse_idxs(&cumul) {
                        Some((i, j, k)) => (i, j, k),
                        None => return None,
                    };

                    let tri = Polygon::from_edges(vec![
                        &Edge {
                            vertex_one: idx_one,
                            vertex_two: idx_two,
                        },
                        &Edge {
                            vertex_one: idx_two,
                            vertex_two: idx_three,
                        },
                        &Edge {
                            vertex_one: idx_one,
                            vertex_two: idx_three,
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
                    cumul = String::new();
                    start_cumul = false;
                }
                _ => {
                    if start_cumul {
                        cumul.push(chr);
                    }
                }
            }
        }

        Some((subdivision, subd_idx))
    }

    pub fn new_with_regex(s: &str) -> Self {
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

/// A Flip is defined as going from
/// subdivision_one -> subdivision_two
/// by replacing edge_one by edge_two in all polygons
#[derive(Debug, PartialEq)]
pub struct Flip {
    pub edge_one: Option<Edge>,
    pub subdivision_one_idx: usize,
    pub edge_two: Option<Edge>,
    pub subdivision_two_idx: usize,
}

#[test]
fn parse_flips() {
    let test_str_one = "flip[3820]:={78,78}; // to known triang, supported by [{1,8,9},{7}]";
    let test_str_two = "flip[3821]:={78,78}; // to known triang, supported by [{4},{1,8}]";
    let test_str_three = "flip[3697]:={78,78}; // supported by [{0,9},{3}]";
    let test_str_four = "flip[1786]:={40,55}; // to known triang, supported by [{3,8},{4,7}]";

    assert_eq!(Flip::new(test_str_one), Flip::new_with_regex(test_str_one));
    assert_eq!(Flip::new(test_str_two), Flip::new_with_regex(test_str_two));
    assert_eq!(
        Flip::new(test_str_three),
        Flip::new_with_regex(test_str_three)
    );
    assert_eq!(
        Flip::new(test_str_four),
        Flip::new_with_regex(test_str_four)
    );
}

impl Flip {
    /// Given string of the form a,b parse as (a,b)
    /// Reject if not in this form
    fn parse_idxs(s: &str) -> Option<(usize, usize)> {
        let split = s.split(",");

        if split.clone().count() != 2 {
            return None;
        }

        let idxs = split
            .map(|s| s.parse::<usize>().unwrap())
            .collect::<Vec<_>>();

        Some((idxs[0], idxs[1]))
    }

    pub fn new(s: &str) -> Option<Flip> {
        let mut flip = Flip {
            edge_one: None,
            edge_two: None,
            subdivision_one_idx: usize::MAX,
            subdivision_two_idx: usize::MAX,
        };

        let mut cumul = String::new();
        let mut start_cumul = false;
        let mut count = 0;
        for chr in s.chars() {
            match chr {
                '{' => start_cumul = true,
                '}' => {
                    let (idx_one, idx_two) = match Flip::parse_idxs(&cumul) {
                        Some((i, j)) => (i, j),
                        None => return None,
                    };

                    match count {
                        0 => {
                            flip.subdivision_one_idx = idx_two;
                            flip.subdivision_two_idx = idx_one;
                        }
                        1 => {
                            flip.edge_one = Some(Edge {
                                vertex_one: idx_one as i16,
                                vertex_two: idx_two as i16,
                            });
                        }
                        2 => {
                            flip.edge_two = Some(Edge {
                                vertex_one: idx_one as i16,
                                vertex_two: idx_two as i16,
                            });
                        }
                        _ => unreachable!("Should not be reachable because of if statement above."),
                    }
                    start_cumul = false;
                    count += 1;
                    cumul = String::new();
                }
                _ => {
                    if start_cumul {
                        cumul.push(chr);
                    }
                }
            }
        }

        Some(flip)
    }

    pub fn new_with_regex(s: &str) -> Option<Flip> {
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
