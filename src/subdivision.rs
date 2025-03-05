use regex::Regex;
use std::{cmp::Ordering, collections::HashMap, fmt};

use crate::polygon::LineSegment;

#[derive(Debug, Clone, Copy, PartialEq, Eq, std::hash::Hash)]
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
fn test_parallel_edge() {
    let p = Polygon::Parallelogram(
        Edge::new(5, 10),
        Edge::new(5, 11),
        Edge::new(10, 13),
        Edge::new(11, 13),
    );

    assert_eq!(
        Edge::new(10, 13),
        p.get_parallel_edge_to(Edge::new(5, 11)).unwrap()
    );
    assert_eq!(
        Edge::new(5, 10),
        p.get_parallel_edge_to(Edge::new(11, 13)).unwrap()
    );
    assert_eq!(
        Edge::new(5, 11),
        p.get_parallel_edge_to(Edge::new(10, 13)).unwrap()
    );
    assert_eq!(
        Edge::new(11, 13),
        p.get_parallel_edge_to(Edge::new(5, 10)).unwrap()
    );
}

#[test]
fn test_illegal_flip() {
    // flip
    let fs = "flip[15]:={4,4}; // supported by [{7,10},{8,9}]";
    // triangulation
    let ts =  "T[4] := {{0,1,2},{0,2,3},{1,2,5},{1,4,5},{2,3,5},{3,5,6},{3,6,7},{3,7,8},{4,5,9},{5,6,9},{6,7,9},{7,8,9},{8,9,10},{8,10,11},{8,11,12},{9,10,13},{10,11,13},{11,12,13}};";
    let flip = match Flip::new(fs) {
        Some(f) => f,
        _ => panic!("Failed to parse flip"),
    };

    let triang = match Subdivision::new(ts) {
        Some((t, _)) => t,
        _ => panic!("Failed to parse triangulation"),
    };

    println!("{:?}", flip);
    println!("{:?}", triang);
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

// [[0,2,1],[1,1,1],[1,2,1],[1,3,1],[2,0,1],[2,1,1],[2,2,1],[2,3,1],[2,4,1],[3,1,1],[3,2,1],[3,3,1],[4,2,1]]
#[test]
fn test_is_legal_paralellogram() {
    let vertices = vec![
        crate::polygon::Point { x: 0, y: 2 },
        crate::polygon::Point { x: 1, y: 1 },
        crate::polygon::Point { x: 2, y: 0 },
        crate::polygon::Point { x: 2, y: 1 },
    ];

    let parallelogram = Polygon::from_edges(vec![
        &Edge {
            vertex_one: 0,
            vertex_two: 1,
        },
        &Edge {
            vertex_one: 1,
            vertex_two: 2,
        },
        &Edge {
            vertex_one: 2,
            vertex_two: 3,
        },
        &Edge {
            vertex_one: 0,
            vertex_two: 3,
        },
    ])
    .unwrap();

    assert!(!parallelogram.is_legal_parallelogram(&vertices))
}

#[test]
fn test_is_legal_paralellogram_b() {
    let vertices = vec![
        crate::polygon::Point { x: 0, y: 2 },
        crate::polygon::Point { x: 1, y: 1 },
        crate::polygon::Point { x: 2, y: 1 },
        crate::polygon::Point { x: 1, y: 2 },
    ];

    let parallelogram = Polygon::from_edges(vec![
        &Edge {
            vertex_one: 0,
            vertex_two: 1,
        },
        &Edge {
            vertex_one: 1,
            vertex_two: 2,
        },
        &Edge {
            vertex_one: 2,
            vertex_two: 3,
        },
        &Edge {
            vertex_one: 0,
            vertex_two: 3,
        },
    ])
    .unwrap();

    assert!(parallelogram.is_legal_parallelogram(&vertices))
}

impl Polygon {
    pub fn from_edges(mut edges: Vec<&Edge>) -> Option<Self> {
        edges.sort_by(|a, b| a.compare_to(b));

        // TODO: check if any consecutive edges are collinear

        match edges.len() {
            3 => Some(Polygon::Triangle(*edges[0], *edges[1], *edges[2])),
            4 => {
                // make sure that the points from a parallelogram

                Some(Polygon::Parallelogram(
                    *edges[0], *edges[1], *edges[2], *edges[3],
                ))
            }
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

    /// Returns tuple-of-tuple each containing edges
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

    pub fn get_parallel_edge_to(&self, edge: Edge) -> Option<Edge> {
        match self {
            Polygon::Parallelogram(e1, e2, e3, e4) => {
                for e in vec![e1, e2, e3, e4].into_iter() {
                    if !(e.shares_vertex_with(&edge)) {
                        return Some(*e);
                    }
                }

                None
            }
            _ => None,
        }
    }

    pub fn is_legal_parallelogram(&self, vertices: &[crate::polygon::Point]) -> bool {
        match self.get_disjoint_edges() {
            Some(((e1, e2), (e3, e4))) => {
                return LineSegment::from_points(
                    vertices[e1.vertex_one as usize],
                    vertices[e1.vertex_two as usize],
                )
                .is_parallel_to(LineSegment::from_points(
                    vertices[e2.vertex_one as usize],
                    vertices[e2.vertex_two as usize],
                )) && LineSegment::from_points(
                    vertices[e3.vertex_one as usize],
                    vertices[e3.vertex_two as usize],
                )
                .is_parallel_to(LineSegment::from_points(
                    vertices[e4.vertex_one as usize],
                    vertices[e4.vertex_two as usize],
                ));
            }

            None => false,
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

    pub fn new(i: i16, j: i16) -> Self {
        Self {
            vertex_one: i,
            vertex_two: j,
        }
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

#[test]
fn next_triangle() {
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

    assert_eq!(
        subd.next_triangle(
            Polygon::Parallelogram(
                Edge::new(1, 2),
                Edge::new(1, 4),
                Edge::new(2, 5),
                Edge::new(4, 5),
            ),
            Edge::new(2, 5)
        ),
        None
    );

    assert_eq!(
        subd.next_triangle(
            Polygon::Parallelogram(
                Edge::new(5, 10),
                Edge::new(5, 11),
                Edge::new(10, 13),
                Edge::new(11, 13),
            ),
            Edge::new(11, 13)
        ),
        None
    );
}

impl Subdivision {
    /// Given string of the form a,b,c parse as (a,b,c)
    /// Reject if not in this form
    fn parse_idxs(s: &str) -> Option<(i16, i16, i16)> {
        let split = s.split(",");

        if split.clone().count() != 3 {
            return None;
        }

        let idxs = split
            .map(|s| s.trim().parse::<i16>().unwrap())
            .collect::<Vec<_>>();

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
                        None => return Some((subdivision, subd_idx)),
                    };

                    let tri = Polygon::from_edges(vec![
                        &Edge::new(idx_one, idx_two),
                        &Edge::new(idx_two, idx_three),
                        &Edge::new(idx_one, idx_three),
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
                &Edge::new(verticies[0], verticies[1]),
                &Edge::new(verticies[1], verticies[2]),
                &Edge::new(verticies[0], verticies[2]),
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

    pub fn next_triangle(&self, parallelogram: Polygon, edge: Edge) -> Option<Polygon> {
        let mut edge = edge;
        let mut parallelogram = parallelogram;

        loop {
            let parallel_edge = match parallelogram.get_parallel_edge_to(edge) {
                Some(e) => e,
                None => return None,
            };

            let next_poly = self
                .polygon_map
                .get(&parallel_edge)
                .unwrap()
                .into_iter()
                .filter(|p| *p != &parallelogram)
                .collect::<Vec<_>>();

            if next_poly.len() == 0 {
                return None;
            }

            let next_p = next_poly[0];

            match next_p {
                Polygon::Triangle(..) => {
                    return Some(*next_p);
                }
                Polygon::Parallelogram(..) => {
                    edge = parallel_edge;
                    parallelogram = *next_p;
                }
            }
        }
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
            .map(|s| s.trim().parse::<usize>().unwrap())
            .collect::<Vec<_>>();

        Some((idxs[0], idxs[1]))
    }

    // should be polygons from edge_one
    pub fn apply_flip(
        &self,
        subdivisions: &Vec<Subdivision>,
        vertices: &[crate::polygon::Point],
    ) -> Option<Vec<Subdivision>> {
        let Flip {
            edge_one: edge,
            subdivision_one_idx: _,
            edge_two: _,
            subdivision_two_idx: _,
        } = self;

        let mut flipped_subds: Vec<Subdivision> = vec![];

        for subd in subdivisions {
            let mut polygons;
            let (polygon_a, polygon_b) = match subd.polygon_map.get(&edge.unwrap()) {
                Some(adjs) => {
                    let flip_adj_polygons = (adjs[0], adjs[1]);
                    polygons = subd
                        .polygons
                        .clone()
                        .into_iter()
                        .filter(|p| !adjs.contains(p))
                        .collect::<Vec<_>>();

                    flip_adj_polygons
                }
                None => return None,
            };

            match Polygon::flip_edge(polygon_a, polygon_b, edge.unwrap()) {
                Some(p) => {
                    // or check if its a parallelogram by check if the disjoint edges are parallel
                    if p.is_legal_parallelogram(vertices) {
                        polygons.push(p)
                    } else {
                        println!(
                            "Illegal parallelogram: {} {} {:?}",
                            p,
                            edge.unwrap(),
                            subdivisions
                        );
                        return None;
                    }
                }
                None => {
                    match (polygon_a, polygon_b) {
                        (Polygon::Triangle(..), Polygon::Triangle(..)) => {
                            println!(
                                "Cannot find flip edge: {} {} {}\n\n",
                                edge.unwrap(),
                                polygon_a,
                                polygon_b
                            );
                        }
                        (_, _) => (),
                    };

                    return None;
                }
            };

            flipped_subds.push(Subdivision::new_from_polygons(polygons));
        }

        Some(flipped_subds)
    }

    pub fn set_up_flip(
        &self,
        subdivisions: &HashMap<usize, Vec<Subdivision>>,
    ) -> Option<Vec<Subdivision>> {
        let Flip {
            edge_one,
            subdivision_one_idx: subd_one_idx,
            edge_two,
            subdivision_two_idx: subd_two_idx,
        } = self;

        // check if there exist these subdivisions
        let subds = match subdivisions.get(subd_one_idx) {
            Some(sbds) => sbds,
            None => return None,
        };

        // make sure that the edge has two adjecent polygons
        let flippable_subds = subds
            .clone()
            .into_iter()
            .filter(|subd| match subd.polygon_map.get(&edge_one.unwrap()) {
                Some(adjs) => adjs.len() == 2,
                None => false,
            })
            .collect::<Vec<_>>();

        let subds = match subdivisions.get(subd_two_idx) {
            Some(sbd) => sbd,
            None => return None,
        };

        // make sure that the edge has two adjecent polygons
        if subds
            .iter()
            .any(|subd| match subd.polygon_map.get(&edge_two.unwrap()) {
                Some(adjs) => adjs.len() == 2 && !flippable_subds.is_empty(),
                None => false,
            })
        {
            return Some(flippable_subds);
        }

        None
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
                            if idx_one == idx_two {
                                return None;
                            }

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
