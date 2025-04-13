use std::{cmp::Ordering, collections::HashMap, fmt};

use crate::polygon::LineSegment;

/// Polygons which subdivisions can be made up of
#[derive(Debug, Clone, Copy, PartialEq, Eq, std::hash::Hash)]
pub enum Polygon {
    /// Three edges which define a triangle
    Triangle(Edge, Edge, Edge),
    /// Four edges which define a parallelogram
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

    let parrallelogram = Polygon::remove_edge(
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
    /// Initialize a new polygon from a vector of edges
    ///
    /// Returns parallelogram if input is four edges
    /// A triangle if input is three edges
    /// Nothing is input is fewer or more
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

    /// Returns an iterator over the edges of a polygon
    pub fn edge_itr(&self) -> impl Iterator<Item = &'_ Edge> {
        match self {
            Polygon::Triangle(edge, edge1, edge2) => vec![edge, edge1, edge2].into_iter(),
            Polygon::Parallelogram(edge, edge1, edge2, edge3) => {
                vec![edge, edge1, edge2, edge3].into_iter()
            }
        }
    }

    /// True if the edge is part of the polygon
    /// Edges vertices are ordered on initialization so that `vertex_one` < `vertex_two`
    pub fn contains_edge(&self, edge: &Edge) -> bool {
        self.edge_itr().any(|e| e == edge)
    }

    /// Remove an edge between two triangles forming a parallelogram of left over edges
    /// `polya` and `polyb` both must contain `edge`
    pub fn remove_edge(polya: Polygon, polyb: Polygon, edge: Edge) -> Option<Polygon> {
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

    /// Return true if polygon is a triangle
    pub fn is_triangle(&self) -> bool {
        match self {
            Polygon::Parallelogram(..) => false,
            Polygon::Triangle(..) => true,
        }
    }

    /// Returns tuple-of-tuple each containing edges with disjoint vertices
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

    /// Returns the parallel edge to input edge
    /// None is `self` is a triangle
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

    /// Determine if a parallelogram is legal - made up of two parallel edges
    pub fn is_legal_parallelogram(&self, vertices: &[crate::polygon::Point]) -> bool {
        match self.get_disjoint_edges() {
            Some(((e1, e2), (e3, e4))) => {
                LineSegment::from_points(
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
                ))
            }

            None => false,
        }
    }
}

/// A data structure representing edges of a subdivision between two lattice points (vertices)
#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Edge {
    /// First vertex of edge
    pub vertex_one: i16,
    /// Second vertex of edge
    pub vertex_two: i16,
}

impl fmt::Display for Edge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.vertex_one, self.vertex_two)
    }
}

impl Edge {
    /// Create a new edge from two vertices
    pub fn new(i: i16, j: i16) -> Self {
        Self {
            vertex_one: i,
            vertex_two: j,
        }
    }

    /// Compare two vertices of edges for ordering
    fn compare_to(&self, other: &Edge) -> Ordering {
        self.vertex_two.cmp(&other.vertex_two)
    }

    /// Check if two edges share a vertex
    fn shares_vertex_with(&self, other: &Edge) -> bool {
        self.vertex_one == other.vertex_one
            || self.vertex_one == other.vertex_two
            || self.vertex_two == other.vertex_one
            || self.vertex_two == other.vertex_two
    }
}

/// Data structure to represent a subdivision
#[derive(Debug, Clone, PartialEq)]
pub struct Subdivision {
    /// Polygons which the subdivision is made out of
    pub polygons: Vec<Polygon>,
    /// A map of edges and polygons which contain that edge
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
    let test_str = "{{0,1,2},{1,2,4},{1,3,4},{3,4,5}} # ID: 12";

    let polygons = vec![
        Polygon::Triangle(Edge::new(0, 1), Edge::new(1, 2), Edge::new(0, 2)),
        Polygon::Triangle(Edge::new(1, 2), Edge::new(2, 4), Edge::new(1, 4)),
        Polygon::Triangle(Edge::new(1, 3), Edge::new(3, 4), Edge::new(1, 4)),
        Polygon::Triangle(Edge::new(3, 4), Edge::new(4, 5), Edge::new(3, 5)),
    ];

    let t_subd = Subdivision::new_from_polygons(polygons);
    let (subd, subd_idx) = Subdivision::new(test_str).unwrap();

    assert_eq!(subd, t_subd);
    assert_eq!(subd_idx, 12);
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

    /// Create a new subdivision from a vector of polygons
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

    /// Create a new subdivision from a string representation of the subdivision (expecting the format from TOPCOM)
    ///
    /// Ex: {{0,1,2},{1,2,4},{1,3,4},{3,4,5}} # ID: 1
    /// Where in this example there are four interior triangles which make up the subdivision and it is identified with idx 1.
    pub fn new(subd_str: &str) -> Option<(Self, usize)> {
        let mut subdivision = Subdivision {
            polygon_map: HashMap::new(),
            polygons: vec![],
        };

        let mut cumul = String::new();
        let mut start_cumul = false;

        for chr in subd_str.chars() {
            match chr {
                ':' | 'D' | 'I' | '#' => continue,
                '{' => {
                    start_cumul = true;
                }
                '}' => {
                    let (idx_one, idx_two, idx_three) = match Subdivision::parse_idxs(&cumul) {
                        Some((i, j, k)) => (i, j, k),
                        None => {
                            start_cumul = true;
                            cumul = String::new();
                            continue;
                        } // this should happen only after all indices have been parsed
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

        let subd_idx = cumul.trim().parse::<usize>().unwrap();

        Some((subdivision, subd_idx))
    }

    /// Find the next triangle - or boundary if returns None - by following parallel edges of a parallelogram
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
                .iter()
                .filter(|p| *p != &parallelogram)
                .collect::<Vec<_>>();

            if next_poly.is_empty() {
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
    /// Edge which can be flipped in `subdivision_one`
    pub edge_one: Option<Edge>,
    /// Subdivision which `edge_one` exists in
    pub subdivision_one_idx: usize,
    /// Edge which can be flipped in `subdivision_two`
    pub edge_two: Option<Edge>,
    /// Subdivision which `edge_two` exists in
    pub subdivision_two_idx: usize,
}

#[test]
fn parse_flips() {
    let test_str_one = "{4, 9} # ID: 9 supported on [{2,7},{4,5}] (to new triang)";
    let f = Flip::new(test_str_one).unwrap();
    let t_f: Flip = Flip {
        subdivision_one_idx: 9,
        subdivision_two_idx: 4,
        edge_one: Some(Edge::new(2, 7)),
        edge_two: Some(Edge::new(4, 5)),
    };

    assert_eq!(f, t_f);

    let test_str_two = "{78, 78} # ID: 9 supported on [{4},{1,8}] (to new triang)";
    assert!(Flip::new(test_str_two).is_none());
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

    /// Remove a flip to add a unit parallelogram to a subdivision
    ///
    /// Before removing the flip ensure that removing it will add a legal unit parallelogram, that is a convex parallelogram with area 1.
    /// Return a list of all subdivisions with the flipped edge such that breaking the edge adds a legal unit parallelogram.
    pub fn remove_flip(
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
                None => continue,
            };

            match Polygon::remove_edge(polygon_a, polygon_b, edge.unwrap()) {
                Some(p) => {
                    // or check if its a parallelogram by check if the disjoint edges are parallel
                    if p.is_legal_parallelogram(vertices) {
                        polygons.push(p)
                    } else {
                        continue;
                    }
                }
                None => continue,
            };

            flipped_subds.push(Subdivision::new_from_polygons(polygons));
        }

        Some(flipped_subds)
    }

    /// Finds the corresponding subdivisions which a flip is defined on.
    ///
    /// If the corresponding interior polygons of the subdivisions are not compatible with the flip return None
    /// Else return the subdivisions where the flip can be broken in
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

    /// Parsing a string which specifies a flip in a triangulation
    ///
    /// Ex: {4, 9} # ID: 9 supported on [{2,7},{4,5}] (to new triang)
    /// This means that the edge between vertices (4,5) in triangulation 4 is flipped to become the edge (2,7) in triangulation 4.
    pub fn new(flip_str: &str) -> Option<Flip> {
        let mut flip = Flip {
            edge_one: None,
            edge_two: None,
            subdivision_one_idx: usize::MAX,
            subdivision_two_idx: usize::MAX,
        };

        let mut cumul = String::new();
        let mut start_cumul = false;
        let mut count = 0;
        for chr in flip_str.chars() {
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
                            flip.edge_one = Some(Edge::new(idx_one as i16, idx_two as i16));
                        }
                        2 => {
                            flip.edge_two = Some(Edge::new(idx_one as i16, idx_two as i16));
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
}

/// Remove flipped edges from subdivisions to add unit parallelogram
pub fn remove_flipped_edges(
    subdivisions_m: &HashMap<usize, Vec<Subdivision>>,
    flips: &Vec<Flip>,
    vertices: &[crate::polygon::Point],
) -> HashMap<usize, Vec<Subdivision>> {
    let mut nodal_subdivisions_m: HashMap<usize, Vec<Subdivision>> = HashMap::new();

    // Flip edge in both triangulation one and two - as long as it is possible
    for flip in flips {
        let flipped_subdivisions = match flip.set_up_flip(subdivisions_m) {
            Some(flippable_subdivisions) => {
                match flip.remove_flip(&flippable_subdivisions, vertices) {
                    Some(f_subd) => f_subd,
                    None => continue,
                }
            }
            None => continue,
        };

        match nodal_subdivisions_m.get_mut(&flip.subdivision_one_idx) {
            Some(v) => v.extend(flipped_subdivisions),
            None => {
                nodal_subdivisions_m.insert(flip.subdivision_one_idx, flipped_subdivisions);
            }
        }
    }

    nodal_subdivisions_m
}
