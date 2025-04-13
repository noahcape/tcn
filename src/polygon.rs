use regex::Regex;
use std::{
    collections::HashSet,
    fmt::{self, Write},
    fs,
    io::{BufRead, BufReader, Read},
};

use crate::{subdivision::Polygon, utils};

/// A 2D lattice polygon
#[derive(Debug, Hash, Clone)]
pub struct LatticePolygon {
    points: Vec<Point>,
}

impl fmt::Display for LatticePolygon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buffer = String::new();
        for p in &self.points {
            fmt::write(&mut buffer, format_args!("{} ", p)).unwrap();
        }

        write!(f, "{buffer}")
    }
}

#[test]
fn test_boundary_points() {
    let pointa = Point { x: 0, y: 1 };
    let pointb = Point { x: 1, y: 0 };
    let pointc = Point { x: 4, y: 0 };
    let pointd = Point { x: 4, y: 1 };
    let pointe = Point { x: 3, y: 2 };
    let pointf = Point { x: 0, y: 2 };
    let points = vec![pointa, pointb, pointc, pointd, pointe, pointf];
    let lp = LatticePolygon { points };

    let bpointa = Point::parse_from_str("2,0,0").unwrap();
    let bpointb = Point::parse_from_str("3,0,0").unwrap();
    let bpointc = Point::parse_from_str("1,2,0").unwrap();
    let bpointd = Point::parse_from_str("2,2,0").unwrap();

    let boundary_lp = LatticePolygon {
        points: vec![
            pointb, bpointa, bpointb, pointc, pointd, pointe, pointa, bpointc, bpointd, pointf,
        ],
    };

    assert_eq!(
        lp.boundary_points().order_points().points,
        boundary_lp.order_points().points
    );
}

#[test]
fn test_nth_interior() {
    let pointa = Point { x: 0, y: 0 };
    let pointb = Point { x: 6, y: 2 };
    let pointc = Point { x: 2, y: 4 };
    let points = vec![pointa, pointb, pointc];
    let lp = LatticePolygon { points };

    assert_eq!(lp.maximal_interior(false), 2);
}

#[test]
fn test_interior_points() {
    let pointa = Point { x: 0, y: -1 };
    let pointb = Point { x: 5, y: 0 };
    let pointc = Point { x: 1, y: 1 };
    let points = vec![pointa, pointb, pointc];
    let lp = LatticePolygon { points };
    let interior = lp.interior_points(false);

    let interior_pts = vec![
        Point { x: 1, y: 0 },
        Point { x: 2, y: 0 },
        Point { x: 3, y: 0 },
        Point { x: 4, y: 0 },
    ];

    println!("Interior Points: {:?}", interior);
    assert_eq!(
        interior.order_points().points,
        LatticePolygon {
            points: interior_pts
        }
        .order_points()
        .points
    );
    assert_eq!(lp.num_boundary_points(), 3);
    assert!(lp.is_hyperelliptic(&interior));
    assert_eq!(lp.maximal_interior(false), 1);
}

#[test]
fn test_interior_points_2() {
    let pointa = Point { x: -1, y: -1 };
    let pointb = Point { x: 4, y: 0 };
    let pointc = Point { x: 4, y: 1 };
    let pointd = Point { x: 0, y: 2 };
    let points = vec![pointa, pointb, pointc, pointd];
    let lp = LatticePolygon { points };
    let interior = lp.interior_points(false);

    let interior_t = vec![
        Point { x: 0, y: 0 },
        Point { x: 0, y: 1 },
        Point { x: 1, y: 0 },
        Point { x: 1, y: 1 },
        Point { x: 2, y: 0 },
        Point { x: 2, y: 1 },
        Point { x: 3, y: 0 },
        Point { x: 3, y: 1 },
    ];

    assert_eq!(
        interior.order_points().points,
        LatticePolygon {
            points: interior_t.clone()
        }
        .order_points()
        .points
    );
    assert_eq!(
        interior
            .convex_set()
            .boundary_points()
            .order_points()
            .points,
        LatticePolygon { points: interior_t }.order_points().points
    );
    assert!(interior.doubly_interior(false).points.is_empty());
    assert_eq!(lp.num_boundary_points(), 4);
    assert!(!lp.is_hyperelliptic(&interior));
    assert!(!interior.is_hyperelliptic(&interior));
}
#[test]
fn test_doulby_interior_points() {
    let doubly_interior = Point { x: 1, y: 1 };

    let pointa = Point { x: -1, y: 0 };
    let pointb = Point { x: 0, y: -1 };
    let pointc = Point { x: 2, y: -1 };
    let pointd = Point { x: 3, y: 0 };
    let pointe = Point { x: 3, y: 1 };
    let pointf = Point { x: 1, y: 3 };
    let pointg = Point { x: 0, y: 3 };
    let pointh = Point { x: -1, y: 2 };
    let points = vec![
        pointa, pointb, pointc, pointd, pointe, pointf, pointg, pointh,
    ];
    let lp = LatticePolygon { points };

    assert_eq!(vec![doubly_interior], lp.doubly_interior(false).points);

    println!("{}", lp.topcom_p());
}

#[test]
fn test_parsing_vertices() {
    let vertices = "[[0,0,1],[1,0,1],[2,0,1],[3,0,1],[4,0,1],[5,0,1],[6,0,1],[0,1,1],[1,1,1],[2,1,1],[3,1,1],[4,1,1],[0,2,1],[1,2,1],[2,2,1],[0,3,1]]\n";

    let parsed_vertices = Point::parse_many_h(vertices.to_owned());
    println!("{:?}", parsed_vertices);
}

impl LatticePolygon {
    /// Print out lattice polygon so use for input to TOPCOM software
    /// Expects lattice polygons as: [[x,y,z],...]
    pub fn topcom_p(&self) -> String {
        let mut buffer = String::new();
        let _ = buffer.write_char('[');
        for (idx, p) in self.points.clone().into_iter().enumerate() {
            let _ = buffer.write_fmt(format_args!(
                "{}{}",
                p.topcom_p(),
                if idx < (self.points.len() - 1) {
                    ","
                } else {
                    ""
                }
            ));
        }
        let _ = buffer.write_char(']');

        buffer
    }

    /// Create a new lattice polygon from a polygon specified by indicies of vertices
    pub fn from_polygon(p: Polygon, vertices: &[Point]) -> Self {
        LatticePolygon {
            points: p
                .edge_itr()
                .flat_map(|e| {
                    vec![
                        vertices[e.vertex_one as usize],
                        vertices[e.vertex_two as usize],
                    ]
                })
                .collect::<HashSet<_>>()
                .into_iter()
                .collect::<Vec<_>>(),
        }
    }

    /// Create a new polygon by joining the points of two polygons
    /// Does not check if there is an overlap of points - assumes the point set are distinct
    pub fn join_polygons(&mut self, o: Self) -> Self {
        Self {
            points: [self.points.clone(), o.points].concat(),
        }
    }

    /// Order points of lattice polygon, first by x then by y
    pub fn order_points(&self) -> Self {
        let mut lp = LatticePolygon {
            points: self.points.clone(),
        };
        lp.points.sort_by(|t, o| t.x.cmp(&o.x));
        lp.points.sort_by(|t, o| t.y.cmp(&o.y));

        lp
    }

    /// Create a string representation of a lattice points of a lattice polygons ordered first by x coordinate then by y coordinate
    pub fn to_ordered_string(&self) -> String {
        let ordered_lp = self.order_points();
        let mut buffer = String::new();
        for (idx, p) in ordered_lp.points.clone().into_iter().enumerate() {
            let _ = buffer.write_fmt(format_args!(
                "{}{}",
                p,
                if idx < (ordered_lp.points.len() - 1) {
                    ", "
                } else {
                    ""
                }
            ));
        }
        buffer
    }

    /// Parse many lattice polygons
    pub fn parse_many(pfile: String) -> Result<Vec<Self>, String> {
        let file = match fs::File::open(pfile.clone()) {
            Ok(f) => f,
            Err(_) => return Err(format!("failed to open file {pfile}")),
        };
        let pfile_lines: Vec<String> = BufReader::new(file)
            .lines()
            .map(|l| l.unwrap_or_else(|_| format!("Could not parse line from {pfile}")))
            .collect();

        let mut polygons: Vec<LatticePolygon> = vec![];

        for line in pfile_lines {
            let mut lattice_points = vec![];
            match line.is_empty() {
                false => {
                    let re = Regex::new(r"<-?\d+, -?\d+>").unwrap();
                    for mat in re.find_iter(&line) {
                        let points = line
                            .get(mat.range())
                            .unwrap()
                            .split(",")
                            .map(|s| {
                                s.trim_matches(|c| c == '>' || c == '<')
                                    .trim()
                                    .parse::<i16>()
                                    .unwrap()
                            })
                            .collect::<Vec<_>>();
                        lattice_points.push(Point::from_vec(points));
                    }
                }
                true => continue,
            }
            polygons.push(LatticePolygon {
                points: lattice_points,
            });
        }

        Ok(polygons)
    }

    fn to_lines(&self, is_full: bool) -> Vec<LineSegment> {
        let boundary_points = if is_full {
            self.convex_set().points
        } else {
            self.points.clone()
        };

        if boundary_points.is_empty() {
            return vec![];
        }

        let shifted = {
            let mut copied = boundary_points.clone();
            let first = copied.remove(0);
            copied.push(first);
            copied
        };

        boundary_points
            .iter()
            .zip(shifted.iter())
            .map(|(a, b)| LineSegment { a: *a, b: *b })
            .collect::<Vec<_>>()
    }

    fn get_bounded_box(&self) -> Vec<Point> {
        let xs = self.points.iter().map(|p| p.x);
        let ys = self.points.iter().map(|p| p.y);
        let min_x = xs.clone().min().unwrap();
        let max_x = xs.max().unwrap();

        let min_y = ys.clone().min().unwrap();
        let max_y = ys.max().unwrap();

        let mut bbox: Vec<Point> = vec![];

        for i in min_x..=max_x {
            for j in min_y..=max_y {
                bbox.push(Point { x: i, y: j });
            }
        }

        bbox
    }

    fn on_boundary(lines: &Vec<LineSegment>, point: Point) -> bool {
        for line in lines {
            if point.on_line(line) {
                return true;
            }
        }

        false
    }

    fn is_interior(&self, lines: &Vec<LineSegment>, point: Point) -> bool {
        for bp in &self.points {
            let to_boundary = LineSegment { a: *bp, b: point };
            for line in lines {
                if to_boundary.intersect(line) {
                    return false;
                }
            }
        }

        true
    }

    /// Compute interior points of a lattice polygon
    /// TODO: this should have a flag either get bounded box or just use the polygon if its `full`
    pub fn interior_points(&self, is_full: bool) -> Self {
        let mut interior_points = vec![];

        if self.points.is_empty() {
            return LatticePolygon {
                points: interior_points,
            };
        }

        let lines = self.to_lines(is_full);

        let bbox = if is_full {
            self.points.clone()
        } else {
            self.get_bounded_box()
        };

        for point in bbox {
            if !LatticePolygon::on_boundary(&lines, point) && self.is_interior(&lines, point) {
                interior_points.push(point);
            }
        }

        LatticePolygon {
            points: interior_points,
        }
    }

    /// Given a lattice polygon compute its convex set.
    ///
    /// That is a new convex lattice polygon (defined by its boundary points) containing all the points contained in `self`
    /// May not return all boundary points of convex set, yet the polygon will be convex lattice polygon
    pub fn convex_set(&self) -> Self {
        let mut convex_set = vec![];

        if self.points.len() < 3 {
            return LatticePolygon { points: convex_set };
        }

        let mut index = 0;
        let leftmost = self.points[index];

        let mut cur = leftmost;

        loop {
            convex_set.push(cur);

            index = (index + 1) % self.points.len();
            for i in 0..self.points.len() {
                match Point::orientation(&cur, &self.points[i], &self.points[index]) {
                    Orientation::CounterClockwise => index = i,
                    _ => continue,
                }
            }

            cur = self.points[index];
            if cur == leftmost {
                break;
            }
        }

        LatticePolygon { points: convex_set }
    }

    /// Compute all doubly interior points of a lattice polygon
    pub fn doubly_interior(&self, is_full: bool) -> Self {
        let interior = self.interior_points(is_full);
        let interior_convex_hull = interior.convex_set();

        interior_convex_hull.interior_points(is_full)
    }

    /// Return a list of the boundary points of a lattice polygons
    pub fn num_boundary_points(&self) -> usize {
        // For the line from (𝑎,𝑏) to (𝑐,𝑑) the number of such points is
        // gcd(c-a,d-b)+1.

        let convex_set = self.convex_set();

        // create a new list of points to iterate over
        let mut tpoints = convex_set.points.clone();
        tpoints.push(convex_set.points[0]);
        let mut boundary_points = convex_set.points.len();

        for (a_idx, b) in tpoints[1..].iter().enumerate() {
            let a = tpoints[a_idx];
            // this included a and b to remove them in the end
            boundary_points +=
                ((utils::foo_gcd((b.x - a.x).abs(), (b.y - a.y).abs()) + 1) - 2) as usize;
        }

        boundary_points
    }

    /// Compute the area of a convex lattice polygon using Pick's theorem
    ///
    /// Pick's theorem states for any convex lattice polygon the area is given by
    /// I: number of interior points
    /// B: number of boundary points
    /// A = I + B/2 - 1
    pub fn area_picks(&self) -> usize {
        let i = self.interior_points(false).points.len();
        let b = self.num_boundary_points();

        i + (b / 2) - 1
    }

    /// Compute the area of a covnex lattice polygon using Pick's theorem
    ///
    /// If interior polygon has already been computed don't recompute here
    /// Pick's theorem states for any convex lattice polygon the area is given by
    /// I: number of interior points
    /// B: number of boundary points
    /// A = I + B/2 - 1
    pub fn area_picks_interior(&self, interior_polygon: &Self) -> usize {
        let i = interior_polygon.points.len();
        let b = self.num_boundary_points();

        i + (b / 2) - 1
    }

    /// Determine if a polygon is hyperelleptic
    ///
    /// True if the number of points between the rightmost and leftmost pattice account for all interior points of the polygon
    pub fn is_hyperelliptic(&self, interior: &Self) -> bool {
        assert!(!interior.points.is_empty());

        let a = interior.points[0];
        let b = interior.points.last().unwrap();

        let points_inbetween = (utils::foo_gcd((b.x - a.x).abs(), (b.y - a.y).abs()) + 1) as usize;

        points_inbetween == interior.points.len()
    }

    /// Given a minimal set of points to define a convex lattice polygon find all points on that boundary
    pub fn boundary_points(&self) -> Self {
        let mut tpoints = self.points.clone();
        tpoints.push(self.points[0]);

        let mut boundary_points: Vec<Point> = self.points.clone();

        for (a_idx, b) in tpoints[1..].iter().enumerate() {
            let a = tpoints[a_idx];
            let mut dy = b.y - a.y;
            let mut dx = b.x - a.x;

            let gcd = utils::foo_gcd(dx.abs(), dy.abs());
            dy /= gcd;
            dx /= gcd;
            let mut next: Point = Point {
                x: a.x + dx,
                y: a.y + dy,
            };

            while !next.eq(b) {
                boundary_points.push(next);

                next = Point {
                    x: next.x + dx,
                    y: next.y + dy,
                };
            }
        }

        Self {
            points: boundary_points,
        }
    }

    /// Compute the maximum interiority of a polygon
    pub fn maximal_interior(&self, is_full: bool) -> usize {
        let mut n_interior = 0;

        let mut interior = self.interior_points(is_full);
        while !interior.points.is_empty() {
            n_interior += 1;

            interior = interior.interior_points(true);
        }

        n_interior
    }
}

/// Point data structure
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Point {
    /// x coordinate
    pub x: i16,
    /// y coordinate
    pub y: i16,
}

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

#[derive(PartialEq, Eq)]
enum Orientation {
    Clockwise,
    Collinear,
    CounterClockwise,
}

impl Orientation {
    fn is_clockwise(&self) -> bool {
        self == &Orientation::Clockwise
    }
}

impl Point {
    fn topcom_p(&self) -> String {
        format_args!("[{},{},1]", self.x, self.y).to_string()
    }

    fn from_vec(ps: Vec<i16>) -> Self {
        assert_eq!(ps.len(), 2);
        Point { x: ps[0], y: ps[1] }
    }

    fn orientation(a: &Point, b: &Point, c: &Point) -> Orientation {
        let val = (c.y - a.y) * (b.x - a.x) - (b.y - a.y) * (c.x - a.x);

        match val {
            0 => Orientation::Collinear,
            1_i16..=i16::MAX => Orientation::CounterClockwise,
            i16::MIN..=-1_i16 => Orientation::Clockwise,
        }
    }

    /// Check if a point lies on a line segment
    pub fn on_line(&self, line: &LineSegment) -> bool {
        let a = line.a;
        let b = line.b;
        let c = self;

        let cross_product = (c.y - a.y) * (b.x - a.x) - (c.x - a.x) * (b.y - a.y);
        let dot_product = (c.x - a.x) * (b.x - a.x) + (c.y - a.y) * (b.y - a.y);
        let squared_length_ba = (b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y);

        !(cross_product.abs() != 0 || dot_product < 0 || dot_product > squared_length_ba)
    }

    fn parse_from_str(s: &str) -> Option<Point> {
        let split = s.split(",");

        if split.clone().count() != 3 {
            return None;
        }

        let idxs = split.map(|s| s.parse::<i16>().unwrap()).collect::<Vec<_>>();

        Some(Point {
            x: idxs[0],
            y: idxs[1],
        })
    }

    fn parse_many_h(buf: String) -> Option<Vec<Point>> {
        let mut points = vec![];

        let mut cumul = String::new();
        let mut start_cumul = false;

        for chr in buf.chars() {
            match chr {
                '[' => start_cumul = true,
                ']' => {
                    if cumul.is_empty() {
                        break;
                    }
                    let pt = match Point::parse_from_str(&cumul) {
                        Some(pt) => pt,
                        None => return None,
                    };
                    points.push(pt);
                    start_cumul = false;
                    cumul = String::new();
                }
                _ => {
                    if start_cumul {
                        cumul.push(chr);
                    }
                }
            }
        }

        Some(points)
    }

    /// Parse many points
    pub fn parse_many(file: String) -> Option<Vec<Point>> {
        let mut fopen = std::fs::File::open(file).unwrap();
        let mut buf = String::new();

        match fopen.read_to_string(&mut buf) {
            Ok(_) => (),
            Err(_) => panic!("Failed to read vertex file"),
        }

        Self::parse_many_h(buf)
    }
}

/// A line segment
#[derive(Copy, Clone, Debug)]
pub struct LineSegment {
    /// One end of line segment
    pub a: Point,
    /// Another end of the line segment
    pub b: Point,
}

impl LineSegment {
    /// Create a new line segment between `p1` and `p2`
    pub fn from_points(p1: Point, p2: Point) -> Self {
        LineSegment { a: p1, b: p2 }
    }

    /// Determine if two line segments are parallel
    pub fn is_parallel_to(&self, other: LineSegment) -> bool {
        let (this_slope_num, this_slope_denom) = self.slope();
        let (other_slope_num, other_slope_denom) = other.slope();

        this_slope_num * other_slope_denom == other_slope_num * this_slope_denom
    }

    // slope as (numerator, denominator)
    fn slope(&self) -> (i16, i16) {
        ((self.b.y - self.a.y).abs(), (self.b.x - self.a.x).abs())
    }

    fn intersect(&self, other: &LineSegment) -> bool {
        let a = self.a;
        let b = self.b;
        let c = other.a;
        let d = other.b;

        (Point::orientation(&a, &c, &d).is_clockwise()
            != Point::orientation(&b, &c, &d).is_clockwise())
            && (Point::orientation(&a, &b, &c).is_clockwise()
                != Point::orientation(&a, &b, &d).is_clockwise())
    }
}
