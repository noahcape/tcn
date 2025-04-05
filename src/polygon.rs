use regex::Regex;
use std::{
    collections::HashSet,
    fmt,
    io::{self, Read},
};

use crate::subdivision::Polygon;

// A 2D lattice polygon
// @param: points (the minimal defining set of points)
#[derive(Debug)]
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
fn test_interior_points() {
    let pointa = Point { x: 0, y: -1 };
    let pointb = Point { x: 5, y: 0 };
    let pointc = Point { x: 1, y: 1 };
    let points = vec![pointa, pointb, pointc];
    let lp = LatticePolygon { points };

    println!("Interior Points: {:?}", lp.interior_points());
    println!(
        "Convex hull of interior points: {:?}",
        lp.interior_points().convex_hull()
    );
}

#[test]
fn test_interior_points_2() {
    let pointa = Point { x: -1, y: -1 };
    let pointb = Point { x: 4, y: 0 };
    let pointc = Point { x: 4, y: 1 };
    let pointd = Point { x: 0, y: 2 };
    let points = vec![pointa, pointb, pointc, pointd];
    let lp = LatticePolygon { points };

    println!("Lp points: {}", lp);
    println!("Interior pts: {}", lp.interior_points());
    println!(
        "Convex hull of interior pts: {}",
        lp.interior_points().convex_hull()
    );
    println!("Doubly interior pts: {}", lp.doubly_interior());
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

    assert_eq!(vec![doubly_interior], lp.doubly_interior().points);
}

#[test]
fn test_parsing_vertices() {
    let vertices = "[[0,0,1],[1,0,1],[2,0,1],[3,0,1],[4,0,1],[5,0,1],[6,0,1],[0,1,1],[1,1,1],[2,1,1],[3,1,1],[4,1,1],[0,2,1],[1,2,1],[2,2,1],[0,3,1]]\n";

    let parsed_vertices = Point::parse_many_h(vertices.to_owned());
    println!("{:?}", parsed_vertices);
}

impl LatticePolygon {
    pub fn is_convex(&self) -> bool {
        self.interior_points().points.is_empty()
    }

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

    pub fn to_ordered_string(&self) -> String {
        let mut copied_points = self.points.clone();
        copied_points.sort_by(|t, o| t.x.cmp(&o.x));
        copied_points.sort_by(|t, o| t.y.cmp(&o.y));
        let mut buffer = String::new();
        for p in copied_points {
            fmt::write(&mut buffer, format_args!("{}", p)).unwrap();
        }
        buffer
    }

    // read from IO to parse polygons
    pub fn parse_many() -> Vec<Self> {
        let lines = io::stdin().lines();
        let mut polygons: Vec<LatticePolygon> = vec![];

        for line in lines {
            let mut lattice_points = vec![];
            match line {
                Ok(l) => match l.is_empty() {
                    false => {
                        let re = Regex::new(r"<-?\d+, -?\d+>").unwrap();
                        for mat in re.find_iter(&l) {
                            let points = l
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
                },
                _ => unreachable!("Something went wrong reading the file"),
            }
            polygons.push(LatticePolygon {
                points: lattice_points,
            });
        }

        polygons
    }

    fn to_lines(&self) -> Vec<LineSegment> {
        let shifted = {
            let mut copied = self.points.clone();
            let first = copied.remove(0);
            copied.push(first);
            copied
        };

        self.points
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

    pub fn interior_points(&self) -> Self {
        let mut interior_points = vec![];

        if self.points.is_empty() {
            return LatticePolygon {
                points: interior_points,
            };
        }

        let lines = self.to_lines();
        let bbox = self.get_bounded_box();

        for point in bbox {
            if !LatticePolygon::on_boundary(&lines, point) && self.is_interior(&lines, point) {
                interior_points.push(point);
            }
        }

        LatticePolygon {
            points: interior_points,
        }
    }

    // Given a polygon compute the convex hull
    pub fn convex_hull(&self) -> Self {
        let mut convex_hull = vec![];

        if self.points.len() < 3 {
            return LatticePolygon {
                points: convex_hull,
            };
        }

        let mut index = 0;
        let leftmost = self.points[index];

        let mut cur = leftmost;

        loop {
            convex_hull.push(cur);

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

        LatticePolygon {
            points: convex_hull,
        }
    }

    pub fn doubly_interior(&self) -> Self {
        let interior = self.interior_points();
        let interior_convex_hull = interior.convex_hull();

        interior_convex_hull.interior_points()
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Point {
    pub x: i16,
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

    // check if c is on the line segment of a-b
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

#[derive(Copy, Clone, Debug)]
pub struct LineSegment {
    pub a: Point,
    pub b: Point,
}

impl LineSegment {
    pub fn from_points(p1: Point, p2: Point) -> Self {
        LineSegment { a: p1, b: p2 }
    }

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
