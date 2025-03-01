use criterion::{criterion_group, criterion_main, Criterion};
use regex::Regex;
use std::str::FromStr;
use tcn::subdivision::{Flip, Subdivision};

pub fn parse_without_regex(c: &mut Criterion) {
    use std::{
        fs::File,
        io::{BufRead, BufReader},
    };

    let file_path = "./triang.out";

    let file = File::open(file_path).expect("no such file");
    let buf = BufReader::new(file);
    let lines: Vec<String> = buf
        .lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    c.bench_function("parse topcom file without regex", |b| {
        b.iter(|| {
            let base_length = 10000;
            let mut subdivision_length = 0;
            let mut subdivisions: Vec<Subdivision> = vec![];
            let mut flips: Vec<Flip> = vec![];
            let mut subdivision_idxs = vec![usize::MAX; base_length];

            for line in lines.clone() {
                match line.starts_with("T") {
                    // subdivision
                    true => {
                        let (triangulation, idx) = match Subdivision::new(&line) {
                            Some((triangulation, idx)) => (triangulation, idx),
                            None => continue,
                        };
                        subdivisions.push(triangulation);

                        match idx.cmp(&subdivision_idxs.len()) {
                            std::cmp::Ordering::Less => subdivision_idxs[idx] = subdivision_length,
                            std::cmp::Ordering::Equal => subdivision_idxs.push(subdivision_length),
                            std::cmp::Ordering::Greater => {
                                subdivision_idxs.resize(subdivision_idxs.len() + idx, usize::MAX);
                                subdivision_idxs[idx] = subdivision_length;
                            }
                        }
                        subdivision_length += 1;
                    }
                    // flip
                    false => {
                        if let Some(flip) = Flip::new(&line) {
                            flips.push(flip);
                        }
                    }
                }
            }
        })
    });
}

pub fn parse_with_regex(c: &mut Criterion) {
    use std::{
        fs::File,
        io::{BufRead, BufReader},
    };

    let file_path = "./triang.out";

    let file = File::open(file_path).expect("no such file");
    let buf = BufReader::new(file);
    let lines: Vec<String> = buf
        .lines()
        .map(|l| l.expect("Could not parse line"))
        .collect();

    c.bench_function("parse topcom file with regex", |b| {
        b.iter(|| {
            let base_length = 10000;
            let mut subdivision_length = 0;
            let mut subdivisions: Vec<Subdivision> = vec![];
            let mut flips: Vec<Flip> = vec![];
            let mut subdivision_idxs = vec![usize::MAX; base_length];

            for line in lines.clone() {
                let flip_re = Regex::new(r"flip\[\d+\]").unwrap();
                let triangulation_re = Regex::new(r"T\[\d+\]").unwrap();

                if flip_re.is_match(&line) {
                    if let Some(flip) = Flip::new_with_regex(&line) {
                        flips.push(flip);
                    }
                } else if triangulation_re.is_match(&line) {
                    let triangulation = Subdivision::new_with_regex(&line);
                    subdivisions.push(triangulation);

                    let mts = triangulation_re.find(&line).unwrap();
                    let idx: usize =
                        FromStr::from_str(line.get(mts.start() + 2..mts.end() - 1).unwrap())
                            .unwrap();

                    match idx.cmp(&subdivision_idxs.len()) {
                        std::cmp::Ordering::Less => subdivision_idxs[idx] = subdivision_length,
                        std::cmp::Ordering::Equal => subdivision_idxs.push(subdivision_length),
                        std::cmp::Ordering::Greater => {
                            subdivision_idxs.resize(subdivision_idxs.len() + idx, usize::MAX);
                            subdivision_idxs[idx] = subdivision_length;
                        }
                    }
                    subdivision_length += 1;
                }
            }
        })
    });
}

criterion_group!(benches, parse_with_regex, parse_without_regex);

criterion_main!(benches);
