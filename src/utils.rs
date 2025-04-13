use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;

use crate::subdivision::*;

/// Create a hashtable of non-troplanar graphs to filter them out as skeletons
pub fn parse_non_troplanar_hashes(file: &str) -> Option<HashSet<String>> {
    match file.is_empty() {
        false => {
            let mut hash_set = HashSet::new();
            let res = read_to_string(file);
            match res {
                Ok(lines) => {
                    for line in lines.lines() {
                        hash_set.insert(line.to_string());
                    }

                    Some(hash_set)
                }
                Err(_) => None,
            }
        }
        true => None,
    }
}

/// Parse TOPCOM output
pub fn parse_input(lines: Vec<String>) -> (HashMap<usize, Vec<Subdivision>>, Vec<Flip>) {
    let mut subdivisions_m: HashMap<usize, Vec<Subdivision>> = HashMap::new();
    let mut flips: Vec<Flip> = vec![];

    for line in lines.clone() {
        // less intuitive filtering but flips start with {a,b} and triangulations start with a {{a,b,c},..,{c,d,e}}
        match line.starts_with("{{") {
            // subdivision
            true => {
                let (triangulation, idx) = match Subdivision::new(&line) {
                    Some((triangulation, idx)) => (triangulation, idx),
                    None => continue,
                };

                match subdivisions_m.get_mut(&idx) {
                    Some(v) => v.push(triangulation),
                    None => {
                        subdivisions_m.insert(idx, vec![triangulation]);
                    }
                }
            }
            // flip
            false => {
                if let Some(flip) = Flip::new(&line) {
                    flips.push(flip);
                }
            }
        }
    }

    (subdivisions_m, flips)
}

/// Compute the gcd of two numbers if neither are 0
/// If either are zero return the other
pub fn foo_gcd(mut n: i16, mut m: i16) -> i16 {
    if n == 0 {
        return m;
    }
    if m == 0 {
        return n;
    }

    while m != 0 {
        if m < n {
            std::mem::swap(&mut m, &mut n);
        }
        m %= n;
    }
    n
}
