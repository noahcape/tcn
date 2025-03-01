use std::collections::{HashMap, HashSet};
use std::fs::{self, read_to_string};
use std::io::{BufRead, BufReader};

use crate::polygon::*;
use crate::subdivision::*;

pub fn parse_nonplanar_hashes(file: &str) -> Option<HashSet<String>> {
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

pub fn apply_flips(
    subdivisions: &[Subdivision],
    flips: &Vec<Flip>,
    subdivision_idxs: &[usize],
    num_triangulations: usize,
) -> (Vec<Subdivision>, Vec<usize>) {
    let mut num_nodal_subdivisions = 0;
    let mut nodal_subdivisions: Vec<Subdivision> = vec![];
    let mut nodal_subd_idxs: Vec<usize> = vec![usize::MAX; num_triangulations];

    for flip in flips {
        if flip.subdivision_one_idx >= subdivision_idxs.len()
            || flip.subdivision_two_idx >= subdivision_idxs.len()
        {
            continue;
        }

        if subdivision_idxs[flip.subdivision_one_idx] == usize::MAX
            || subdivision_idxs[flip.subdivision_two_idx] == usize::MAX
        {
            continue;
        }

        let subdivision_one = &subdivisions[subdivision_idxs[flip.subdivision_one_idx]];
        let subdivision_two = &subdivisions[subdivision_idxs[flip.subdivision_two_idx]];

        let subd_one_adjacent_polygons = subdivision_one.polygon_map.get(&flip.edge_one.unwrap());
        match subd_one_adjacent_polygons {
            Some(adjacency_list) => {
                if adjacency_list.len() != 2 {
                    continue;
                }
            }
            None => continue,
        }

        let subd_two_adjacent_polygons = subdivision_two.polygon_map.get(&flip.edge_two.unwrap());
        match subd_two_adjacent_polygons {
            Some(adjacency_list) => {
                if adjacency_list.len() != 2 {
                    continue;
                }
            }
            None => continue,
        }

        let mut polygons = subdivision_one
            .polygons
            .iter()
            .filter_map(|p| {
                if !p.contains_edge(&flip.edge_one.unwrap()) {
                    Some(*p)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        let polygons_in_flip = subdivision_one
            .polygon_map
            .get(&flip.edge_one.unwrap())
            .unwrap();

        match Polygon::flip_edge(
            polygons_in_flip[0],
            polygons_in_flip[1],
            flip.edge_one.unwrap(),
        ) {
            Some(p) => polygons.push(p),
            None => continue,
        };

        nodal_subdivisions.push(Subdivision::new_from_polygons(polygons));
        nodal_subd_idxs[flip.subdivision_one_idx] = num_nodal_subdivisions;
        num_nodal_subdivisions += 1;
    }

    (nodal_subdivisions, nodal_subd_idxs)
}

pub fn parse_input(lines: Vec<String>) -> (Vec<Subdivision>, Vec<Flip>, Vec<usize>) {
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

    (subdivisions, flips, subdivision_idxs)
}

pub fn maximal_polygon_classes() {
    let lattice_polygons = LatticePolygon::parse_many();
    let mut lattice_polygon_map: HashMap<String, LatticePolygon> = HashMap::new();

    for lp in lattice_polygons {
        let interior = lp.interior_points();
        lattice_polygon_map
            .entry(interior.to_ordered_string())
            .or_insert(lp);
    }

    for (ilp, lp) in lattice_polygon_map {
        println!("interior points: {}\nlattice polygon: {}\n", ilp, lp,);
    }
}

pub fn skeleton_classes(nontroplanar: String, nodes: usize, fname: String) {
    let fopen = fs::File::open(fname).unwrap();
    let buf = BufReader::new(fopen);

    let (mut subdivisions, flips, mut subdivision_idxs) = crate::utils::parse_input(
        buf.lines()
            .map(|l| l.expect("Could not parse line"))
            .collect(),
    );

    for _ in 0..nodes {
        let res =
            crate::utils::apply_flips(&subdivisions, &flips, &subdivision_idxs, subdivisions.len());

        subdivisions = res.0;
        subdivision_idxs = res.1;
    }

    let mut buckets: HashMap<String, Vec<crate::graph::Graph>> = HashMap::new();

    let filter = crate::utils::parse_nonplanar_hashes(&nontroplanar);

    for subd in subdivisions {
        let graph = crate::graph::Graph::from_subdivision(&subd);
        let mut skeletonized_graph = graph.skeletonize(&subd);
        let key = skeletonized_graph.categorizing_hash();

        match filter {
            Some(ref filter) => {
                let insert = filter.contains(&key);
                if insert {
                    if let Some(v) = buckets.get_mut(&key) {
                        if v.iter().all(|g| !g.is_isomorphic(&skeletonized_graph)) {
                            v.push(skeletonized_graph);
                        }
                    } else {
                        buckets.insert(key, vec![skeletonized_graph]);
                    };
                }
            }
            None => {
                if let Some(v) = buckets.get_mut(&key) {
                    if v.iter().all(|g| !g.is_isomorphic(&skeletonized_graph)) {
                        v.push(skeletonized_graph);
                    }
                } else {
                    buckets.insert(key, vec![skeletonized_graph]);
                };
            }
        }
    }

    // prints all graphs up to isomorphism grouped by (Loops:Bridges:Bi-Edges:Sprawling)
    for (k, v) in buckets.iter() {
        println!("Loops:Bridges:Bi-Edges:Sprawling");
        println!("{k}");
        for g in v {
            println!("{}", g);
        }
        println!();
    }
}
