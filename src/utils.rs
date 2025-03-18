use std::collections::{HashMap, HashSet};
use std::fs::{self, read_to_string};
use std::io::{BufRead, BufReader, Write};

use crate::polygon::*;
use crate::subdivision::*;

/// Parse nontroplanar file to filter out troplanar graphs
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

/// Appply flips to subdivisions to get subdivision for n+1 unit parallelograms
pub fn apply_flips(
    subdivisions_m: &HashMap<usize, Vec<Subdivision>>,
    flips: &Vec<Flip>,
    vertices: &[Point],
) -> HashMap<usize, Vec<Subdivision>> {
    let mut nodal_subdivisions_m: HashMap<usize, Vec<Subdivision>> = HashMap::new();

    // Flip edge in both triangulation one and two - as long as it is possible
    for flip in flips {
        let flipped_subdivisions = match flip.set_up_flip(subdivisions_m) {
            Some(flippable_subdivisions) => {
                match flip.apply_flip(&flippable_subdivisions, vertices) {
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

/// Parse TOPCOM data flips and triangulations
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

/// Parse from std::in
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

pub fn skeleton_classes(
    // don't use this for now
    // nontroplanar_dir: String,
    genus: usize,
    out: String,
    tfile: String,
    vfile: String,
) -> Result<(), String> {
    // enusre we have correct genus as input
    assert!(genus > 3 && genus < 9);

    let filename = std::path::Path::new(&tfile)
        .file_name()
        .unwrap()
        .to_str()
        .unwrap();
    let file = match fs::File::open(tfile.clone()) {
        Ok(f) => f,
        Err(_) => return Err(format!("failed to open file {tfile}")),
    };
    let fbuf = BufReader::new(file);

    println!("Parsing vertices from {}", vfile);
    let vertices = match crate::polygon::Point::parse_many(vfile.clone()) {
        Some(vs) => vs,
        None => return Err(format!("Cannot parse the vertice file {vfile}")),
    };

    println!("Parsing triangulations and flips from {}", tfile);
    let (mut subdivisions_m, flips) = crate::utils::parse_input(
        fbuf.lines()
            .map(|l| l.unwrap_or_else(|_| panic!("Could not parse line from {tfile}")))
            .collect(),
    );

    let nodes = genus - 3;

    for node in 1..=nodes {
        let genus_str = format!("genus{}", genus - node);
        // let nontroplanar_file = format!("{}/{genus_str}.txt", nontroplanar_dir);
        println!(
            "Computing genus {} graphs of {} triangulations with {} node(s)",
            genus - node,
            tfile,
            node
        );

        let out_dir = format!("./{out}/{genus_str}");
        match fs::DirBuilder::new().recursive(true).create(&out_dir) {
            Ok(_) => (),
            Err(_) => return Err(format!("Failed out create dir {out}")),
        }

        let mut out_f = match fs::OpenOptions::new()
            .create(true)
            .write(true)
            .open(format!("{out}/{genus_str}/from_{filename}"))
        {
            Ok(f) => f,
            Err(e) => {
                return Err(format!(
                    "Failed out create file ./{out}/{genus_str}/from_{filename} -- {e}"
                ))
            }
        };

        subdivisions_m = crate::utils::apply_flips(&subdivisions_m, &flips, &vertices);

        let mut buckets: std::collections::HashMap<
            String,
            Vec<(crate::graph::Graph, crate::subdivision::Subdivision)>,
        > = std::collections::HashMap::new();

        // for now keep filter out
        // let filter = match crate::utils::parse_nonplanar_hashes(&nontroplanar_file) {
        //     Some(f) => f,
        //     None => return Err(format!("Could not read file {}", nontroplanar_file)),
        // };

        for subd in subdivisions_m.values().flatten() {
            let mut skeletonized_graph = crate::graph::Graph::new_skeleton(subd);
            let key = skeletonized_graph.categorizing_hash();

            // let insert = filter.contains(&key);
            // if insert {
            if let Some(v) = buckets.get_mut(&key) {
                if v.iter().all(|(g, _)| !g.is_isomorphic(&skeletonized_graph)) {
                    v.push((skeletonized_graph, subd.clone()));
                }
            } else {
                buckets.insert(key, vec![(skeletonized_graph, subd.clone())]);
            };
            // }
        }

        // write all graphs up to isomorphism grouped by (Loops:Bridges:Bi-Edges:Sprawling)
        for (key, graphs) in buckets.iter() {
            match out_f.write_fmt(format_args!("{}\n", key)) {
                Ok(_) => (),
                Err(_) => {
                    return Err(format!(
                        "Failed to write key {key} to file {out}/{genus_str}!"
                    ))
                }
            };
            for (graph, subd) in graphs {
                match out_f.write_fmt(format_args!("Skeleton: {}\nSubdivision: {}\n", graph, subd))
                {
                    Ok(_) => (),
                    Err(_) => {
                        return Err(format!(
                            "Failed to write graph {graph} to file {out}/{genus_str}!"
                        ))
                    }
                };
            }
            match out_f.write_all(b"\n") {
                Ok(_) => (),
                Err(_) => return Err(format!("Failed to write to file {out}/{genus_str}!")),
            };
        }
    }

    Ok(())
}
