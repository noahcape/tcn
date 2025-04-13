use std::{
    fs,
    io::{BufRead, BufReader, Write},
};

use tcn;

/// An example of how to use `tcn` to compute all nodal subdivisions from all triangulations of a lattice polygon.
/// Here we parse the triangulations and the associated flips with generated the triangulations.
/// Every flip poses the opprotunity to add a unit parallelogram to a subdivision.
///
/// Require genus to be greater than 3.
/// Only compute nodal subdivisions dual to at least genus 3 skeletons.
fn main() {
    let genus = 4;
    let out = String::from("./example_out/tcn_genus4.out");
    let tfile = String::from("./example_input/genus4_triangulation.txt");
    let vfile = String::from("./example_input/genus4_maximal_polygon_vertices.txt");

    // ensure we have correct genus as input
    assert!(genus > 3 && genus < 9);

    let filename = std::path::Path::new(&tfile)
        .file_name()
        .unwrap()
        .to_str()
        .unwrap();
    let file = match fs::File::open(tfile.clone()) {
        Ok(f) => f,
        Err(_) => panic!("failed to open file {tfile}"),
    };
    let fbuf = BufReader::new(file);

    println!("Parsing vertices from {}", vfile);
    let vertices = match tcn::polygon::Point::parse_many(vfile.clone()) {
        Some(vs) => vs,
        None => panic!("Cannot parse the vertice file {vfile}"),
    };

    println!("Parsing triangulations and flips from {}", tfile);
    let (mut subdivisions_m, flips) = tcn::utils::parse_input(
        fbuf.lines()
            .map(|l| l.unwrap_or_else(|_| panic!("Could not parse line from {tfile}")))
            .collect(),
    );

    let nodes = genus - 3;

    for node in 1..=nodes {
        let genus_str = format!("genus{}", genus - node);
        println!(
            "Computing genus {} graphs of {} triangulations with {} node(s)",
            genus - node,
            tfile,
            node
        );

        let out_dir = format!("./{out}/{genus_str}");
        match fs::DirBuilder::new().recursive(true).create(&out_dir) {
            Ok(_) => (),
            Err(_) => panic!("Failed out create dir {out}"),
        }

        let mut out_f = match fs::OpenOptions::new()
            .create(true)
            .write(true)
            .open(format!("{out}/{genus_str}/from_{filename}"))
        {
            Ok(f) => f,
            Err(e) => {
                panic!("Failed out create file ./{out}/{genus_str}/from_{filename} -- {e}")
            }
        };

        subdivisions_m = tcn::subdivision::remove_flipped_edges(&subdivisions_m, &flips, &vertices);

        let mut buckets: std::collections::HashMap<
            String,
            Vec<(tcn::graph::Graph, tcn::subdivision::Subdivision)>,
        > = std::collections::HashMap::new();

        for subd in subdivisions_m.values().flatten() {
            let mut skeletonized_graph = tcn::graph::Graph::new_skeleton(subd);
            let key = skeletonized_graph.categorizing_hash();

            if let Some(v) = buckets.get_mut(&key) {
                if v.iter().all(|(g, _)| !g.is_isomorphic(&skeletonized_graph)) {
                    v.push((skeletonized_graph, subd.clone()));
                }
            } else {
                buckets.insert(key, vec![(skeletonized_graph, subd.clone())]);
            };
        }

        // write all graphs up to isomorphism grouped by (Loops:Bridges:Bi-Edges:Sprawling)
        for (key, graphs) in buckets.iter() {
            match out_f.write_fmt(format_args!("{}\n", key)) {
                Ok(_) => (),
                Err(_) => {
                    panic!("Failed to write key {key} to file {out}/{genus_str}!")
                }
            };
            for (graph, subd) in graphs {
                match out_f.write_fmt(format_args!("Skeleton: {}\nSubdivision: {}\n", graph, subd))
                {
                    Ok(_) => (),
                    Err(_) => {
                        panic!("Failed to write graph {graph} to file {out}/{genus_str}!")
                    }
                };
            }
            match out_f.write_all(b"\n") {
                Ok(_) => (),
                Err(_) => panic!("Failed to write to file {out}/{genus_str}!"),
            };
        }
    }
}
