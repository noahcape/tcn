use std::{collections::HashMap, fs::File, io::Write};

use tcn::{self, polygon::LatticePolygon};

/// Given an enumeration of all convex lattice polygons of genus g, compute the set of maximal nonhyperelliptic polygons of that genus.
fn main() {
    let compute_statistics = false;

    let mut outf = File::create("./maximal_polygons_1to25.txt").expect("Couldn't open file");

    for g in 1..=25 {
        match outf.write_fmt(format_args!("Maximal Polygons of genus {}:\n", g)) {
            Ok(_) => (),
            Err(_) => {
                panic!("Failed to write to file");
            }
        };

        let polygon_file = String::from(format!("./lattice_polygons_all/genus_{}.txt", g));

        let lattice_polygons = match tcn::polygon::LatticePolygon::parse_many(polygon_file) {
            Ok(ls) => ls,
            Err(e) => panic!("Error {e}"),
        };

        let mut lattice_polygon_map: HashMap<String, tcn::polygon::LatticePolygon> = HashMap::new();

        for lp in lattice_polygons {
            let interior = lp.interior_points(false);
            let lp_area = lp.area_picks_interior(&interior);

            if !lp.is_hyperelliptic(&interior) {
                match lattice_polygon_map.entry(interior.to_ordered_string()) {
                    std::collections::hash_map::Entry::Occupied(mut map_lp) => {
                        if map_lp.get().area_picks() < lp_area {
                            map_lp.insert(lp.boundary_points().join_polygons(interior));
                        }
                    }
                    std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(lp.boundary_points().join_polygons(interior));
                    }
                }
            }
        }

        let mut max_boundary_pts: (usize, Option<LatticePolygon>) = (0, None);
        let mut num_per_interior: Vec<usize> = vec![0; 25];

        for (_, lp) in lattice_polygon_map {
            if compute_statistics {
                let boundary_points = lp.num_boundary_points();
                let nth_interior_defined = lp.maximal_interior(true);

                if boundary_points > max_boundary_pts.0 {
                    max_boundary_pts = (boundary_points, Some(lp.clone()));
                }
                num_per_interior[nth_interior_defined] += 1;
            }

            match outf.write_fmt(format_args!("{}\n", lp.to_ordered_string())) {
                Ok(_) => (),
                Err(_) => {
                    panic!("Failed to write to file");
                }
            };
        }

        if compute_statistics {
            println!("Genus {}", g);
            if max_boundary_pts.1.is_some() {
                println!(
                    "Maximum Number of boundary points: {} from {}\n",
                    max_boundary_pts.0,
                    max_boundary_pts.1.unwrap().order_points()
                );
            }

            println!(
                "Number per interior shell defined: {:?}\n",
                num_per_interior
            );
        }

        match outf.write_all(b"\n") {
            Ok(_) => (),
            Err(_) => {
                panic!("Failed to write to file");
            }
        };
    }
}
