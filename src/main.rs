use clap::Parser;

pub mod graph;
pub mod polygon;
pub mod subdivision;
pub mod utils;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// File containing nontroplanar graphs to keep track of
    #[arg(short, long, default_value = "")]
    nontroplanar: String,
}

fn main() {
    // **computing maximal lattice polygons interior structure**
    // utils::maximal_polygon_classes();

    // **computing skeletons**
    // utils::skeleton_classes(Args::parse().nontroplanar, 0);
}
