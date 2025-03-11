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

    /// Number of interior points polygon triangulated
    #[arg(short, long)]
    genus: usize,

    /// topcom file
    #[arg(short, long)]
    topcomf: String,

    /// out dir
    #[arg(short, long)]
    out: String,

    /// vertices for Newton polygon
    #[arg(short, long)]
    vfile: String,
}

fn main() {
    let args = Args::parse();
    // **computing maximal lattice polygons interior structure**
    // utils::maximal_polygon_classes();

    // **computing skeletons**
    match utils::skeleton_classes(
        // args.nontroplanar,
        args.genus,
        args.out,
        args.topcomf,
        args.vfile,
    ) {
        Ok(_) => (),
        Err(e) => eprintln!("{}", e),
    }
}
