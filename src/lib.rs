#![warn(missing_docs)]

//! Library for computing tropical crossing number (tcn) as well as computing maximal lattice polygons.
//!
//! This software is mean to be used downstream of current methods.
//! For computing tcn, it reads in a triangulation file from TOPCOM.
//! The exact triangulation is computing with the following command.
//!     ```
//!         points2triangs --flips --unimodular --regular --affinesymmetries -i <polygon-file>
//!     ```
//!
//! For computing maximal lattice polygons, the testing was done on all polygons up to genus 31 from Castryck (2012).
//!
//! This work was completed for my senior undergraduate thesis titled "Tropical Crossing Number of Finite Graphs"

/// Module for computing the skeleton from a nodal subdivision
pub mod graph;

/// Module containing functions to compute over sets of points defining polygons in the plane
pub mod polygon;

/// Module with methods to manipulate subdivision of lattice polygons
pub mod subdivision;

/// Module containing utility functions for parsing input
pub mod utils;
