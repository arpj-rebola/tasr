#![feature(generic_associated_types)]
#![feature(map_first_last)]
#![feature(maybe_uninit_ref)]
#![feature(ptr_offset_from)]
#![feature(test)]
#![allow(non_upper_case_globals)]
#![allow(incomplete_features)]
#![allow(dead_code)]
#![allow(unused_macros)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
pub mod io;
pub mod textparser;
pub mod basic;
pub mod database;
pub mod substitution;
pub mod model;
pub mod idflags;
pub mod idmap;
pub mod formula;
pub mod display;
pub mod checkerdb;
pub mod clauseset;
pub mod buffer;
pub mod chain;
pub mod proof;
pub mod tempfile;
pub mod propagation;
pub mod integrity;
pub mod split;
pub mod trim;
pub mod correctness;
pub mod app;
