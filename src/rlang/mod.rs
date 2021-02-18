#[allow(dead_code, unused_macros)]
#[macro_use]
mod rmacros;
pub use rmacros::*;

#[allow(dead_code)]
mod rprog;
pub use rprog::{Expr, REnv, RProgram};

#[allow(dead_code)]
mod rrandp;
pub use rrandp::{randp, RandEnv};

#[allow(dead_code)]
mod ropt;
pub use ropt::Opt;

#[allow(dead_code)]
mod uniquify;
pub use uniquify::Uniquify;

#[allow(dead_code)]
mod resolve_complex;
