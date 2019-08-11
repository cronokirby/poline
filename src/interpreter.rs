use crate::simplifier::*;

/// Represents the side effects poline programs need access to.
///
/// This trait is useful in order to test the interpreter by running against mock
/// side effects.
pub trait ProgramIO {
    /// Print a message out to the world.
    fn print(&mut self, message: &str);
}

/// This struct implements the ProgramIO trait by actually performing side effects.
///
/// This is the main struct to use in practice. This struct doesn't actually contain
/// any data, it simply acts as a marker for the right trait implementation.
#[derive(Clone, Copy, Debug)]
pub struct RealProgramIO;

impl ProgramIO for RealProgramIO {
    fn print(&mut self, message: &str) {
        print!("{}", message);
    }
}

/// Start running the contents of a program.
///
/// This is the main entrance point for the interpreter.
pub fn interpret<I: ProgramIO>(io: &mut I, program: Program) {}
