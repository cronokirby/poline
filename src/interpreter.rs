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

impl ProgramIO for &mut RealProgramIO {
    fn print(&mut self, message: &str) {
        print!("{}", message);
    }
}

/// This holds all the context that the interpreter needs.
struct Interpreter<'prg, I> {
    io: I,
    program: &'prg Program,
    stack: Vec<String>,
}


impl<'prg, I> Interpreter<'prg, I> {
    fn new(io: I, program: &'prg Program) -> Self {
        Self {
            io,
            program,
            stack: Vec::new(),
        }
    }
}

impl<'prg, I: ProgramIO> Interpreter<'prg, I> {
    fn print_argument(&mut self, arg: Argument) {
        match arg {
            Argument::Str(index) => self.io.print(self.program.string_table.get(index)),
            Argument::Name(index) => self.io.print(&self.stack[index.0 as usize]),
        }
    }

    fn statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Recv => panic!("Unimplemented statement: Recv"),
            Statement::Send(_, _) => panic!("Unimplemented statement: Send"),
            Statement::Call(_) => panic!("Unimplemented statement: Call"),
            Statement::Spawn(_) => panic!("Unimplemented statement: Spawn"),
            Statement::Print(arg) => self.print_argument(*arg),
        }
    }

    fn function(&mut self, index: StackIndex) {
        // Simplification has made sure that functions are defined.
        let declaration = &self.program.functions[index.0 as usize];
        for statement in &declaration.body {
            self.statement(statement);
        }
    }

    fn main_function(&mut self) {
        self.function(self.program.main_function);
    }
}

/// Start running the contents of a program.
///
/// This is the main entrance point for the interpreter.
pub fn interpret<I: ProgramIO>(io: I, program: Program) {
    let mut interpreter = Interpreter::new(io, &program);
    interpreter.main_function();
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{parser, simplifier};

    #[derive(Debug, PartialEq)]
    struct VecProgramIO<'v> {
        prints: &'v mut Vec<String>
    }

    impl <'v> ProgramIO for VecProgramIO<'v> {
        fn print(&mut self, message: &str) {
            self.prints.push(message.into());
        }
    }

    fn run_panic(source: &str) -> Vec<String> {
        let syntax = parser::collect_errors_and_parse(source).unwrap();
        let program = simplifier::simplify(syntax).unwrap();
        let mut prints = Vec::new();
        let io = VecProgramIO { prints: &mut prints };
        interpret(io, program);
        prints
    }

    #[test]
    fn interpreter_works_on_print() {
        let source = "fn main() { print \"foo\"; }";
        let output = run_panic(source);
        let expected = vec!["foo"];
        assert_eq!(output, expected);
    }
}
