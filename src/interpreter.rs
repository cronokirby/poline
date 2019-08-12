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

/// Represents a variable in the program.
///
/// These are the actual values that get pushed to the stack and passed around.
#[derive(Clone, Copy, Debug)]
enum Variable {
    /// A reference to a string litteral.
    Str(StringIndex),
    /// An undefined variable.
    ///
    /// This happens when a function is missing arguments.
    Undefined,
}

/// This holds the variables living in a function.
///
/// The initial arguments to a function are passed to its stack, and further introduction
/// of arguments create new elements on its stack.
struct Stack(Vec<Variable>);

impl Stack {
    fn push(&mut self, var: Variable) {
        self.0.push(var);
    }
}

/// This hold information about where we are in the execution of a function.
struct FunctionState {
    /// The index of the declaration we're at in the function.
    statement: usize,
    /// The original declaration we're interpreting.
    function: StackIndex,
    /// The stack containing variables used in this function.
    stack: Stack,
}

impl FunctionState {
    fn from_args<'prg>(prg: &'prg Program, function: StackIndex, mut args: Vec<Variable>) -> Self {
        let arg_count = prg.functions[function.0 as usize].arg_count;
        args.resize(arg_count as usize, Variable::Undefined);
        FunctionState {
            statement: 0,
            function,
            stack: Stack(args),
        }
    }

    fn next<'prg>(&mut self, prg: &'prg Program) -> Option<&'prg Statement> {
        let declaration = &prg.functions[self.function.0 as usize];
        let res = declaration.body.get(self.statement);
        if res.is_some() {
            self.statement += 1;
        }
        res
    }

    fn get_var(&self, index: StackIndex) -> Variable {
        self.stack.0[index.0 as usize]
    }
}

/// This struct holds the information about where we all in function calls.
///
/// In addition to holding the nested stacks, this holds the information about where
/// in each function call we are, so we can resume interpreting a function after ending
/// a call.
struct Frames {
    /// This is the actual layer of Function calls so far.
    ///
    /// Each call holds information so that we can resume execution after popping the
    /// latest function off of the stack.
    calls: Vec<FunctionState>,
}

impl Frames {
    fn new() -> Self {
        Self { calls: Vec::new() }
    }

    fn call<'prg>(&mut self, program: &'prg Program, function: StackIndex, args: Vec<Variable>) {
        let function = FunctionState::from_args(program, function, args);
        self.calls.push(function);
    }

    // This will return None when we've reached the end of the root function call.
    fn next_stmt<'prg>(&mut self, program: &'prg Program) -> Option<&'prg Statement> {
        loop {
            if self.calls.is_empty() {
                return None;
            }
            let last = self.calls.len() - 1;
            match self.calls[last].next(program) {
                None => {
                    self.calls.pop();
                }
                Some(stmt) => return Some(stmt),
            }
        }
    }

    // This assumes we're not at the end of the function calls
    fn get_var(&self, index: StackIndex) -> Variable {
        let last = self.calls.len() - 1;
        self.calls[last].get_var(index)
    }
}

/// This enum represents the state of a thread.
///
/// A thread is a logicial unit of execution in the interpreter.
///
/// A thread is either somewhere in a function, or stopped, after reaching the end of
/// its function calls.
enum ThreadState {
    /// This thread is running with a certain state in its stack frames.
    Running(Frames),
    /// This thread is done, after having run through all its stack frames.
    Done,
}

impl ThreadState {
    fn is_done(&self) -> bool {
        match self {
            ThreadState::Running(_) => false,
            ThreadState::Done => true,
        }
    }
}

/// This holds the information about where the execution of each thread is.
///
/// We can use this to keep track of our current position in the various threads,
/// and automatically fetch the next statement to execute.
struct Threads {
    /// This holds the index of the current state we're working on.
    current_thread: usize,
    /// This holds the list of thread states that currently exist.
    ///
    /// We try and reuse slots that have finished executing.
    threads: Vec<ThreadState>,
}

impl Threads {
    fn new() -> Self {
        Self {
            current_thread: 0,
            threads: Vec::new(),
        }
    }

    fn call<'prg>(&mut self, program: &'prg Program, function: StackIndex, args: Vec<Variable>) {
        match &mut self.threads[self.current_thread] {
            ThreadState::Running(frames) => frames.call(program, function, args),
            ThreadState::Done => panic!(
                "Tried to call function on done thread #{}",
                self.current_thread
            ),
        }
    }

    fn spawn<'prg>(&mut self, program: &'prg Program, function: StackIndex, args: Vec<Variable>) {
        let mut frames = Frames::new();
        frames.call(program, function, args);
        let new_thread = ThreadState::Running(frames);
        let first_done = self.threads.iter_mut().find(|x| x.is_done());
        match first_done {
            None => self.threads.push(new_thread),
            Some(thread) => *thread = new_thread,
        }
    }

    // This will return None when all threads have finished executing.
    fn next_stmt<'prg>(&mut self, program: &'prg Program) -> Option<&'prg Statement> {
        let start_thread = self.current_thread;
        let thread_count = self.threads.len();
        // We loop over every index, started with the current and wrapping around.
        for i in start_thread..start_thread + thread_count {
            self.current_thread = i;
            if i >= thread_count {
                self.current_thread -= start_thread;
            }
            if let ThreadState::Running(frames) = &mut self.threads[self.current_thread] {
                let next_stmt = frames.next_stmt(program);
                if next_stmt.is_some() {
                    return next_stmt;
                }
            }
        }
        None
    }

    // This assumes we're not at the end of the thread calls
    fn get_var(&self, index: StackIndex) -> Variable {
        match &self.threads[self.current_thread] {
            ThreadState::Running(frames) => frames.get_var(index),
            ThreadState::Done => panic!(
                "Tried to get variable #{} on done thread #{}",
                index.0, self.current_thread
            ),
        }
    }
}

/// This holds all the context that the interpreter needs.
struct Interpreter<'prg, I> {
    io: I,
    program: &'prg Program,
    threads: Threads,
}

impl<'prg, I> Interpreter<'prg, I> {
    fn new(io: I, program: &'prg Program) -> Self {
        Self {
            io,
            program,
            threads: Threads::new(),
        }
    }
}

impl<'prg, I: ProgramIO> Interpreter<'prg, I> {
    fn print_argument(&mut self, arg: Argument) {
        match arg {
            Argument::Str(index) => self.io.print(self.program.string_table.get(index)),
            Argument::Name(index) => {
                let msg = match self.threads.get_var(index) {
                    Variable::Str(index) => self.program.string_table.get(index),
                    Variable::Undefined => &"undefined",
                };
                self.io.print(msg)
            }
        }
    }

    fn statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Recv => panic!("Unimplemented statement: Recv"),
            Statement::Send(_, _) => panic!("Unimplemented statement: Send"),
            Statement::Call(call) => {
                let mut vars = Vec::new();
                for arg in &call.args {
                    let var = match *arg {
                        Argument::Str(index) => Variable::Str(index),
                        Argument::Name(index) => self.threads.get_var(index),
                    };
                    vars.push(var);
                }
                self.threads.call(self.program, call.name, vars);
            }
            Statement::Spawn(call) => {
                let mut vars = Vec::new();
                for arg in &call.args {
                    let var = match *arg {
                        Argument::Str(index) => Variable::Str(index),
                        Argument::Name(index) => self.threads.get_var(index),
                    };
                    vars.push(var);
                }
                self.threads.spawn(self.program, call.name, vars);
            }
            Statement::Print(arg) => self.print_argument(*arg),
        }
    }

    fn main_function(&mut self) {
        self.threads
            .spawn(self.program, self.program.main_function, Vec::new());
        while let Some(stmt) = self.threads.next_stmt(self.program) {
            self.statement(stmt);
        }
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
        prints: &'v mut Vec<String>,
    }

    impl<'v> ProgramIO for VecProgramIO<'v> {
        fn print(&mut self, message: &str) {
            self.prints.push(message.into());
        }
    }

    fn run_panic(source: &str) -> Vec<String> {
        let syntax = parser::collect_errors_and_parse(source).unwrap();
        let program = simplifier::simplify(syntax).unwrap();
        let mut prints = Vec::new();
        let io = VecProgramIO {
            prints: &mut prints,
        };
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

    #[test]
    fn interpreter_can_call_functions() {
        let source = "fn p(a) { print a; } fn main() { p(\"A\"); p(\"B\"); }";
        let output = run_panic(source);
        let expected = vec!["A", "B"];
        assert_eq!(output, expected);
    }

    #[test]
    fn interpreter_can_call_functions_with_too_many_args() {
        let source = "fn p(a) { print a; } fn main() { p(\"A\", \"B\"); }";
        let output = run_panic(source);
        let expected = vec!["A"];
        assert_eq!(output, expected);
    }

    #[test]
    fn interpreter_can_call_functions_with_too_little_args() {
        let source = "fn p(a) { print a; } fn main() { p(); }";
        let output = run_panic(source);
        let expected = vec!["undefined"];
        assert_eq!(output, expected);
    }

    #[test]
    fn interpreter_can_spawn_functions() {
        let source = "fn p(a) { print a; } fn main() { spawn p(\"A\") as x; p(\"B\"); }";
        let output = run_panic(source);
        let expected = vec!["B", "A"];
        assert_eq!(output, expected);
    }
}