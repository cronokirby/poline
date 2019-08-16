use crate::simplifier::*;
use std::collections::VecDeque;

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
///
/// This struct implements the trait by actually printing to stdout. Each message
/// has a newline added to it. This is similar to other dynamic languages like python.
#[derive(Clone, Copy, Debug)]
pub struct RealProgramIO;

impl ProgramIO for &mut RealProgramIO {
    fn print(&mut self, message: &str) {
        println!("{}", message);
    }
}

/// Represents a reference to a given thread.
///
/// This contains information about where we've located the thread in memory,
/// and provides a unique identifier for sending things to this thread. We don't
/// want to confuse the two to prevent a thread receiving a message
#[derive(Clone, Copy, Debug)]
struct ThreadRef {
    /// This is the slot the thread happens to occupy.
    slot: usize,
    /// This identifies the thread uniquely.
    ///
    /// For now this is intended to increment with every new thread. The first thread
    /// spawned gets `logical: 0`, the next `logical: 1`, and so on.
    logical: u32,
}

/// Represents a variable in the program.
///
/// These are the actual values that get pushed to the stack and passed around.
#[derive(Clone, Copy, Debug)]
enum Variable {
    /// A reference to a string litteral.
    Str(StringIndex),
    /// A reference to another spawned thread.
    Thread(ThreadRef),
    /// An undefined variable.
    ///
    /// This happens when a function is missing arguments. When not enough arguments
    /// are passed to a function, the rest are filled in with this branch.
    Undefined,
}

/// This holds the variables living in a function.
///
/// The initial arguments to a function are passed to its stack, and further introduction
/// of arguments create new elements on its stack.
#[derive(Debug)]
struct Stack(Vec<Variable>);

impl Stack {
    fn push(&mut self, var: Variable) {
        self.0.push(var);
    }
}

/// This holds the different variables send to a given thread.
///
/// Unlike a stack, this is used across all function invocations for a given thread.
#[derive(Debug)]
struct Mailbox(VecDeque<Variable>);

impl Mailbox {
    fn new() -> Self {
        Self(VecDeque::new())
    }

    fn send(&mut self, var: Variable) {
        self.0.push_front(var);
    }

    fn recv(&mut self) -> Option<Variable> {
        self.0.pop_back()
    }
}

/// This hold information about where we are in the execution of a function.
#[derive(Debug)]
struct FunctionState {
    /// The index of the statement we're at in the function.
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

    fn push(&mut self, var: Variable) {
        self.stack.push(var);
    }
}

/// This struct holds the state for a given thread.
///
/// In addition to holding the nested stacks, this holds the information about where
/// in each function call we are, so we can resume interpreting a function after ending
/// a call. This also holds the mailbox for this thread.
#[derive(Debug)]
struct Frames {
    /// This is the actual layer of Function calls so far.
    ///
    /// Each call holds information so that we can resume execution after popping the
    /// latest function off of the stack.
    calls: Vec<FunctionState>,
    /// The mailbox containing all variables sent to this thread
    mailbox: Mailbox,
}

impl Frames {
    fn new() -> Self {
        Self {
            calls: Vec::new(),
            mailbox: Mailbox::new(),
        }
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

    fn push(&mut self, var: Variable) {
        let last = self.calls.len() - 1;
        self.calls[last].push(var);
    }

    fn send(&mut self, var: Variable) {
        self.mailbox.send(var)
    }

    fn recv(&mut self) -> Option<Variable> {
        self.mailbox.recv()
    }
}

/// This enum represents the state of a thread.
///
/// A thread is a logicial unit of execution in the interpreter.
///
/// A thread is either somewhere in a function, or stopped, after reaching the end of
/// its function calls.
#[derive(Debug)]
enum ThreadState {
    /// This thread is running with a certain state in its stack frames.
    Running { blocked: bool, frames: Frames },
    /// This thread is done, after having run through all its stack frames.
    ///
    /// Once a thread enters this state, it never starts running again.
    ///
    /// We do use this state to replace finished threads with new threads. These
    /// new threads will share the same thread index as the old one.
    Done,
}

impl ThreadState {
    fn is_done(&self) -> bool {
        match self {
            ThreadState::Running { .. } => false,
            ThreadState::Done => true,
        }
    }

    fn running(frames: Frames) -> Self {
        ThreadState::Running {
            blocked: false,
            frames,
        }
    }

    // We handle this here for unblocking logic
    fn send(&mut self, var: Variable) {
        match self {
            ThreadState::Running {
                ref mut blocked,
                frames,
            } => {
                frames.send(var);
                *blocked = false;
            }
            ThreadState::Done => {} // TODO: Maybe warn or something
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
    /// This is the logical identifier the next spawned thread will use.
    ///
    /// We use the *next* identifier instead of the *last* identifier to have
    /// a sensible starting state. If we used the *last* identifier, we'd have
    /// to start at -1.
    ///
    /// This field is useful to guarantee that each thread has a unique increasing ID.
    /// We use the increasing property of our IDs to easily figure out if a thread
    /// is dead.
    next_logical: u32,
    /// This holds the list of thread states that currently exist.
    ///
    /// We try and reuse slots that have finished executing.
    threads: Vec<ThreadState>,
}

impl Threads {
    fn new() -> Self {
        Self {
            current_thread: 0,
            next_logical: 0,
            threads: Vec::new(),
        }
    }

    fn call<'prg>(&mut self, program: &'prg Program, function: StackIndex, args: Vec<Variable>) {
        match &mut self.threads[self.current_thread] {
            ThreadState::Running { frames, .. } => frames.call(program, function, args),
            ThreadState::Done => panic!(
                "Tried to call function on done thread #{}",
                self.current_thread
            ),
        }
    }

    fn spawn<'prg>(
        &mut self,
        program: &'prg Program,
        function: StackIndex,
        args: Vec<Variable>,
    ) -> ThreadRef {
        let mut frames = Frames::new();
        frames.call(program, function, args);
        let new_thread = ThreadState::Running {
            blocked: false,
            frames,
        };
        let first_done = (0..self.threads.len()).find(|&i| self.threads[i].is_done());
        let slot = match first_done {
            None => {
                self.threads.push(new_thread);
                self.threads.len() - 1
            }
            Some(i) => {
                self.threads[i] = new_thread;
                i
            }
        };
        let logical = self.next_logical;
        self.next_logical += 1;
        ThreadRef { slot, logical }
    }

    // This will return None when all threads have finished executing.
    fn next_stmt<'prg>(&mut self, program: &'prg Program) -> Option<&'prg Statement> {
        let thread_count = self.threads.len();
        // We loop over every index, started with the current and wrapping around.
        for _ in 0..thread_count {
            let next_thread = &mut self.threads[self.current_thread];
            if let ThreadState::Running {
                blocked: false,
                frames,
            } = next_thread
            {
                let next_stmt = frames.next_stmt(program);
                if next_stmt.is_some() {
                    return next_stmt;
                } else {
                    *next_thread = ThreadState::Done;
                }
            }

            self.current_thread += 1;
            if self.current_thread >= thread_count {
                self.current_thread -= thread_count;
            }
        }
        None
    }

    // This assumes we're not at the end of the thread calls
    fn get_var(&self, index: StackIndex) -> Variable {
        match &self.threads[self.current_thread] {
            ThreadState::Running { frames, .. } => frames.get_var(index),
            ThreadState::Done => panic!(
                "Tried to get variable #{} on done Thread#{}",
                index.0, self.current_thread
            ),
        }
    }

    fn push(&mut self, var: Variable) {
        match &mut self.threads[self.current_thread] {
            ThreadState::Running { frames, .. } => frames.push(var),
            ThreadState::Done => panic!(
                "Tried to push variable #{:?} on done Thread#{}",
                var, self.current_thread
            ),
        }
    }

    /// This receives for the current thread.
    fn recv(&mut self) {
        match self.threads[self.current_thread] {
            ThreadState::Done => panic!("Tried to recv on done Thread#{}", self.current_thread),
            ThreadState::Running { blocked: true, .. } => {}
            ThreadState::Running {
                ref mut blocked,
                ref mut frames,
            } => match frames.recv() {
                Some(already) => frames.push(already),
                None => *blocked = true,
            },
        }
    }

    fn send(&mut self, var: Variable, to: ThreadRef) {
        self.threads[to.slot].send(var);
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
                match self.threads.get_var(index) {
                    Variable::Str(index) => self.io.print(self.program.string_table.get(index)),
                    Variable::Thread(ThreadRef { logical, .. }) => {
                        self.io.print(&format!("Thread#{}", logical));
                    }
                    Variable::Undefined => self.io.print("undefined"),
                };
            }
        }
    }

    fn statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Recv => self.threads.recv(),
            Statement::Send(arg, index) => {
                let thread = match self.threads.get_var(*index) {
                    Variable::Thread(t) => t,
                    v => {
                        let warning = format!("Warning: tried to send {:?} to {:?}", arg, v);
                        return self.io.print(&warning);
                    }
                };
                let var = match *arg {
                    Argument::Str(index) => Variable::Str(index),
                    Argument::Name(index) => self.threads.get_var(index),
                };
                self.threads.send(var, thread);
            }
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
                let thread = self.threads.spawn(self.program, call.name, vars);
                self.threads.push(Variable::Thread(thread));
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

    #[test]
    fn interpreter_can_print_spawned_threads() {
        let source = "fn foo() {} fn main() { spawn foo() as p; print p; }";
        let output = run_panic(source);
        let expected = vec!["Thread#1"];
        assert_eq!(output, expected);
    }

    #[test]
    fn threads_dont_reuse_identifiers() {
        let source =
            "fn c() {} fn b() { spawn c() as p; print p; } fn main() { spawn b() as p; print p; }";
        let output = run_panic(source);
        let expected = vec!["Thread#1", "Thread#2"];
        assert_eq!(output, expected);
    }

    #[test]
    fn interpreter_can_handle_basic_send_and_recieve() {
        let source = "fn p() { recv x; print x; } fn main() { spawn p() as p; send \"2\" to p; print \"1\"; }";
        let output = run_panic(source);
        let expected = vec!["1", "2"];
        assert_eq!(output, expected);
    }
}
