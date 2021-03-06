//! This module exists to provide utilities to simplify the parsed AST.
//!
//! The main reason this module exists is to clean up the representation
//! of our source code into a form more amenable to interpretation. This mainly
//! involves extracting out string litterals and changing variables to use
//! an index form instead of relying on string litterals.
use crate::parser;
use std::collections::HashMap;

/// Represents a De Bruijin-esque index, instead of a variable name.
///
/// For reference, see: https://en.wikipedia.org/wiki/De_Bruijn_index
///
/// A newtype is used, to avoid confusion with other u32 indices used, such
/// as indices in a string litteral table.
///
/// Note that stack indices are always relative to their position in a
/// function or program. For function declarations, these are "absolute",
/// since every function is in the top scope.
///
/// De Bruijn indices are commonly found in languages based off the lambda calculus,
/// in that scheme, the indices replace expressions like `\x.\y.x`, turning
/// them into `\.\.2`. The numbe references a variable, based on how many steps
/// back it was introduced.
///
/// Our interpreted language is simple enough to statically know when a
/// variable will be introduced in a function. The only way to introduce
/// a new variable is by listing function parameters, using the `recv`
/// statement, or using the `spawn` statement. Since variables are always
/// shadowed, this always introduces a new variable. Because of this, we
/// can replace variable names with positions in the variable stack.
///
/// # Example
/// ```
/// fn foo(a, b) {
///   recv x;
///   bar(a, b, x);
/// }
/// ```
///
/// The last call becomes `bar(0, 1, 2)` based on the positions of each variable
/// in the stack.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StackIndex(pub u32);

/// Represents an index into the string litteral table.
///
/// Instead of storing string litterals directly, we keep them an external
/// table, and use indices into this table in the AST instead. This makes
/// manipulating the AST a bit easier.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StringIndex(pub u32);

/// Represents an Argument, either as a string litteral or a variable.
///
/// In this module, instead of String names, we use de bruijin indices instead,
/// which always reference the nth variable on the stack when interpreting the stack.
/// String litterals reference the nth string in the litteral table.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Argument {
    /// Represents a reference to a named variable.
    Name(StackIndex),
    /// Represents a string litteral.
    Str(StringIndex),
}

/// Holds the information for a function call.
///
/// A function call is defined by the name of the function being called,
/// as well as the arguments being passed to the function.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionCall {
    /// The index giving us the name, and thus code, for the function.
    pub name: StackIndex,
    /// The arguments being passed to the function.
    pub args: Vec<Argument>,
}

/// Holds information defining a given statement.
#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    /// Represents printing an argument to the console.
    Print(Argument),
    /// Receive a value into a new variable.
    ///
    /// This statement introduces a new variable implicitly, the name of that
    /// variable has been eliminated at this stage.
    Recv,
    /// Represents spawning a function into a new thread.
    Spawn(FunctionCall),
    /// Send a given argument to a process, referenced by stack index.
    Send(Argument, StackIndex),
    /// Call a given function.
    Call(FunctionCall),
}

/// Holds information about a given function.
///
/// This information is sufficient to be able to call the given function.
#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclaration {
    /// The number of arguments a function takes.
    ///
    /// At this stage, the names of the variables have been eliminated.
    pub arg_count: u32,
    /// A sequence of statements making up the function.
    pub body: Vec<Statement>,
}

/// Represents the table of string litterals in our program.
///
/// We store all of the string litterals in the program in a single table,
/// to make the representation of the program a bit simpler.
#[derive(Clone, Debug, PartialEq)]
pub struct StringTable(Vec<String>);

impl StringTable {
    fn new() -> Self {
        StringTable(Vec::new())
    }

    /// We take ownership of the String because we need
    /// to store the data permanently.
    fn insert(&mut self, s: String) -> StringIndex {
        self.0.push(s);
        StringIndex((self.0.len() - 1) as u32)
    }

    /// Get the string data corresponding to a given index.
    pub fn get(&self, index: StringIndex) -> &str {
        &self.0[index.0 as usize]
    }
}

/// This is the context used when generating new statements.
///
/// This struct should be reset at the beginning of each new function.
///
/// The context allows us to map variable names to a stack index.
struct NameContext {
    /// This maps names we've encountered so far.
    names: HashMap<String, StackIndex>,
    /// This holds the latest variable index.
    index: u32,
}

impl NameContext {
    fn new() -> Self {
        NameContext {
            names: HashMap::new(),
            index: 0,
        }
    }

    // Replace a given name with its stack index.
    fn lookup(&mut self, name: String) -> SimplifyResult<StackIndex> {
        match self.names.get(&name) {
            Some(&index) => Ok(index),
            None => undefined_name(name),
        }
    }

    // Introduce a new variable into the context
    fn introduce(&mut self, s: String) {
        self.names.insert(s, StackIndex(self.index));
        self.index += 1;
    }
}

/// This struct is used to replace function names with indices.
///
/// This holds mutable state about what functions we've encountered so far in order
/// to replace references to these functions with indices.
struct FunctionNames {
    /// This maps function names to indices.
    name_to_index: HashMap<String, StackIndex>,
    /// This holds the current stack index.
    ///
    /// This gets incremented for each new function declaration.
    index: u32,
    /// Once we encounter the main function, this gets set to that.
    main_index: Option<StackIndex>,
}

impl FunctionNames {
    fn new() -> Self {
        FunctionNames {
            name_to_index: HashMap::new(),
            index: 0,
            main_index: None,
        }
    }

    fn replace(&self, name: String) -> SimplifyResult<StackIndex> {
        match self.name_to_index.get(&name) {
            Some(&index) => Ok(index),
            None => undefined_name(name),
        }
    }

    fn new_name(&mut self, name: String) {
        if !self.name_to_index.contains_key(&name) {
            let was_main = &name == "main";
            let new_index = StackIndex(self.index);
            self.name_to_index.insert(name, new_index);
            if was_main {
                self.main_index = Some(new_index);
            }
            self.index += 1;
        }
    }
}

/// Represents the context we need to carry around when simplifying.
///
/// This context carries things like the data surrounding name generation,
/// as well as holding the future string litteral table.
struct Context {
    /// This will be used to store the string litterals we encounter.
    strings: StringTable,
    /// The name context is used to keep track of variable names.
    ///
    /// The context is reset at the start of each function. It helps us replace
    /// variable names with stack indices.
    names: NameContext,
    /// This helps us replace named functions with indices.
    functions: FunctionNames,
}

impl Context {
    fn new() -> Self {
        Context {
            strings: StringTable::new(),
            names: NameContext::new(),
            functions: FunctionNames::new(),
        }
    }

    // Clear the name context.
    //
    // This should be used when moving to a new function, where we need to start
    // our variable naming over again.
    fn reset_names(&mut self) {
        self.names = NameContext::new();
    }
}

/// Represents a simplified Poline program.
#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    /// The string table holds the string litterals in the program.
    pub string_table: StringTable,
    /// This holds information about the index of the main function.
    ///
    /// The main function will be used by the interpreter to start executing our program.
    /// When simplifying the AST we parsed out, an error is thrown when no main function
    /// is found.
    pub main_function: StackIndex,
    /// The top level function declarations making up the program.
    ///
    /// The function called "main" is the entry point of the program.
    pub functions: Vec<FunctionDeclaration>,
}

/// This enum represents the different errors that can occurr when simplifying.
#[derive(Clone, Debug, PartialEq)]
pub enum SimplifyError {
    /// The program references a name that hasn't been defined.
    ///
    /// This can happen when an undefined variable is referenced, or when an undefined
    /// function is called.
    UndefinedName(String),
    /// No main function has been defined in the program.
    ///
    /// We could catch this later in the program, but figuring this out now is a bit
    /// easier for the interpreting stage. If we didn't report this now, the interpreting
    /// stage would need to immediately, since it can't start interpreting without a main
    /// function to start with.
    NoMainFunction,
}

pub type SimplifyResult<T> = Result<T, SimplifyError>;

fn undefined_name<T>(name: String) -> SimplifyResult<T> {
    Err(SimplifyError::UndefinedName(name))
}

fn simplify_vec<A, B, F>(vec: Vec<A>, mut f: F) -> SimplifyResult<Vec<B>>
where
    F: FnMut(A) -> SimplifyResult<B>,
{
    let f = &mut f;
    let mut acc = Vec::new();
    for item in vec {
        acc.push(f(item)?);
    }
    Ok(acc)
}

fn simplify_arg(ctx: &mut Context, argument: parser::Argument) -> SimplifyResult<Argument> {
    match argument {
        parser::Argument::Str(s) => Ok(Argument::Str(ctx.strings.insert(s))),
        parser::Argument::Name(s) => ctx.names.lookup(s).map(Argument::Name),
    }
}

fn simplify_function_call(
    ctx: &mut Context,
    function: parser::FunctionCall,
) -> SimplifyResult<FunctionCall> {
    let name = ctx.functions.replace(function.name)?;
    let args = simplify_vec(function.args, |arg| simplify_arg(ctx, arg))?;
    Ok(FunctionCall { name, args })
}

fn simplify_statement(
    ctx: &mut Context,
    statement: parser::Statement,
) -> SimplifyResult<Statement> {
    match statement {
        parser::Statement::Print(arg) => simplify_arg(ctx, arg).map(Statement::Print),
        parser::Statement::Recv(new) => {
            ctx.names.introduce(new);
            Ok(Statement::Recv)
        }
        parser::Statement::Send(arg, process) => {
            let process_name = ctx.names.lookup(process)?;
            let arg = simplify_arg(ctx, arg)?;
            Ok(Statement::Send(arg, process_name))
        }
        parser::Statement::Call(function) => {
            simplify_function_call(ctx, function).map(Statement::Call)
        }
        parser::Statement::Spawn(function, new) => {
            ctx.names.introduce(new);
            simplify_function_call(ctx, function).map(Statement::Spawn)
        }
    }
}

fn simplify_fn(
    ctx: &mut Context,
    function: parser::FunctionDeclaration,
) -> SimplifyResult<FunctionDeclaration> {
    ctx.functions.new_name(function.name);
    let arg_count = function.arg_names.len() as u32;
    ctx.reset_names();
    for arg_name in function.arg_names {
        ctx.names.introduce(arg_name);
    }
    let body = simplify_vec(function.body, |stmt| simplify_statement(ctx, stmt))?;
    Ok(FunctionDeclaration { arg_count, body })
}

pub fn simplify(syntax: parser::Syntax) -> SimplifyResult<Program> {
    let mut ctx = Context::new();
    let functions = simplify_vec(syntax.functions, |func| simplify_fn(&mut ctx, func))?;
    let main_index = ctx.functions.main_index;
    let main_function = main_index.ok_or(SimplifyError::NoMainFunction)?;
    Ok(Program {
        string_table: ctx.strings,
        main_function,
        functions,
    })
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn simplify_works_on_function_names() {
        let source = "fn one(a) {} fn two(a, b) {} fn main() {}";
        let syntax = parser::collect_errors_and_parse(source).unwrap();
        let result = simplify(syntax);
        let functions = vec![
            FunctionDeclaration {
                arg_count: 1,
                body: Vec::new(),
            },
            FunctionDeclaration {
                arg_count: 2,
                body: Vec::new(),
            },
            FunctionDeclaration {
                arg_count: 0,
                body: Vec::new(),
            },
        ];
        let expected = Program {
            string_table: StringTable::new(),
            main_function: StackIndex(2),
            functions,
        };
        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn simplify_can_handle_shadowing() {
        let source = "fn main(a) { recv a; print a; }";
        let syntax = parser::collect_errors_and_parse(source).unwrap();
        let result = simplify(syntax);
        let body = vec![
            Statement::Recv,
            Statement::Print(Argument::Name(StackIndex(1))),
        ];
        let functions = vec![FunctionDeclaration { arg_count: 1, body }];
        let expected = Program {
            string_table: StringTable::new(),
            main_function: StackIndex(0),
            functions,
        };
        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn simplify_can_handle_spawn_and_send() {
        let source = "fn main() { spawn main() as p; send p to p; }";
        let syntax = parser::collect_errors_and_parse(source).unwrap();
        let result = simplify(syntax);
        let body = vec![
            Statement::Spawn(FunctionCall {
                name: StackIndex(0),
                args: vec![],
            }),
            Statement::Send(Argument::Name(StackIndex(0)), StackIndex(0)),
        ];
        let functions = vec![FunctionDeclaration { arg_count: 0, body }];
        let expected = Program {
            string_table: StringTable::new(),
            main_function: StackIndex(0),
            functions,
        };
        assert_eq!(result, Ok(expected));
    }

    #[test]
    fn simplify_can_handle_string_litterals() {
        let source = "fn foo() { print \"A\"; } fn main() { print \"A\"; }";
        let syntax = parser::collect_errors_and_parse(source).unwrap();
        let result = simplify(syntax);
        let functions = vec![
            FunctionDeclaration {
                arg_count: 0,
                body: vec![Statement::Print(Argument::Str(StringIndex(0)))],
            },
            FunctionDeclaration {
                arg_count: 0,
                body: vec![Statement::Print(Argument::Str(StringIndex(1)))],
            },
        ];
        let mut string_table = StringTable::new();
        string_table.insert("A".into());
        string_table.insert("A".into());
        let expected = Program {
            string_table,
            main_function: StackIndex(1),
            functions,
        };
        assert_eq!(result, Ok(expected));
    }
}
