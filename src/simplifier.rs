//! This module exists to provide utilities to simplify the parsed AST.
//!
//! The main reason this module exists is to clean up the representation
//! of our source code into a form more amenable to interpretation. This mainly
//! involves extracting out string litterals and changing variables to use
//! an index form instead of relying on string litterals.
use crate::parser;
use std::collections::HashMap;
use std::iter::*;

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
struct StackIndex(u32);

/// Represents an index into the string litteral table.
///
/// Instead of storing string litterals directly, we keep them an external
/// table, and use indices into this table in the AST instead. This makes
/// manipulating the AST a bit easier.
#[derive(Clone, Copy, Debug, PartialEq)]
struct StringIndex(u32);

/// Represents an Argument, either as a string litteral or a variable.
///
/// In this module, instead of String names, we use de bruijin indices instead,
/// which always reference the nth variable on the stack when interpreting the stack.
/// String litterals reference the nth string in the litteral table.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Argument {
    /// Represents a reference to a named variable.
    ///
    /// This may be missing because we're referencing a non existent variable.
    Name(Option<StackIndex>),
    /// Represents a string litteral.
    Str(StringIndex),
}

/// Holds the information for a function call.
///
/// A function call is defined by the name of the function being called,
/// as well as the arguments being passed to the function.
#[derive(Clone, Debug, PartialEq)]
struct FunctionCall {
    /// The index giving us the name, and thus code, for the function.
    name: StackIndex,
    /// The arguments being passed to the function.
    args: Vec<Argument>,
}

/// Holds information defining a given statement.
#[derive(Clone, Debug, PartialEq)]
enum Statement {
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
    ///
    /// The program may have tried to send a variable that doesn't exist.
    Send(Argument, Option<StackIndex>),
    /// Call a given function.
    Call(FunctionCall),
}

/// Holds information about a given function.
///
/// This information is sufficient to be able to call the given function.
#[derive(Clone, Debug, PartialEq)]
struct FunctionDeclaration {
    /// Represents the name of the function.
    ///
    /// Functions use a stack index, where index N represents the Nth function
    /// declared. This is a consequence of all function declarations being
    /// on the top level of a Poline program.
    name: StackIndex,
    /// The number of arguments a function takes.
    ///
    /// At this stage, the names of the variables have been eliminated.
    arg_count: u32,
    /// A sequence of statements making up the function.
    body: Vec<Statement>,
}

/// Represents the table of string litterals in our program.
///
/// We store all of the string litterals in the program in a single table,
/// to make the representation of the program a bit simpler.
struct StringTable(Vec<String>);

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
/// The context allows us to map variable names to a stack index.
struct NameContext {
    names: HashMap<String, u32>,
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
    fn replace(&mut self, s: String) -> Option<StackIndex> {
        self.names.get(&s).map(|&index| StackIndex(index))
    }

    // Introduce a new variable into the context
    fn introduce(&mut self, s: String) {
        self.names.insert(s, self.index);
        self.index += 1;
    }
}

struct FunctionNames {
    name_to_index: HashMap<String, StackIndex>,
    index: u32,
}

impl FunctionNames {
    fn new() -> Self {
        // We start at 1 because "0" is reserved for the main function.
        FunctionNames {
            name_to_index: HashMap::new(),
            index: 1,
        }
    }

    fn new_name(&mut self, name: String) -> StackIndex {
        if &name == "main" {
            StackIndex(0)
        } else {
            match self.name_to_index.get(&name) {
                Some(index) => *index,
                None => {
                    let inserted = StackIndex(self.index);
                    self.name_to_index.insert(name, inserted);
                    self.index += 1;
                    inserted
                }
            }
        }
    }
}

/// Represents the context we need to carry around when simplifying.
///
/// This context carries things like the data surrounding name generation,
/// as well as holding the future string litteral table.
struct Context {
    strings: StringTable,
    names: NameContext,
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
struct Program {
    /// The string table holds the string litterals in the program.
    string_table: HashMap<String, StringIndex>,
    /// The top level function declarations making up the program.
    ///
    /// The function called "main" is the entry point of the program.
    functions: Vec<FunctionDeclaration>,
}

fn simplify_arg(ctx: &mut Context, argument: parser::Argument) -> Argument {
    match argument {
        parser::Argument::Str(s) => Argument::Str(ctx.strings.insert(s)),
        parser::Argument::Name(s) => Argument::Name(ctx.names.replace(s)),
    }
}

fn simplify_statements(ctx: &mut Context, statements: Vec<parser::Statement>) -> Vec<Statement> {
    statements
        .into_iter()
        .map(|statement| match statement {
            parser::Statement::Print(arg) => Statement::Print(simplify_arg(ctx, arg)),
            _ => unimplemented!(),
        })
        .collect()
}

fn simplify(syntax: parser::Syntax) -> Program {
    let mut ctx = Context::new();
    let mut functions = Vec::new();
    for function in syntax.functions {
        let name = ctx.functions.new_name(function.name);
        let arg_count = function.arg_names.len() as u32;
        ctx.reset_names();
        for arg_name in function.arg_names {
            ctx.names.introduce(arg_name);
        }
        let body = simplify_statements(&mut ctx, function.body);
        functions.push(FunctionDeclaration {
            name,
            arg_count,
            body,
        });
    }
    unimplemented!()
}
