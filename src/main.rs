/// Represents the Tokens that compose the language.
///
/// The first step in going from a textual representation of the language to
/// an in-code representation is to split up the text into a sequence of
/// tokens. Besides the litteral and string tokens, each token corresponds
/// to a single string. The variable tokens have specific content, either
/// because they represent a string litteral, or a variable name.
enum Token {
    /// Declares a function; corresponds to `fn`.
    Function,
    /// Starts a receive statement; corresponds to `recv`.
    Recv,
    /// Starts a send statement; corresponds to `send`.
    Send,
    /// Used in the latter part of a send statement; corresponds to `to`.
    To,
    /// Starts a spawn statement; corresponds to `spawn`.
    Spawn,
    /// Used in the latter part of a spawn statement; corresponds to `as`.
    As,
    /// Used at the start of a print statement; corresponds to `print`.
    Print,
    /// Represents the first brace in a function; corresponds to `{`.
    OpenBrace,
    /// Represents the last brace in a function; corresponds to `}`.
    CloseBrace,
    /// Represents the end of a statement; corresponds to `;`.
    Semicolon,
    /// Represents a string litteral; e.g. `"abc"`, `"bar"`.
    Str(String),
    /// Represents a variable; e.g. `x`, `baz`.
    Name(String),
}

fn main() {
    println!("Hello, world!");
}
