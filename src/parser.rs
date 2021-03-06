//! The parser module converts source code into a typed representation of that Code.
//!
//! The representation tries to stay faithful to what was written in that
//! source code. The main way to interact with this module is through the
//! `collect_errors_and_parse` function.
use std::iter::Peekable;
use std::str::Chars;

/// Represents the Tokens that compose the language.
///
/// The first step in going from a textual representation of the language to
/// an in-code representation is to split up the text into a sequence of
/// tokens. Besides the litteral and string tokens, each token corresponds
/// to a single string. The variable tokens have specific content, either
/// because they represent a string litteral, or a variable name.
#[derive(Debug, PartialEq)]
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
    /// Represents the first parens in a function; corresponds to `(`.
    OpenParens,
    /// Represents the last parens in a function; corresponds to `)`.
    CloseParens,
    /// The comma is used for arguments; corresponds to `,`.
    Comma,
    /// Represents the end of a statement; corresponds to `;`.
    Semicolon,
    /// Represents a string litteral; e.g. `"abc"`, `"bar"`.
    Str(String),
    /// Represents a variable; e.g. `x`, `baz`.
    Name(String),
}

/// Represents the type of errors that can occurr while lexing.
#[derive(Debug, PartialEq)]
pub enum LexError {
    /// Represents generic failure.
    Failed,
    /// Represents failure due to an unterminated string
    UnterminatedString,
}

type LexResult = Result<Token, LexError>;

/// The lexer takes in a source string, and spits out tokens.
struct Lexer<'a> {
    /// The source holds the entire program in a single string.
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars().peekable(),
        }
    }

    fn string(&mut self) -> Result<String, LexError> {
        let mut acc = String::new();
        let mut terminated = false;
        while let Some(c) = self.chars.next() {
            if c == '"' {
                terminated = true;
                break;
            }
            acc.push(c);
        }
        if terminated {
            Ok(acc)
        } else {
            Err(LexError::UnterminatedString)
        }
    }

    fn identifier(&mut self, starter: char) -> String {
        let mut acc = String::new();
        acc.push(starter);
        while let Some(c) = self.chars.peek() {
            if !c.is_alphanumeric() {
                break;
            }
            // This is safe since we peeked
            acc.push(self.chars.next().unwrap());
        }
        acc
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.chars.next() {
            match c {
                '{' => return Some(Ok(Token::OpenBrace)),
                '}' => return Some(Ok(Token::CloseBrace)),
                ';' => return Some(Ok(Token::Semicolon)),
                '(' => return Some(Ok(Token::OpenParens)),
                ')' => return Some(Ok(Token::CloseParens)),
                ',' => return Some(Ok(Token::Comma)),
                '"' => return Some(self.string().map(Token::Str)),
                c if c.is_alphabetic() => {
                    let ident = self.identifier(c);
                    let out = match ident.as_ref() {
                        "fn" => Token::Function,
                        "recv" => Token::Recv,
                        "send" => Token::Send,
                        "to" => Token::To,
                        "spawn" => Token::Spawn,
                        "as" => Token::As,
                        "print" => Token::Print,
                        _ => Token::Name(ident),
                    };
                    return Some(Ok(out));
                }
                // Simply ignore whitespace between tokens
                w if w.is_whitespace() => {}
                _ => return Some(Err(LexError::Failed)),
            }
        }
        None
    }
}

/// Represents a syntactical argument.
#[derive(Debug, PartialEq)]
pub enum Argument {
    /// A variable name.
    Name(String),
    /// A String litteral.
    Str(String),
}

/// Represents a function call synactically.
#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    /// The name of the function being called.
    pub name: String,
    /// A series of arguments being passed to the call.
    pub args: Vec<Argument>,
}

/// A statement is a single operation inside the body of a function.
#[derive(Debug, PartialEq)]
pub enum Statement {
    /// Represents printing an argument out.
    Print(Argument),
    /// Represents receiving a message inside a variable.
    Recv(String),
    /// Represents spawning a new thread with a function call and name.
    Spawn(FunctionCall, String),
    /// Represents a send operation to a given thread.
    Send(Argument, String),
    /// Represents a function call as a statement.
    Call(FunctionCall),
}

/// Represents a single function declaration.
///
/// A Poline program is composed of a list of function declarations, one
/// of which is the main function.
#[derive(Debug, PartialEq)]
pub struct FunctionDeclaration {
    /// The name associated with the function.
    pub name: String,
    /// The names of the variables declared for use in the function.
    pub arg_names: Vec<String>,
    /// The body of a function is composed of a series of a statements.
    pub body: Vec<Statement>,
}

/// Represents the root of our syntax tree.
///
/// A program in Poline is just a list of function declarations.
#[derive(Debug, PartialEq)]
pub struct Syntax {
    /// The sequence of functions that make up our program.
    pub functions: Vec<FunctionDeclaration>,
}

/// Represents an error occurring during parsing.
///
/// At the moment the parse errors aren't particularly expressive.
#[derive(Debug, PartialEq)]
pub enum ParseError {
    /// The parser failed for some reason, described in the string.
    Failed(String),
    /// The input to parse failed to pass the lexing step.
    ///
    /// The parser never generates this error, but it's useful
    /// to have this to avoid exposing the lexing step outside
    /// of this module.
    FailedToLex(Vec<LexError>),
}

pub type ParseResult<T> = Result<T, ParseError>;

fn parse_fail<T, S: Into<String>>(s: S) -> ParseResult<T> {
    Err(ParseError::Failed(s.into()))
}

/// This holds the information and state necessary to parse items.
struct Parser {
    /// A sequence of tokens composing the program we want to compile.
    tokens: Vec<Token>,
    /// Our current position in the sequence of tokens.
    position: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    fn at_end(&self) -> bool {
        self.position == self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.position - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.at_end() {
            self.position += 1;
        }
        self.previous()
    }

    fn check(&self, token: &Token) -> bool {
        self.peek() == Some(token)
    }

    fn expect(&mut self, token: &Token) -> ParseResult<()> {
        match self.peek() {
            Some(right) if right == token => {
                self.advance();
                Ok(())
            }
            Some(wrong) => parse_fail(format!("Expected {:?} got {:?}", token, wrong)),
            None => parse_fail("Insufficient input"),
        }
    }

    fn extract<R, F>(&mut self, matcher: F) -> ParseResult<R>
    where
        F: Fn(&Token) -> Option<R>,
    {
        if let Some(p) = self.peek() {
            if let Some(r) = matcher(p) {
                self.advance();
                Ok(r)
            } else {
                parse_fail(format!("Unexpected {:?}", p))
            }
        } else {
            parse_fail("Insufficient input")
        }
    }

    fn semicolon(&mut self) -> ParseResult<()> {
        self.expect(&Token::Semicolon)
    }

    fn name(&mut self) -> ParseResult<String> {
        self.extract(|x| match x {
            Token::Name(s) => Some(s.to_owned()),
            _ => None,
        })
    }

    fn arg_names(&mut self) -> ParseResult<Vec<String>> {
        self.expect(&Token::OpenParens)?;
        let mut args = Vec::new();
        if let Some(Token::Name(s)) = self.peek() {
            let mine = s.to_owned();
            self.advance();
            args.push(mine);
            while self.check(&Token::Comma) {
                self.advance();
                args.push(self.name()?);
            }
        }
        self.expect(&Token::CloseParens)?;
        Ok(args)
    }

    fn argument(&mut self) -> ParseResult<Argument> {
        self.extract(|x| match x {
            Token::Name(s) => Some(Argument::Name(s.to_owned())),
            Token::Str(s) => Some(Argument::Str(s.to_owned())),
            _ => None,
        })
    }

    fn arguments(&mut self) -> ParseResult<Vec<Argument>> {
        self.expect(&Token::OpenParens)?;
        let mut args = Vec::new();
        let first_argument = match self.peek() {
            Some(Token::Name(s)) => Some(Argument::Name(s.to_owned())),
            Some(Token::Str(s)) => Some(Argument::Str(s.to_owned())),
            _ => None,
        };
        if let Some(arg) = first_argument {
            self.advance();
            args.push(arg);
            while self.check(&Token::Comma) {
                self.advance();
                args.push(self.argument()?);
            }
        }
        self.expect(&Token::CloseParens)?;
        Ok(args)
    }

    fn function_call(&mut self) -> ParseResult<FunctionCall> {
        let name = self.name()?;
        let args = self.arguments()?;
        Ok(FunctionCall { name, args })
    }

    fn statement(&mut self) -> ParseResult<Option<Statement>> {
        if self.check(&Token::Print) {
            self.advance();
            let arg = self.argument()?;
            self.semicolon()?;
            Ok(Some(Statement::Print(arg)))
        } else if self.check(&Token::Recv) {
            self.advance();
            let name = self.name()?;
            self.semicolon()?;
            Ok(Some(Statement::Recv(name)))
        } else if self.check(&Token::Spawn) {
            self.advance();
            let call = self.function_call()?;
            self.expect(&Token::As)?;
            let name = self.name()?;
            self.semicolon()?;
            Ok(Some(Statement::Spawn(call, name)))
        } else if self.check(&Token::Send) {
            self.advance();
            let arg = self.argument()?;
            self.expect(&Token::To)?;
            let name = self.name()?;
            self.semicolon()?;
            Ok(Some(Statement::Send(arg, name)))
        } else if let Some(Token::Name(s)) = self.peek() {
            let name = s.to_owned();
            self.advance();
            let args = self.arguments()?;
            self.semicolon()?;
            let call = FunctionCall { name, args };
            Ok(Some(Statement::Call(call)))
        } else {
            Ok(None)
        }
    }

    fn function_decl(&mut self) -> ParseResult<FunctionDeclaration> {
        if !self.check(&Token::Function) {
            return parse_fail("Expected function declaration");
        }
        self.advance();
        let name = self.name()?;
        let arg_names = self.arg_names()?;
        self.expect(&Token::OpenBrace)?;
        let mut body = Vec::new();
        while let Some(statement) = self.statement()? {
            body.push(statement);
        }
        self.expect(&Token::CloseBrace)?;
        Ok(FunctionDeclaration {
            name,
            arg_names,
            body,
        })
    }

    fn syntax(&mut self) -> ParseResult<Syntax> {
        let mut functions = Vec::new();
        while !self.at_end() {
            let decl = self.function_decl()?;
            functions.push(decl);
        }
        Ok(Syntax { functions })
    }
}

/// Try and parse a Poline program, failing at the Lexing or Parsing step.
///
/// This acts as the main way to consume this module.
///
/// In the Lexing step, multiple errors can be emitted, and will be collected
/// and returned with the `ParseError::FailedToLex` branch. Any error
/// during Parsing will short circuit, and end the step.
pub fn collect_errors_and_parse(content: &str) -> ParseResult<Syntax> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    for token in Lexer::new(content) {
        match token {
            Ok(t) => tokens.push(t),
            Err(e) => errors.push(e),
        }
    }
    if !errors.is_empty() {
        return Err(ParseError::FailedToLex(errors));
    }
    let mut parser = Parser::new(tokens);
    parser.syntax()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lexer_works_for_semicolons() {
        let text = ";;";
        let tokens: Vec<LexResult> = Lexer::new(text).collect();
        assert_eq!(tokens, vec![Ok(Token::Semicolon), Ok(Token::Semicolon)]);
    }

    #[test]
    fn lexer_works_for_braces() {
        let text = "{}";
        let tokens: Vec<LexResult> = Lexer::new(text).collect();
        assert_eq!(tokens, vec![Ok(Token::OpenBrace), Ok(Token::CloseBrace)]);
    }

    #[test]
    fn lexer_works_for_to_and_as() {
        let text = "to as";
        let tokens: Vec<LexResult> = Lexer::new(text).collect();
        let expected = vec![Ok(Token::To), Ok(Token::As)];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn lexer_ignores_whitespace() {
        let text = ";   \t\n;";
        let tokens: Vec<LexResult> = Lexer::new(text).collect();
        assert_eq!(tokens, vec![Ok(Token::Semicolon), Ok(Token::Semicolon)]);
    }

    #[test]
    fn lexer_can_parse_strings() {
        let text = "\"foo\";";
        let tokens: Vec<LexResult> = Lexer::new(text).collect();
        let expected = vec![Ok(Token::Str(String::from("foo"))), Ok(Token::Semicolon)];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn lexer_errors_on_unterminated_strings() {
        let text = "\"bad";
        let tokens: Vec<LexResult> = Lexer::new(text).collect();
        assert_eq!(tokens, vec![Err(LexError::UnterminatedString)]);
    }

    #[test]
    fn lexer_works_on_parens_and_comma() {
        let text = "(,)";
        let tokens: Vec<LexResult> = Lexer::new(text).collect();
        let expected = vec![
            Ok(Token::OpenParens),
            Ok(Token::Comma),
            Ok(Token::CloseParens),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn lexer_works_on_identifiers_and_keywords() {
        let text = "fn print recv send to spawn as variable";
        let tokens: Vec<LexResult> = Lexer::new(text).collect();
        let expected = vec![
            Ok(Token::Function),
            Ok(Token::Print),
            Ok(Token::Recv),
            Ok(Token::Send),
            Ok(Token::To),
            Ok(Token::Spawn),
            Ok(Token::As),
            Ok(Token::Name(String::from("variable"))),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn lexer_works_on_multiple_arg_names() {
        let text = "both(a, b)";
        let tokens: Vec<LexResult> = Lexer::new(text).collect();
        let expected = vec![
            Ok(Token::Name(String::from("both"))),
            Ok(Token::OpenParens),
            Ok(Token::Name(String::from("a"))),
            Ok(Token::Comma),
            Ok(Token::Name(String::from("b"))),
            Ok(Token::CloseParens),
        ];
        assert_eq!(tokens, expected);
    }

    #[test]
    fn parser_works_on_example_program() {
        let program =
            "fn foo(a, b) { print a; spawn foo(a, b) as p; send \"bar\" to p; recv a; foo(a, b); }";
        let result = collect_errors_and_parse(program);
        let body = vec![
            Statement::Print(Argument::Name("a".into())),
            Statement::Spawn(
                FunctionCall {
                    name: "foo".into(),
                    args: vec![Argument::Name("a".into()), Argument::Name("b".into())],
                },
                "p".into(),
            ),
            Statement::Send(Argument::Str("bar".into()), "p".into()),
            Statement::Recv("a".into()),
            Statement::Call(FunctionCall {
                name: "foo".into(),
                args: vec![Argument::Name("a".into()), Argument::Name("b".into())],
            }),
        ];
        let foo_func = FunctionDeclaration {
            name: "foo".into(),
            arg_names: vec!["a".into(), "b".into()],
            body,
        };
        let expected = Syntax {
            functions: vec![foo_func],
        };
        assert_eq!(result, Ok(expected));
    }
}
