use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::iter::Peekable;
use std::str::Chars;

/// Represents the branch variant for a given token
///
/// These exist because the parser needs to know the branch of a given
/// enum without matching on its contents.
#[derive(Debug, Clone, Copy, PartialEq)]
enum TokenType {
    Function,
    Recv,
    Send,
    To,
    Spawn,
    As,
    Print,
    OpenBrace,
    CloseBrace,
    OpenParens,
    CloseParens,
    Comma,
    Semicolon,
    Str,
    Name,
}

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

impl Token {
    fn typ(&self) -> TokenType {
        match *self {
            Token::Function => TokenType::Function,
            Token::Recv => TokenType::Recv,
            Token::Send => TokenType::Send,
            Token::To => TokenType::To,
            Token::Spawn => TokenType::Spawn,
            Token::As => TokenType::As,
            Token::Print => TokenType::Print,
            Token::OpenBrace => TokenType::OpenBrace,
            Token::CloseBrace => TokenType::CloseBrace,
            Token::OpenParens => TokenType::OpenParens,
            Token::CloseParens => TokenType::CloseParens,
            Token::Comma => TokenType::Comma,
            Token::Semicolon => TokenType::Semicolon,
            Token::Str(_) => TokenType::Str,
            Token::Name(_) => TokenType::Name,
        }
    }
}

/// Represents the type of errors that can occurr while lexing.
#[derive(Debug, PartialEq)]
enum LexError {
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

    fn same(&mut self, expected: char) -> bool {
        match self.chars.peek() {
            Some(c) if *c == expected => {
                self.chars.next();
                true
            }
            _ => false,
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
                't' => {
                    if self.same('o') {
                        return Some(Ok(Token::To));
                    }
                }
                'a' => {
                    if self.same('s') {
                        return Some(Ok(Token::As));
                    }
                }
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
enum Argument {
    /// A variable name.
    Name(String),
    /// A String litteral.
    Litteral(String),
}

/// Represents a function call synactically.
struct FunctionCall {
    /// The name of the function being called.
    name: String,
    /// A series of arguments being passed to the call.
    args: Vec<Argument>,
}

/// A statement is a single operation inside the body of a function.
enum Statement {
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
struct FunctionDeclaration {
    /// The name associated with the function.
    name: String,
    /// The names of the variables declared for use in the function.
    arg_names: Vec<String>,
    /// The body of a function is composed of a series of a statements.
    body: Vec<Statement>,
}

/// This holds the information and state necessary to parse items.
struct Parser {
    /// A sequence of tokens composing the program we want to compile.
    tokens: Vec<Token>,
    /// Our current position in the sequence of tokens.
    position: usize,
}

impl Parser {
    fn at_end(&self) -> bool {
        self.position == self.tokens.len()
    }

    fn peek<'a>(&'a self) -> Option<&'a Token> {
        self.tokens.get(self.position)
    }

    fn previous<'a>(&'a self) -> &'a Token {
        &self.tokens[self.position - 1]
    }

    fn advance<'a>(&'a mut self) -> &'a Token {
        if !self.at_end() {
            self.position += 1;
        }
        self.previous()
    }

    fn check(&self, typ: TokenType) -> bool {
        match self.peek() {
            Some(token) if token.typ() == typ => true,
            _ => false,
        }
    }

    fn any(&mut self, types: &[TokenType]) -> bool {
        for typ in types {
            if self.check(*typ) {
                self.advance();
                return true;
            }
        }
        false
    }
}

fn main() -> io::Result<()> {
    let mut args = std::env::args();
    args.next();
    let file_name = args.next().unwrap();
    let mut file = File::open(file_name)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    for token in Lexer::new(&contents) {
        println!("{:?}", token);
    }
    Ok(())
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
        let text = "toas";
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
}
