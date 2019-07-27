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
                // Simply ignore whitespace between tokens
                w if w.is_whitespace() => {}
                _ => return Some(Err(LexError::Failed)),
            }
        }
        None
    }
}

fn main() {
    println!("Hello, world!");
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
        let expected = vec![Ok(Token::OpenParens), Ok(Token::Comma), Ok(Token::CloseParens)];
        assert_eq!(tokens, expected);
    }
}
