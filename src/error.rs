use nom::Err;
use thiserror::Error;

// TODO: Remove these once we have error handling. These aliases are defined to make it obvious
// what error values need fixing, remove them once proper error handling has been implemented

pub type NullError = ();
pub const NULL_ERROR: NullError = ();

// TODO: Actual error messages from this
pub type NomError<'a> = Err<&'a [u8]>;

// General idea:
//   - Have internal error types returned by parser/lexer, only displayed by tests
//   - Have formatted error type that is created from the Internal errors. Error formatter
//     can do QoL things like looking up source locations

#[derive(Debug, Error)]
pub enum LexicalError {
    #[error("Failed to lex byte at offset {offset}")]
    FailedToLex { offset: usize },
    #[error("Unexpected end of file")]
    UnexpectedEOF,
}

// Parser will return error of parse path that reached the furthest, so offsets need to be
// preserved. Offsets are lexeme indices, will be translated by the error formatter
#[derive(Debug, Error)]
pub enum ParseError<'a> {
    #[error("Failed to parse: {0:?}")]
    NomError(NomError<'a>),
}
