use crate::parser::Error as ParseError;
use codespan::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

/*pub fn report_parse_error(file_content: &str, err: ParseError) {
    match err {
        ParseError::ByteLiteralOutOfRange(token) => {

        }
    }
}*/
