use crate::parser::Error as ParseError;
use codespan::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use std::path::Path;

pub fn report_parse_error(file: &Path, file_content: &str, err: ParseError) {
    let mut files = SimpleFiles::new();
    let file_id = files.add(file.to_string_lossy(), file_content);
    let diagnostic = match err {
        ParseError::ByteLiteralOutOfRange { token, span } => Diagnostic::error()
            .with_message(format!("Byte literal '{:?}' out of range", token))
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::DuplicateSectionDirective(span) => Diagnostic::error()
            .with_message("Duplicate section directive")
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::Eof => Diagnostic::error().with_message("Reached end of file when parsing."),
        ParseError::HalfLiteralOutOfRange { token, span } => Diagnostic::error()
            .with_message(format!("Half word literal '{:?}' out of range", token))
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::IncorrectCoprocRegister {
            got,
            expected,
            span,
        } => Diagnostic::error()
            .with_message(format!(
                "Register for CP{} found where register for CP{} was expected",
                got, expected
            ))
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::IncorrectLiteralType {
            got,
            span,
            expected,
        } => Diagnostic::error()
            .with_message(format!(
                "Literal '{:?}' found where {} was expected",
                got, expected
            ))
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::InvalidAlignment { token, span } => Diagnostic::error()
            .with_message(format!(
                "Invalid alignment '{:?}' - alignments must be in [0, 3]",
                token
            ))
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::InvalidFloatLiteral(token, span) => Diagnostic::error()
            .with_message(format!("Invalid float literal '{:?}'", token))
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::InvalidIntegerLiteral(token, span) => Diagnostic::error()
            .with_message(format!("Invalid integer literal '{:?}'", token))
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::LexerError(span) => Diagnostic::error()
            .with_message("Lexer error - unexpected symbol")
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::MissingComma(span) => Diagnostic::error()
            .with_message("Missing comma")
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::UnexpectedToken {
            got,
            span,
            expected,
        } => Diagnostic::error()
            .with_message(format!(
                "Got token {:?} where {} was expected",
                got, expected
            ))
            .with_labels(vec![Label::primary(file_id, span)]),
        ParseError::UnknownLabel(label, span) => Diagnostic::error()
            .with_message(format!("Unknown label '{}'", label))
            .with_labels(vec![Label::primary(file_id, span)]),
    };
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();
    term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
}
