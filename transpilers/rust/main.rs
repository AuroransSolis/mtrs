use logos::Logos;
use mips_parser::{Error as LexParseError, Parser};
use std::{
    fs::{read_to_string, write},
    io::{Error as IoError, Read, Write},
    path::Path,
};
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(name = "m2rs", version = "0.0.1", author = "Aurorans Solis")]
struct Arguments {
    #[structopt(short = "i", long = "input")]
    input_file: String,
    #[structopt(short = "o", long = "output")]
    output_file: Option<String>,
    #[structopt(short = "f", long = "force")]
    force: bool,
}

#[derive(Debug)]
enum Error {
    LexParseError(LexParseError),
    IoError(IoError),
}

fn main() -> Result<(), Error> {
    let Arguments {
        input_file,
        output_file,
        force,
    } = Arguments::from_args();
    let file_contents = read_to_string(&input_file).map_err(|e| Error::IoError(e))?;
    let parser = Parser::new(&file_contents);
    let expressions = parser.parse().map_err(|e| Error::LexParseError(e))?;
    for expression in expressions {
        println!("expression: {:?}", expression);
    }
    Ok(())
}
