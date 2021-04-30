use clap::{App, Arg};
use std::error::Error;
use std::io::prelude::*;

mod error;
mod parse;
mod ast;

fn get_line() -> String {
    print!("> ");
    std::io::stdout().flush().unwrap();
    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_s) => {}
        Err(_e) => {}
    };
    input.trim().to_string()
}

fn eval(code: String) -> Result<(), Box<dyn Error>> {
    let (tokens, errors) = parse::scan(code);
    for token in tokens.iter() {
        println!("{}", token);
    }
    for error in errors.iter() {
        eprintln!("{}", error);
    }
    Ok(())
}

fn run_repl() -> Result<(), Box<dyn Error>> {
    loop {
        let line = get_line();
        if line == "quit" {
            break ();
        }
        if let Err(e) = eval(line) {
            println!("Error: {}", e);
        }
    }
    Ok(())
}

fn run_file(file: String) -> Result<(), Box<dyn Error>> {
    let result = std::fs::read_to_string(file);

    match result {
        Ok(contents) => eval(contents),
        Err(e) => Err(error::FileReadError::new_from(e.into()).into()),
    }
}

fn run(file: Option<&str>) -> Result<(), Box<dyn Error>> {
    match file {
        None => run_repl(),
        Some(f) => run_file(f.to_string()),
    }
}

fn main() {
    let app = App::new("lox-rs")
        .version("0.1")
        .author("Peter Malmgren <ptmalmgren@gmail.com>")
        .about("Crafting interpreters lox language")
        .arg(Arg::with_name("file").required(false).index(1));
    let matches = app.get_matches();

    if let Err(e) = run(matches.value_of("file")) {
        eprintln!("Error: {}", e);
    }
}
