use std::error;
use std::fmt;

#[derive(Debug, Clone)]
pub struct FileReadError {
    msg: String,
}

#[derive(Debug, Clone)]
pub struct SyntaxError {
    pub line: u32,
    pub code: String,
    pub message: String,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[line {}] Error {}: {}", self.line, self.code, self.message)
    }
}

impl error::Error for SyntaxError {}

impl FileReadError {
    pub fn new_from(err: Box<dyn error::Error>) -> Self {
        FileReadError {
            msg: format!("{}", err),
        }
    }
}

impl fmt::Display for FileReadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error reading source file: {}", self.msg)
    }
}

impl error::Error for FileReadError {}
