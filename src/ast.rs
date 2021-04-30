#[path = "parse.rs"]
mod parse;

use std::fmt;

#[derive(Clone)]
pub enum Expression {
    Binary(Box<Expression>, parse::Token, Box<Expression>),
    Grouping(Box<Expression>),
    Literal(parse::Literal),
    Unary(parse::Token, Box<Expression>),
}

impl Expression {
    fn accept<T>(self: &Box<Self>, visitor: &dyn Visitor<T>) -> T {
        visitor.visit(self)
    }
}

trait Visitor<T> {
    fn visit(&self, expr: &Box<Expression>) -> T;
}

pub struct AstPrinter {
    pub ast: Box<Expression>,
}

impl Visitor<String> for AstPrinter {
    fn visit(&self, expr: &Box<Expression>) -> String {
        match expr.as_ref() {
            Expression::Binary(lhs, tok, rhs) => {
                format!("({} {} {})", tok.lexeme.as_str(), &*lhs.accept(self).as_str(), &*rhs.accept(self).as_str())
            },
            Expression::Grouping(expr) => {
                format!("(group {})", &*expr.accept(self))
            },
            Expression::Literal(lit) => {
                format!("{}", lit)
            },
            Expression::Unary(tok, expr) => {
                format!("({} {})", tok.lexeme, &*expr.accept(self))
            },
        }
    }
}

impl ToString for AstPrinter {
    fn to_string(&self) -> String {
        let ast = &self.ast;
        let s = ast.accept(self);
        s.to_string()
    }
}

impl AstPrinter {
    fn print_ast(&self) {
        println!("{}", self.to_string());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parse::{Token, TokenType, Literal};

    fn addition(num1: i64, num2: i64) -> Box<Expression> {
        let num1 = Literal::Number(num1);
        let num2 = Literal::Number(num2);
        let op = Token::new(
            TokenType::Plus,
            "+".to_string(),
            Literal::Symbol("+".to_string()),
            1
        );
        Box::new(Expression::Binary(
            Box::new(Expression::Literal(num1)),
            op,
            Box::new(Expression::Literal(num2)),
        ))
    }

    fn negate(expr: Box<Expression>) -> Box<Expression> {
        let negate = Token::new(
            TokenType::Minus,
            "-".to_string(),
            Literal::Symbol("-".to_string()),
            1
        );
        Box::new(Expression::Unary(negate, expr))
    }

    #[test]
    fn ast_printing() {
        let add = addition(64, 128);
        let printer = AstPrinter { ast: add };
        assert_eq!(printer.to_string(), "(+ 64 128)".to_string());
    }

    #[test]
    fn ast_printing_nested() {
        let add = addition(123, 456);
        let neg = negate(add);
        let printer = AstPrinter { ast: neg };
        assert_eq!(printer.to_string(), "(- (+ 123 456))".to_string());
    }
}
