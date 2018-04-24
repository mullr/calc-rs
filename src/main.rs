extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "calc.pest"]
struct CalcParser;

#[derive(Debug)]
enum Op {
    Plus,
    Minus,
    Times,
    Div,
}

#[derive(Debug)]
enum Expr {
    Int(u32),
    Binary(Op, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum CalcParseError {
    IntParse,
    UnknownRule,
    NotEnoughInput
}

fn to_op(p: pest::iterators::Pair<Rule>) -> Result<Op, CalcParseError> {
    match p.as_rule() {
        Rule::op_plus => Ok(Op::Plus),
        Rule::op_minus => Ok(Op::Minus),
        Rule::op_times => Ok(Op::Times),
        Rule::op_div => Ok(Op::Div),
        _ => Err(CalcParseError::UnknownRule)
    }
}

fn to_expr(p: pest::iterators::Pair<Rule>) -> Result<Expr, CalcParseError> {
    match p.as_rule() {

        Rule::int => match p.as_str().parse::<u32>() {
            Ok(x) => Ok(Expr::Int(x)),
            Err(_) => Err(CalcParseError::IntParse)
        },

        Rule::mult_expr | Rule::add_expr => {
            let mut pairs = p.into_inner();
            let lhs = to_expr(pairs.next().ok_or(CalcParseError::NotEnoughInput)?)?;
            let op = to_op(pairs.next().ok_or(CalcParseError::NotEnoughInput)?)?;
            let rhs = to_expr(pairs.next().ok_or(CalcParseError::NotEnoughInput)?)?;
            Ok(Expr::Binary(op, Box::new(lhs), Box::new(rhs)))
        },

        _ => Err(CalcParseError::UnknownRule),
    }
}

fn eval(e: &Expr) -> u32 {
    match e {
        &Expr::Int(x) => x,
        &Expr::Binary(ref op, ref x, ref y) => match op {
            &Op::Plus => eval(x) + eval(y),
            &Op::Minus => eval(x) - eval(y),
            &Op::Times => eval(x) * eval(y),
            &Op::Div => eval(x) / eval(y),
        }
    }
}

fn main() {
    let pairs = CalcParser::parse(Rule::expr, "1*2+3*4").unwrap();
    for p in pairs {
        let e = to_expr(p).unwrap();
        println!("{:?}, {:?}", e, eval(&e));
    }
}
