use std::ops::Deref;

use rinha::{ast::*, parser::Var};

// A rust macro that matches on the result of an expression and returns the respective variant
macro_rules! match_term {
  ($term:expr, $variant:path, $ty:expr) => {
    // Add => after the macro pattern
    match $term {
      $variant(inner) => inner,
      term => return Err(EvalError::ExpectedType($ty, term)),
    }
  };
}

#[derive(Debug)]
enum Type {
  Tuple,
  Bool,
  Int,
  String,
  Function,
}

#[derive(Debug)]
pub enum EvalError {
  VarNotFound(String),
  ExpectedType(Type, Term),
  Overflow,
  InvalidEquality,
  InvalidAddition(Term, Term),
}
type Scope = Vec<(String, Term)>;

pub fn eval(scope: &Scope, term: Term) -> Result<Term, EvalError> {
  match term {
    Term::Call(Call { callee, arguments, .. }) => {
      let f = match_term!(eval(scope, *callee)?, Term::Function, Type::Function);
      let mut scope = scope.clone();
      let eval_arguments =
        arguments.into_iter().map(|arg| eval(&scope, arg)).collect::<Result<Vec<_>, _>>()?;
      scope.extend(f.parameters.into_iter().map(|v| v.text).zip(eval_arguments));
      eval(&scope, *f.value)
    }
    Term::Binary(Binary { lhs, op, rhs, .. }) => {
      use BinaryOp::*;
      match op {
        Add => match (eval(scope, *lhs)?, eval(scope, *rhs)?) {
          (Term::Int(a), Term::Int(b)) => {
            Ok(Term::Int(Int { value: a.value + b.value, location: Location::default() }))
          }
          (Term::Str(a), Term::Str(b)) => {
            Ok(Term::Str(Str { value: a.value + &b.value, location: Location::default() }))
          }
          (Term::Str(a), Term::Int(b)) => {
            Ok(Term::Str(Str { value: a.value + &b.value.to_string(), location: Location::default() }))
          }
          (Term::Int(a), Term::Str(b)) => {
            Ok(Term::Str(Str { value: a.value.to_string() + &b.value, location: Location::default() }))
          }
          (a, b) => Err(EvalError::InvalidAddition(a, b)),
        },
        Sub => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .value
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int).value)
          .map(|v| Term::Int(Int { value: v, location: Location::default() }))
          .ok_or(EvalError::Overflow),
        Mul => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .value
          .checked_mul(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int).value)
          .map(|v| Term::Int(Int { value: v, location: Location::default() }))
          .ok_or(EvalError::Overflow),
        Div => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .value
          .checked_div(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int).value)
          .map(|v| Term::Int(Int { value: v, location: Location::default() }))
          .ok_or(EvalError::Overflow),
        Rem => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .value
          .checked_rem(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int).value)
          .map(|v| Term::Int(Int { value: v, location: Location::default() }))
          .ok_or(EvalError::Overflow),
        Eq => match (eval(scope, *lhs)?, eval(scope, *rhs)?) {
          (Term::Bool(a), Term::Bool(b)) => {
            Ok(Term::Bool(Bool { value: a.value == b.value, location: Location::default() }))
          }
          (Term::Str(a), Term::Str(b)) => {
            Ok(Term::Bool(Bool { value: a.value == b.value, location: Location::default() }))
          }
          (Term::Int(a), Term::Int(b)) => {
            Ok(Term::Bool(Bool { value: a.value == b.value, location: Location::default() }))
          }
          (Term::Tuple(a), Term::Tuple(b)) => {
            let first_eq = match_term!(
              Term::Binary(Binary {
                lhs: a.first,
                op: BinaryOp::Eq,
                rhs: b.first,
                location: Location::default()
              }),
              Term::Bool,
              Type::Bool
            )
            .value;
            let second_eq = match_term!(
              Term::Binary(Binary {
                lhs: a.second,
                op: BinaryOp::Eq,
                rhs: b.second,
                location: Location::default()
              }),
              Term::Bool,
              Type::Bool
            )
            .value;
            Ok(Term::Bool(Bool { value: first_eq && second_eq, location: Location::default() }))
          }
          _ => Err(EvalError::InvalidEquality),
        },
        Neq => Ok(Term::Bool(Bool {
          value: !match_term!(
            eval(scope, Term::Binary(Binary { lhs, op: BinaryOp::Eq, rhs, location: Location::default() }))?,
            Term::Bool,
            Type::Bool
          )
          .value,
          location: Location::default(),
        })),
        Lt => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .value
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int).value)
          .map(|v| Term::Bool(Bool { value: v < 0, location: Location::default() }))
          .ok_or(EvalError::Overflow),
        Gt => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .value
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int).value)
          .map(|v| Term::Bool(Bool { value: v > 0, location: Location::default() }))
          .ok_or(EvalError::Overflow),
        Lte => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .value
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int).value)
          .map(|v| Term::Bool(Bool { value: v <= 0, location: Location::default() }))
          .ok_or(EvalError::Overflow),
        Gte => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .value
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int).value)
          .map(|v| Term::Bool(Bool { value: v >= 0, location: Location::default() }))
          .ok_or(EvalError::Overflow),
        And => Ok(Term::Bool(Bool {
          value: match_term!(eval(scope, *lhs)?, Term::Bool, Type::Bool).value
            && match_term!(eval(scope, *rhs)?, Term::Bool, Type::Bool).value,
          location: Location::default(),
        })),
        Or => Ok(Term::Bool(Bool {
          value: match_term!(eval(scope, *lhs)?, Term::Bool, Type::Bool).value
            || match_term!(eval(scope, *rhs)?, Term::Bool, Type::Bool).value,
          location: Location::default(),
        })),
      }
    }
    Term::Let(Let { name, value, next, .. }) => {
      let mut scope = scope.clone();
      scope.push((name.text, eval(&scope, *value)?));
      eval(&scope, *next)
    }
    Term::If(If { condition, then, otherwise, .. }) => {
      let bool = match_term!(eval(scope, *condition)?, Term::Bool, Type::Bool);
      if bool.value { eval(scope, *then) } else { eval(scope, *otherwise) }
    }
    Term::Print(Print { value, .. }) => {
      let evaluated = eval(scope, *value);
      fn to_string(term: &Term) -> String {
        match term {
          Term::Bool(Bool { value, .. }) => value.to_string(),
          Term::Int(Int { value, .. }) => value.to_string(),
          Term::Str(Str { value, .. }) => value.to_string(),
          Term::Tuple(Tuple { first, second, .. }) => {
            format!("({}, {})", to_string(first), to_string(second))
          }
          _ => unreachable!(),
        }
      }
      match &evaluated {
        Ok(term) => println!("{}", to_string(term)),
        Err(e) => println!("{:?}", e),
      }
      evaluated
    }
    Term::First(First { value, .. }) => {
      let tuple = match_term!(eval(scope, *value)?, Term::Tuple, Type::Tuple);
      Ok(*tuple.first)
    }
    Term::Second(Second { value, .. }) => {
      let tuple = match_term!(eval(scope, *value)?, Term::Tuple, Type::Tuple);
      Ok(*tuple.second)
    }
    Term::Var(Var { text, .. }) => scope
      .iter()
      .rev()
      .find(|(name, _)| name == &text)
      .map(|(_, term)| term.clone())
      .ok_or(EvalError::VarNotFound(text)),
    Term::Tuple(Tuple { first, second, location }) => Ok(Term::Tuple(Tuple {
      first: eval(scope, *first)?.into(),
      second: eval(scope, *second)?.into(),
      location,
    })),

    Term::Bool(_) | Term::Int(_) | Term::Str(_) | Term::Function(_) => Ok(term),
    Term::Error(_) => unreachable!(),
  }
}
