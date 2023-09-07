use crate::term::*;
use rinha::ast::BinaryOp;

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
pub enum Type {
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
    Term::Call(Call { callee, arguments }) => {
      let f = match_term!(eval(scope, *callee)?, Term::Function, Type::Function);
      let mut scope = scope.clone();
      let eval_arguments =
        arguments.into_iter().map(|arg| eval(&scope, arg)).collect::<Result<Vec<_>, _>>()?;
      scope.extend(f.parameters.into_iter().zip(eval_arguments));
      eval(&scope, *f.value)
    }
    Term::Binary(Binary(lhs, op, rhs)) => {
      use BinaryOp::*;
      match op {
        Add => match (eval(scope, *lhs)?, eval(scope, *rhs)?) {
          (Term::Int(a), Term::Int(b)) => Ok(Term::Int(a + b)),
          (Term::Str(a), Term::Str(b)) => Ok(Term::Str(a + &b)),
          (Term::Str(a), Term::Int(b)) => Ok(Term::Str(a + &b.to_string())),
          (Term::Int(a), Term::Str(b)) => Ok(Term::Str(a.to_string() + &b)),
          (a, b) => Err(EvalError::InvalidAddition(a, b)),
        },
        Sub => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int))
          .map(Term::Int)
          .ok_or(EvalError::Overflow),
        Mul => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .checked_mul(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int))
          .map(Term::Int)
          .ok_or(EvalError::Overflow),
        Div => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .checked_div(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int))
          .map(Term::Int)
          .ok_or(EvalError::Overflow),
        Rem => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .checked_rem(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int))
          .map(Term::Int)
          .ok_or(EvalError::Overflow),
        Eq => match (eval(scope, *lhs)?, eval(scope, *rhs)?) {
          (Term::Bool(a), Term::Bool(b)) => Ok(Term::Bool(a == b)),
          (Term::Str(a), Term::Str(b)) => Ok(Term::Bool(a == b)),
          (Term::Int(a), Term::Int(b)) => Ok(Term::Bool(a == b)),
          (Term::Tuple(Tuple(a1, a2)), Term::Tuple(Tuple(b1, b2))) => {
            let first_eq = match_term!(Term::Binary(Binary(a1, BinaryOp::Eq, b1)), Term::Bool, Type::Bool);
            let second_eq = match_term!(Term::Binary(Binary(a2, BinaryOp::Eq, b2)), Term::Bool, Type::Bool);
            Ok(Term::Bool(first_eq && second_eq))
          }
          _ => Err(EvalError::InvalidEquality),
        },
        Neq => Ok(Term::Bool(!match_term!(
          eval(scope, Term::Binary(Binary(lhs, BinaryOp::Eq, rhs)))?,
          Term::Bool,
          Type::Bool
        ))),
        Lt => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int))
          .map(|v| v < 0)
          .map(Term::Bool)
          .ok_or(EvalError::Overflow),
        Gt => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int))
          .map(|v| v > 0)
          .map(Term::Bool)
          .ok_or(EvalError::Overflow),
        Lte => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int))
          .map(|v| v <= 0)
          .map(Term::Bool)
          .ok_or(EvalError::Overflow),
        Gte => match_term!(eval(scope, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, *rhs)?, Term::Int, Type::Int))
          .map(|v| v >= 0)
          .map(Term::Bool)
          .ok_or(EvalError::Overflow),
        And => Ok(Term::Bool(
          match_term!(eval(scope, *lhs)?, Term::Bool, Type::Bool)
            && match_term!(eval(scope, *rhs)?, Term::Bool, Type::Bool),
        )),
        Or => Ok(Term::Bool(
          match_term!(eval(scope, *lhs)?, Term::Bool, Type::Bool)
            || match_term!(eval(scope, *rhs)?, Term::Bool, Type::Bool),
        )),
      }
    }
    Term::Let(Let { name, value, next, .. }) => {
      let mut scope = scope.clone();
      scope.push((name, eval(&scope, *value)?));
      eval(&scope, *next)
    }
    Term::If(If { condition, then, otherwise, .. }) => {
      let bool = match_term!(eval(scope, *condition)?, Term::Bool, Type::Bool);
      if bool { eval(scope, *then) } else { eval(scope, *otherwise) }
    }
    Term::Print(value) => {
      let evaluated = eval(scope, *value);
      match &evaluated {
        Ok(term) => println!("{}", term),
        Err(e) => println!("{:?}", e),
      }
      evaluated
    }
    Term::First(value) => {
      let Tuple(fst, _) = match_term!(eval(scope, *value)?, Term::Tuple, Type::Tuple);
      Ok(*fst)
    }
    Term::Second(value) => {
      let Tuple(_, snd) = match_term!(eval(scope, *value)?, Term::Tuple, Type::Tuple);
      Ok(*snd)
    }
    Term::Var(var) => scope
      .iter()
      .rev()
      .find(|(name, _)| name == &var)
      .map(|(_, term)| term.clone())
      .ok_or(EvalError::VarNotFound(var)),
    Term::Tuple(Tuple(first, second)) => {
      Ok(Term::Tuple(Tuple(eval(scope, *first)?.into(), eval(scope, *second)?.into())))
    }
    Term::Bool(_) | Term::Int(_) | Term::Str(_) | Term::Function(_) => Ok(term),
  }
}
