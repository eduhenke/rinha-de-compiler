use std::collections::HashMap;

use crate::term::*;

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

fn eval_function_call(
  scope: HashMap<String, Term>,
  memo: &mut HashMap<(String, Vec<Term>), Term>,
  Call { callee, arguments, .. }: Call,
) -> Result<Term, EvalError> {
  let f = match_term!(eval_to_tail(&mut scope.clone(), memo, *callee)?, Term::Function, Type::Function);
  let mut scope = scope.clone();
  let eval_arguments = arguments
    .into_iter()
    .map(|arg| eval_to_tail(&mut scope.clone(), memo, arg))
    .collect::<Result<Vec<_>, _>>()?;
  let memoized_value = f.name.clone().and_then(|name| memo.get(&(name, eval_arguments.clone()))).cloned();
  match memoized_value {
    Some(val) => Ok(val),
    None => {
      scope.extend(f.parameters.into_iter().zip(eval_arguments.clone()));
      let return_val = eval_to_tail(&mut scope, memo, *f.value)?;
      if let Some(name) = f.name {
        memo.insert((name, eval_arguments), return_val.clone());
      }
      Ok(return_val)
    }
  }
}

fn eval_to_tail<'a>(
  scope: &'a mut HashMap<String, Term>,
  memo: &'a mut HashMap<(String, Vec<Term>), Term>,
  term: Term,
) -> Result<Term, EvalError> {
  match term {
    Term::Call(call) => {
      if call.is_tail_call {
        let Call { callee, arguments, is_tail_call } = call;
        let eval_arguments = arguments
          .into_iter()
          .map(|arg| eval_to_tail(&mut scope.clone(), memo, arg))
          .collect::<Result<Vec<_>, _>>()?;

        Ok(Term::Call(Call { callee, arguments: eval_arguments, is_tail_call }))
      } else {
        eval_function_call(scope.clone(), memo, call)
      }
    }
    Term::Binary(Binary(lhs, op, rhs)) => {
      use BinaryOp::*;
      match op {
        Add => match (eval(&mut scope.clone(), memo, *lhs)?, eval(scope, memo, *rhs)?) {
          (Term::Int(a), Term::Int(b)) => Ok(Term::Int(a + b)),
          (Term::Str(a), Term::Str(b)) => Ok(Term::Str(a + &b)),
          (Term::Str(a), Term::Int(b)) => Ok(Term::Str(a + &b.to_string())),
          (Term::Int(a), Term::Str(b)) => Ok(Term::Str(a.to_string() + &b)),
          (a, b) => Err(EvalError::InvalidAddition(a, b)),
        },
        Sub => match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, memo, *rhs)?, Term::Int, Type::Int))
          .map(Term::Int)
          .ok_or(EvalError::Overflow),
        Mul => match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Int, Type::Int)
          .checked_mul(match_term!(eval(scope, memo, *rhs)?, Term::Int, Type::Int))
          .map(Term::Int)
          .ok_or(EvalError::Overflow),
        Div => match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Int, Type::Int)
          .checked_div(match_term!(eval(scope, memo, *rhs)?, Term::Int, Type::Int))
          .map(Term::Int)
          .ok_or(EvalError::Overflow),
        Rem => match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Int, Type::Int)
          .checked_rem(match_term!(eval(scope, memo, *rhs)?, Term::Int, Type::Int))
          .map(Term::Int)
          .ok_or(EvalError::Overflow),
        Eq => match (eval(&mut scope.clone(), memo, *lhs)?, eval(scope, memo, *rhs)?) {
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
          eval(scope, memo, Term::Binary(Binary(lhs, BinaryOp::Eq, rhs)))?,
          Term::Bool,
          Type::Bool
        ))),
        Lt => match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, memo, *rhs)?, Term::Int, Type::Int))
          .map(|v| v < 0)
          .map(Term::Bool)
          .ok_or(EvalError::Overflow),
        Gt => match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, memo, *rhs)?, Term::Int, Type::Int))
          .map(|v| v > 0)
          .map(Term::Bool)
          .ok_or(EvalError::Overflow),
        Lte => match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, memo, *rhs)?, Term::Int, Type::Int))
          .map(|v| v <= 0)
          .map(Term::Bool)
          .ok_or(EvalError::Overflow),
        Gte => match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Int, Type::Int)
          .checked_sub(match_term!(eval(scope, memo, *rhs)?, Term::Int, Type::Int))
          .map(|v| v >= 0)
          .map(Term::Bool)
          .ok_or(EvalError::Overflow),
        And => Ok(Term::Bool(
          match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Bool, Type::Bool)
            && match_term!(eval(scope, memo, *rhs)?, Term::Bool, Type::Bool),
        )),
        Or => Ok(Term::Bool(
          match_term!(eval(&mut scope.clone(), memo, *lhs)?, Term::Bool, Type::Bool)
            || match_term!(eval(scope, memo, *rhs)?, Term::Bool, Type::Bool),
        )),
      }
    }
    Term::Let(Let { name, value, next, .. }) => {
      scope.insert(name, eval_to_tail(&mut scope.clone(), memo, *value)?);
      eval_to_tail(scope, memo, *next)
    }
    Term::If(If { condition, then, otherwise, .. }) => {
      let bool = match_term!(eval(&mut scope.clone(), memo, *condition)?, Term::Bool, Type::Bool);
      if bool { eval_to_tail(scope, memo, *then) } else { eval_to_tail(scope, memo, *otherwise) }
    }
    Term::Print(value) => {
      let evaluated = eval(scope, memo, *value);
      match &evaluated {
        Ok(term) => println!("{}", term),
        Err(e) => println!("{:?}", e),
      }
      evaluated
    }
    Term::First(value) => {
      let Tuple(fst, _) = match_term!(eval(scope, memo, *value)?, Term::Tuple, Type::Tuple);
      Ok(*fst)
    }
    Term::Second(value) => {
      let Tuple(_, snd) = match_term!(eval(scope, memo, *value)?, Term::Tuple, Type::Tuple);
      Ok(*snd)
    }
    Term::Var(var) => scope.get(&var).cloned().ok_or(EvalError::VarNotFound(var)),
    Term::Tuple(Tuple(first, second)) => Ok(Term::Tuple(Tuple(
      eval(&mut scope.clone(), memo, *first)?.into(),
      eval(scope, memo, *second)?.into(),
    ))),
    Term::Bool(_) | Term::Int(_) | Term::Str(_) | Term::Function(_) => Ok(term),
  }
}

pub fn eval<'a>(
  scope: &'a mut HashMap<String, Term>,
  memo: &'a mut HashMap<(String, Vec<Term>), Term>,
  term: Term,
) -> Result<Term, EvalError> {
  let mut term = eval_to_tail(scope, memo, term)?;

  while let Term::Call(call) = term {
    debug_assert!(call.is_tail_call);
    term = eval_function_call(scope.clone(), memo, call)?;
  }

  Ok(term)
}
