use crate::term::{BinaryOp, Term};

// macro to assert that expr is aexpr and get its value
macro_rules! assert_aexpr {
  ($expr:expr) => {
    match &$expr {
      Expr::AExpr(aexpr) => (*aexpr).clone(),
      _ => unreachable!(),
    }
  };
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AExpr {
  Int(i32),
  Str(String),
  Bool(bool),
  Var(String),
  Tuple(Box<AExpr>, Box<AExpr>),
  Binary(Box<AExpr>, BinaryOp, Box<AExpr>),
  First(Box<AExpr>),
  Second(Box<AExpr>),
  Function(Vec<String>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CExpr {
  Print(Box<Expr>),
  If(Box<Expr>, Box<Expr>, Box<Expr>),
  Call(AExpr, Vec<AExpr>, bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
  Let(String, Box<Expr>, Box<Expr>),
  CExpr(CExpr),
  AExpr(AExpr),
}

fn normalize_term(M: Term) -> Expr {
  normalize(M, &|x| x)
}

fn normalize(M: Term, k: &dyn Fn(Expr) -> Expr) -> Expr {
  match M {
    Term::Function(f) => k(Expr::AExpr(AExpr::Function(f.parameters, Box::new(normalize_term(*f.value))))),
    Term::Let(l) => normalize(*l.value, &|n1| {
      Expr::Let(l.name.clone(), Box::new(n1), Box::new(normalize(*l.next.clone(), k)))
    }),
    Term::If(i) => normalize_name(*i.condition, &|t| {
      k(Expr::CExpr(CExpr::If(
        Box::new(t),
        Box::new(normalize_term(*i.then.clone())),
        Box::new(normalize_term(*i.otherwise.clone())),
      )))
    }),
    Term::Print(p) => normalize_name(*p, &|t| k(Expr::CExpr(CExpr::Print(Box::new(t))))),
    Term::Binary(b) => normalize_name(*b.0, &|l| {
      normalize_name(*b.2.clone(), &|r| {
        k(Expr::AExpr(AExpr::Binary(Box::new(assert_aexpr!(l)), b.1.clone(), Box::new(assert_aexpr!(r)))))
      })
    }),
    Term::Call(c) => normalize_name(*c.callee, &|callee| {
      normalize_names(c.arguments.clone(), &|args| {
        k(Expr::CExpr(CExpr::Call(
          assert_aexpr!(callee),
          args.into_iter().map(|x| assert_aexpr!(x)).collect(),
          c.is_tail_call,
        )))
      })
    }),
    Term::Var(v) => k(Expr::AExpr(AExpr::Var(v))),
    Term::Int(i) => k(Expr::AExpr(AExpr::Int(i))),
    Term::Str(s) => k(Expr::AExpr(AExpr::Str(s))),
    Term::Bool(b) => k(Expr::AExpr(AExpr::Bool(b))),
    Term::Tuple(t) => normalize_name(*t.0, &|l| {
      normalize_name(*t.1.clone(), &|r| {
        k(Expr::AExpr(AExpr::Tuple(Box::new(assert_aexpr!(l)), Box::new(assert_aexpr!(r)))))
      })
    }),
    Term::First(f) => normalize_name(*f, &|t| k(Expr::AExpr(AExpr::First(Box::new(assert_aexpr!(t)))))),
    Term::Second(s) => normalize_name(*s, &|t| k(Expr::AExpr(AExpr::Second(Box::new(assert_aexpr!(t)))))),
  }
}

// (define normalize-name
//   (lambda (M k)
//   (normalize M (lambda (N) (if ( Value? N) (k N) (let([t (newvar)]) ‘(let (,t ,N) ,(k t))))))))
fn normalize_name(M: Term, k: &dyn Fn(Expr) -> Expr) -> Expr {
  static mut i: i32 = 0;
  normalize(M, &|n1| match n1 {
    Expr::AExpr(aexpr) => match aexpr {
      AExpr::Int(_) | AExpr::Str(_) | AExpr::Bool(_) | AExpr::Var(_) | AExpr::First(_) | AExpr::Second(_) => {
        k(Expr::AExpr(aexpr))
      }
      AExpr::Tuple(_, _) | AExpr::Binary(_, _, _) | AExpr::Function(_, _) => unsafe {
        let t = "t".to_string() + &i.to_string();
        i += 1;
        Expr::Let(t.clone(), Box::new(Expr::AExpr(aexpr)), Box::new(k(Expr::AExpr(AExpr::Var(t)))))
      },
    },
    _ => unsafe {
      let t = "t".to_string() + &i.to_string();
      i += 1;
      Expr::Let(t.clone(), Box::new(n1), Box::new(k(Expr::AExpr(AExpr::Var(t)))))
    },
  })
}

// (define normaiize-name”
//   (lambda (M* k)
//   (if (null? M*)
//   (k ‘())
//   (normalize-name (car M*) (lambda (t) (normalize-name” (cxtr M*) (lambda (t*)(k ‘(,t . at’)))))))))

fn normalize_names(Ms: Vec<Term>, k: &dyn Fn(Vec<Expr>) -> Expr) -> Expr {
  if Ms.is_empty() {
    k(vec![])
  } else {
    normalize_name(Ms[0].clone(), &|t| {
      normalize_names(Ms[1 ..].to_vec(), &|mut t2| {
        let mut t = vec![t.clone()];
        t.append(&mut t2);
        k(t)
      })
    })
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::term::{Binary, Let};

  fn parse(code: &str) -> Term {
    let parsed = rinha::parser::parse_or_report("stdin", code).unwrap();
    parsed.expression.into()
  }

  #[test]
  fn test() {
    let expr = Expr::Let(
      "x".to_string(),
      Box::new(Expr::AExpr(AExpr::Int(1))),
      Box::new(Expr::Let(
        "y".to_string(),
        Box::new(Expr::AExpr(AExpr::Int(2))),
        Box::new(Expr::CExpr(CExpr::Print(Box::new(Expr::AExpr(AExpr::Var("x".to_string())))))),
      )),
    );
    println!("{:#?}", expr);
    let expr = normalize_term(Term::Let(Let {
      name: "x".to_string(),
      value: Box::new(Term::Int(1)),
      next: Box::new(Term::Let(Let {
        name: "y".to_string(),
        value: Box::new(Term::Int(2)),
        next: Box::new(Term::Print(Box::new(Term::Var("x".to_string())))),
      })),
    }));
    println!("{:#?}", expr);
  }
  #[test]
  fn test2() {
    use self::AExpr::*;
    use BinaryOp::*;
    use Expr::*;
    let expr = normalize_term(parse("1 + 2 + 3"));
    assert_eq!(
      expr,
      Let(
        "t0".to_string(),
        AExpr(Binary(Int(2).into(), Add, Int(3).into())).into(),
        AExpr(Binary(Int(1).into(), Add, Var("t0".to_string()).into())).into(),
      )
    );
  }

  #[test]
  fn test3() {
    use self::AExpr::*;
    use BinaryOp::*;
    use Expr::*;
    let expr = normalize_term(parse(
      r#"
let fib = fn (x) => {
  if (x < 2) {
    x
  } else {
    fib (x - 1) + fib (x - 2)
  }
};
2"#,
    ));
    println!("{:#?}", expr);
  }

  #[test]
  fn test4() {
    use self::AExpr::*;
    use BinaryOp::*;
    use Expr::*;
    let expr = normalize_term(parse(
      r#"
  foo(6 + 8, 10) * bar(2, id(12))
      "#,
    ));
    println!("{:#?}", expr);
  }
}
