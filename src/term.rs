use rinha::ast::BinaryOp;

#[derive(Debug, Clone)]
pub struct Call {
  pub callee: Box<Term>,
  pub arguments: Vec<Term>,
}

#[derive(Debug, Clone)]
pub struct Function {
  pub name: Option<String>,
  pub parameters: Vec<String>,
  pub value: Box<Term>,
}

#[derive(Debug, Clone)]
pub struct Let {
  pub name: String,
  pub value: Box<Term>,
  pub next: Box<Term>,
}

#[derive(Debug, Clone)]
pub struct If {
  pub condition: Box<Term>,
  pub then: Box<Term>,
  pub otherwise: Box<Term>,
}

#[derive(Debug, Clone)]
pub struct Binary(pub Box<Term>, pub BinaryOp, pub Box<Term>);
#[derive(Debug, Clone)]
pub struct Tuple(pub Box<Term>, pub Box<Term>);

#[derive(Debug, Clone)]
pub enum Term {
  Call(Call),
  Function(Function),
  Let(Let),
  If(If),
  Binary(Binary),
  Tuple(Tuple),
  Int(i32),
  Str(String),
  Print(Box<Term>),
  First(Box<Term>),
  Second(Box<Term>),
  Bool(bool),
  Var(String),
}

impl From<rinha::ast::Term> for Term {
  fn from(value: rinha::ast::Term) -> Self {
    match value {
      rinha::ast::Term::Error(_) => unreachable!(),
      rinha::ast::Term::Int(i) => Term::Int(i.value),
      rinha::ast::Term::Str(s) => Term::Str(s.value),
      rinha::ast::Term::Call(c) => Term::Call(Call {
        callee: Term::from(*c.callee).into(),
        arguments: c.arguments.into_iter().map(|a| a.into()).collect(),
      }),
      rinha::ast::Term::Binary(b) => {
        Term::Binary(Binary(Term::from(*b.lhs).into(), b.op, Term::from(*b.rhs).into()))
      }
      rinha::ast::Term::Let(rinha::ast::Let { name, value, next, .. }) => {
        let inner_value = match *value {
          rinha::ast::Term::Function(f) => Term::Function(Function {
            name: Some(name.text.clone()),
            parameters: f.parameters.into_iter().map(|p| p.text).collect(),
            value: Term::from(*f.value).into(),
          }),
          _ => Term::from(*value),
        };
        Term::Let(Let { name: name.text, value: Box::new(inner_value), next: Term::from(*next).into() })
      }
      rinha::ast::Term::Function(f) => Term::Function(Function {
        name: None,
        parameters: f.parameters.into_iter().map(|p| p.text).collect(),
        value: Term::from(*f.value).into(),
      }),
      rinha::ast::Term::If(i) => Term::If(If {
        condition: Term::from(*i.condition).into(),
        then: Term::from(*i.then).into(),
        otherwise: Term::from(*i.otherwise).into(),
      }),
      rinha::ast::Term::Print(p) => Term::Print(Term::from(*p.value).into()),
      rinha::ast::Term::First(f) => Term::First(Term::from(*f.value).into()),
      rinha::ast::Term::Second(s) => Term::Second(Term::from(*s.value).into()),
      rinha::ast::Term::Bool(b) => Term::Bool(b.value),
      rinha::ast::Term::Tuple(t) => {
        Term::Tuple(Tuple(Term::from(*t.first).into(), Term::from(*t.second).into()))
      }
      rinha::ast::Term::Var(v) => Term::Var(v.text),
    }
  }
}

impl std::fmt::Display for Term {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Term::Bool(value) => write!(f, "{}", value),
      Term::Int(value) => write!(f, "{}", value),
      Term::Str(value) => write!(f, "{}", value),
      Term::Tuple(Tuple(first, second)) => {
        write!(f, "({}, {})", first, second)
      }
      _ => write!(f, "{:?}", self),
    }
  }
}
