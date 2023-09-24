#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
  pub callee: Box<Term>,
  pub arguments: Vec<Term>,
  pub is_tail_call: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
  pub name: Option<String>,
  pub parameters: Vec<String>,
  pub value: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Let {
  pub name: String,
  pub value: Box<Term>,
  pub next: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct If {
  pub condition: Box<Term>,
  pub then: Box<Term>,
  pub otherwise: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
  Add, // Add
  Sub, // Subtract
  Mul, // Multiply
  Div, // Divide
  Rem, // Rem
  Eq,  // Equal
  Neq, // Not equal
  Lt,  // Less than
  Gt,  // Greater than
  Lte, // Less than or equal to
  Gte, // Greater than or equal to
  And, // And
  Or,  // Or
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Binary(pub Box<Term>, pub BinaryOp, pub Box<Term>);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Tuple(pub Box<Term>, pub Box<Term>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Term {
  Call(Call),
  Function(Function),
  Let(Let),
  If(If),
  Print(Box<Term>),
  Binary(Binary),
  Tuple(Tuple),
  Int(i32),
  Str(String),
  First(Box<Term>),
  Second(Box<Term>),
  Bool(bool),
  Var(String),
}

impl From<rinha::ast::Term> for Term {
  fn from(value: rinha::ast::Term) -> Self {
    fn go(value: rinha::ast::Term, is_tail_call: bool) -> Term {
      match value {
        rinha::ast::Term::Error(_) => unreachable!(),
        rinha::ast::Term::Int(i) => Term::Int(i.value),
        rinha::ast::Term::Str(s) => Term::Str(s.value),
        rinha::ast::Term::Call(c) => Term::Call(Call {
          callee: go(*c.callee, false).into(),
          arguments: c.arguments.into_iter().map(|a| a.into()).collect(),
          is_tail_call,
        }),
        rinha::ast::Term::Binary(b) => {
          Term::Binary(Binary(go(*b.lhs, false).into(), b.op.into(), go(*b.rhs, false).into()))
        }
        rinha::ast::Term::Let(rinha::ast::Let { name, value, next, .. }) => {
          let inner_value = match *value {
            rinha::ast::Term::Function(f) => Term::Function(Function {
              name: Some(name.text.clone()),
              parameters: f.parameters.into_iter().map(|p| p.text).collect(),
              value: go(*f.value, true).into(),
            }),
            _ => go(*value, is_tail_call),
          };
          Term::Let(Let {
            name: name.text,
            value: Box::new(inner_value),
            next: go(*next, is_tail_call).into(),
          })
        }
        rinha::ast::Term::Function(f) => Term::Function(Function {
          name: None,
          parameters: f.parameters.into_iter().map(|p| p.text).collect(),
          value: go(*f.value, true).into(),
        }),
        rinha::ast::Term::If(i) => Term::If(If {
          condition: go(*i.condition, false).into(),
          then: go(*i.then, is_tail_call).into(),
          otherwise: go(*i.otherwise, is_tail_call).into(),
        }),
        rinha::ast::Term::Print(p) => Term::Print(go(*p.value, false).into()),
        rinha::ast::Term::First(f) => Term::First(go(*f.value, false).into()),
        rinha::ast::Term::Second(s) => Term::Second(go(*s.value, false).into()),
        rinha::ast::Term::Bool(b) => Term::Bool(b.value),
        rinha::ast::Term::Tuple(t) => {
          Term::Tuple(Tuple(go(*t.first, false).into(), go(*t.second, false).into()))
        }
        rinha::ast::Term::Var(v) => Term::Var(v.text),
      }
    }
    go(value, false)
  }
}

impl From<rinha::ast::BinaryOp> for BinaryOp {
  fn from(value: rinha::ast::BinaryOp) -> Self {
    match value {
      rinha::ast::BinaryOp::Add => BinaryOp::Add,
      rinha::ast::BinaryOp::Sub => BinaryOp::Sub,
      rinha::ast::BinaryOp::Mul => BinaryOp::Mul,
      rinha::ast::BinaryOp::Div => BinaryOp::Div,
      rinha::ast::BinaryOp::Rem => BinaryOp::Rem,
      rinha::ast::BinaryOp::Eq => BinaryOp::Eq,
      rinha::ast::BinaryOp::Neq => BinaryOp::Neq,
      rinha::ast::BinaryOp::Lt => BinaryOp::Lt,
      rinha::ast::BinaryOp::Gt => BinaryOp::Gt,
      rinha::ast::BinaryOp::Lte => BinaryOp::Lte,
      rinha::ast::BinaryOp::Gte => BinaryOp::Gte,
      rinha::ast::BinaryOp::And => BinaryOp::And,
      rinha::ast::BinaryOp::Or => BinaryOp::Or,
    }
  }
}

impl std::fmt::Display for Term {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Term::Bool(value) => write!(f, "{}", value),
      Term::Int(value) => write!(f, "{}", value),
      Term::Str(value) => write!(f, "{}", value),
      Term::Function(_) => write!(f, "<#closure>"),
      Term::Tuple(Tuple(first, second)) => {
        write!(f, "({}, {})", first, second)
      }
      _ => write!(f, "{:?}", self),
    }
  }
}
