type Ident = String

data Expr
  = Var Ident Integer -- x^i
  | Con Double
  | Neg Expr -- - e
  | Add Expr Expr -- e1 + e2
  | Rec Expr -- 1 / e
  | Mul Expr Expr -- e1 * e2

newtype EnvReader env a = EnvReader
  { runEnvReader :: env -> Maybe a
  }