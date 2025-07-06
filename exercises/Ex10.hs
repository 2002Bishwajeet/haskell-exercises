type Var = String

data Exp
  = EVar Var
  | ELam Var Exp
  | EApp Exp Exp
  deriving (Show, Eq)

remove :: (Eq a) => a -> [a] -> [a]
remove x [] = []
remove x (y : ys) | x == y = remove x ys
remove x (y : ys) = y : remove x ys

free :: Exp -> [Var]
free (EVar x) = [x]
free (ELam v e) = remove v $ free e
free (EApp e1 e2) = free e1 ++ free e2

fresh :: [Exp] -> Var
fresh es = head $ filter (`notElem` fvs) vars
  where
    fvs = concatMap free es
    vars = ["x" ++ show i | i <- [1 ..]]

step :: Exp -> Maybe Exp


steps :: Exp -> [Exp]
steps e = case step e of
  Nothing -> [e]
  Just e' -> e : steps e'