type Var = String

data Exp
  = EVar Var
  | ELam Var Exp
  | EApp Exp Exp
  deriving (Show, Eq)

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove x (y : ys) | x == y = remove x ys
remove x (y : ys) = y : remove x ys

free :: Exp -> [Var]
free (EVar x) = [x]
free (ELam x e) = remove x (free e)
free (EApp x y) = free x ++ free y

bound :: Exp -> [Var]
bound (EVar x) = []
bound (ELam x e) = x : bound e
bound (EApp e b) = bound e ++ bound b

-- Exercise 7.2.3 [5 Points] ---------------------------------------------------

-- Write a function `sub`, which performs capture avoiding substitution on
-- a term, i.e. `sub x e' e` should replace all occurences of `x` in `e` with `e'`.

-- You can make use of the following function to generate fresh variables:

fresh :: [Var] -> Var
fresh xs = head $ filter (`notElem` xs) $ map (\n -> "x" ++ show n) [1 :: Int ..]

-- You may want to use the `notElem` function, which is imported by default
-- and checks whether an element is not contained in a list.

-- You get partial points if the function performs substitution, but is not
-- capture-avoiding.

-- SUBMISSION BEGIN 7.2.3
sub :: Var -> Exp -> Exp -> Exp
sub x e' (EVar y)
  | x == y = e'
  | otherwise = EVar y
sub x e' (EApp e1 e2) = EApp (sub x e' e1) (sub x e' e2)
sub x e' (ELam y e) | x == y = ELam y e