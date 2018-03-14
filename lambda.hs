{-
  Lambda: Lambda calculus
  Copyright (C) 2018 Filip Kocina

  This file is part of Lambda.

  Lambda is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Lambda is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Lambda.  If not, see <http://www.gnu.org/licenses/>.
-}

type VarName = String
data LExpr
  = Var VarName -- variable
  | App LExpr LExpr -- application
  | Abs VarName LExpr -- abstraction
  deriving (Eq, Show)

subst :: VarName -> LExpr -> LExpr -> LExpr -- subst what with where = result
subst x l1 l2@(Var y) -- lambda expression is only a variable
  | x == y    = l1 -- substitute only if the same variable
  | otherwise = l2 -- leave intact otherwise
subst x l (App l1 l2) = (App ln1 ln2) -- substitute in both parts of the applic.
  where ln1 = subst x l l1
        ln2 = subst x l l2
subst x l1 l2@(Abs v e1) -- substitute in an abstraction until bound var. found
  | x == v    = l2 -- the variable is bound in subexpr.: nothing substituted
  | otherwise = (Abs v2 e2) -- the variable can be free
  where v2 = findVar v (freeVars l1) -- find a non-colliding variable name
        e2 = subst x l1 e3 -- substitute recursively
        e3 = subst v (Var v2) e1 -- rename var. v to var. v2 in e1

findVar :: VarName -> [VarName] -> VarName -- find a non-colliding var. name
findVar v [] = v
findVar v a
  | elem v a = findVar ("_"++v) a -- must be another
  | otherwise = v -- not colliding

freeVars :: LExpr -> [VarName] -- returns a list of free variables
freeVars (Var x) = [x] -- only one variable
freeVars (App l1 l2) = rmDup (a1++a2) -- free vars in both l1 and l2 returned
  where a1 = freeVars l1
        a2 = freeVars l2
freeVars (Abs x l) = rmVar x (freeVars l) -- free variables from l without x

rmDup :: [VarName] -> [VarName] -- remove duplicities from the list
rmDup [] = []
rmDup (x:xs) = [x]++(rmVar x xs2) -- use the head and remove it from the rest
  where xs2 = rmDup xs

rmVar :: VarName -> [VarName] -> [VarName] -- rmVar var from = result
rmVar x [] = []
rmVar x (a:as)
  | x == a    = rmVar x as -- remove if the same
  | otherwise = [a]++(rmVar x as) -- keep if not

beta :: LExpr -> LExpr -- perform beta reduction
beta (App (Abs v l1) l2) = subst v l2 l1
beta l = l -- cannot be performed

eta :: LExpr -> LExpr -- perform eta reduction
eta l2@(Abs v1 (App l1 (Var v2)))
  | v1 /= v2              = l2
  | elem v1 (freeVars l1) = l2 -- cannot be performed
  | otherwise             = l1 -- reduction OK
eta l = l -- cannot be performed

fix :: (LExpr -> LExpr) -> LExpr -> LExpr
fix f x -- find the fixed point of f starting with x
  | x == x2   = x -- fixed point found
  | otherwise = fix f x2 -- looking for another
  where x2 = f x -- call function f

reduce :: LExpr -> LExpr
reduce = fix (beta.eta) -- reduce LExpr to the normal form
