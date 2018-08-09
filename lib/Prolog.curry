------------------------------------------------------------------------------
--- A library defining a representation for Prolog programs together
--- with a simple pretty printer.
--- It does not cover all aspects of Prolog but might be useful
--- for applications generating Prolog programs.
---
--- @author Michael Hanus
--- @version August 14, 2014
--- @category general
------------------------------------------------------------------------------

module Prolog(PlClause(..), PlGoal(..), PlTerm(..), plList,
              showPlProg, showPlClause, showPlGoals, showPlGoal, showPlTerm)
  where

import Char(isAlphaNum,isLower)
import List(union,intercalate)

----------------------------------------------------------------------------
-- Representation of Prolog programs:

--- A Prolog clause is either a program clause consisting of a head
--- and a body, or a directive or a query without a head.
data PlClause = PlClause String [PlTerm] [PlGoal]
              | PlDirective [PlGoal]
              | PlQuery [PlGoal]

--- A Prolog goal is a literal, a negated goal, or a conditional.
data PlGoal = PlLit String [PlTerm]
            | PlNeg [PlGoal]
            | PlCond [PlGoal] [PlGoal] [PlGoal]

--- A Prolog term is a variable, atom, number, or structure.
data PlTerm = PlVar String
            | PlAtom String
            | PlInt Int
            | PlFloat Float
            | PlStruct String [PlTerm]

--- A Prolog list of Prolog terms.
plList :: [PlTerm] -> PlTerm
plList = foldr (\t ts -> PlStruct "." [t,ts]) (PlAtom "[]")

----------------------------------------------------------------------------
--- Shows a Prolog program in standard Prolog syntax.
showPlProg :: [PlClause] -> String
showPlProg clauses = unlines $ map (showPlClause . optimizeClause) clauses

showPlClause (PlClause pred args []) =
  showPlGoal (PlLit pred args) ++ "."
showPlClause (PlClause pred args body@(_:_)) =
  showPlGoal (PlLit pred args) ++ " :- " ++ showPlGoals body ++ "."
showPlClause (PlDirective body) =
  ":- " ++ showPlGoals body ++ "."
showPlClause (PlQuery body) =
  "?- " ++ showPlGoals body ++ "."

showPlGoals gs = intercalate ", " (map showPlGoal gs)

showPrimPlGoals []         = "true"
showPrimPlGoals [g]        = showPlGoal g
showPrimPlGoals gs@(_:_:_) = "(" ++ intercalate ", " (map showPlGoal gs) ++ ")"

showPlGoal (PlLit pred args) =
  if pred=="="
  then showPlTerm (args!!0) ++ "=" ++ showPlTerm (args!!1)
  else showPlTerm (PlStruct pred args)
showPlGoal (PlNeg goal) =
  "\\+" ++ showPrimPlGoals goal
showPlGoal (PlCond cond tgoal fgoal) =
  "(" ++ showPrimPlGoals cond ++ " -> " ++ showPlGoals tgoal ++ " ; " ++
  showPlGoals fgoal ++ ")"

showPlTerm (PlVar v)       = v
showPlTerm (PlAtom a)      = showPlAtom a
showPlTerm (PlInt i)       = show i
showPlTerm (PlFloat f)     = show f
showPlTerm (PlStruct f []) = showPlAtom f
showPlTerm (PlStruct f args@(h:t))
  | f=="." && length args == 2 -- a Prolog list
    = "[" ++ showPlTerm h ++ showPlListElems (head t) ++ "]"
  | (f=="," || all (`elem` specialChars) f) && length args == 2 -- infix op
    = "("++ showPlTerm (args!!0) ++ f ++ showPlTerm (args!!1) ++")"
  | otherwise = showPlAtom f ++"("++ intercalate "," (map showPlTerm args) ++")"

showPlListElems xs = case xs of
  PlAtom "[]" -> ""
  PlStruct f [y,ys] -> if f=="." then "," ++ showPlTerm y ++ showPlListElems ys
                                 else "|" ++ showPlTerm ys
  _                 -> "|" ++ showPlTerm xs

showPlAtom a =
  if a=="[]" || (all (\c -> isAlphaNum c || c=='_') a && isLower (head a))
             || all (`elem` specialChars) a
  then a
  else '\'': (concatMap (\c->if c=='\'' then "\\\'" else [c]) a) ++"\'"

specialChars = "+-*/<=>`\\:.?@#$&^~"

----------------------------------------------------------------------------
-- Optimize a Prolog clause: "head :- b1,X=Y,b2" is replaced by
-- "head :- b1,[X/Y]b2" if X does not occur in head and b1

optimizeClause :: PlClause -> PlClause
optimizeClause (PlClause pred args body) =
  PlClause pred args (optimizeBody (unionMap varsOf args) body)
optimizeClause (PlDirective body) = PlDirective (optimizeBody [] body)
optimizeClause (PlQuery     body) = PlQuery     (optimizeBody [] body)

optimizeBody _ [] = []
optimizeBody vars (PlCond cond tgoal fgoal : lits) =
 let ocond = optimizeBody vars cond
     ocvars = union vars (unionMap varsOfLit cond)
     otgoal = optimizeBody ocvars tgoal
     ofgoal = optimizeBody ocvars fgoal
  in PlCond ocond otgoal ofgoal
      : optimizeBody (union ocvars (unionMap varsOfLit (otgoal++ofgoal))) lits
optimizeBody vars (PlLit pred args : lits)
 | pred=="=" && isPlVar (head args) && (varOf (head args) `notElem` vars)
  = optimizeBody (union vars (varsOf (args!!1)))
                 (map (replaceInLit (varOf (head args)) (args!!1)) lits)
 | pred=="=" && isPlVar (args!!1) && (varOf (args!!1) `notElem` vars)
  = optimizeBody (union vars (varsOf (args!!0)))
                 (map (replaceInLit (varOf (args!!1)) (args!!0)) lits)
 | otherwise
  = PlLit pred args : optimizeBody (union vars (unionMap varsOf args)) lits

replaceInLit x y (PlLit pred args) = PlLit pred (map (replaceInTerm x y) args)
replaceInLit x y (PlCond cond tgoal fgoal) =
  PlCond (map (replaceInLit x y) cond)
         (map (replaceInLit x y) tgoal)
         (map (replaceInLit x y) fgoal)

replaceInTerm x y (PlVar   v) = if x==v then y else PlVar v
replaceInTerm _ _ (PlAtom  a) = PlAtom  a
replaceInTerm _ _ (PlInt   i) = PlInt   i
replaceInTerm _ _ (PlFloat f) = PlFloat f
replaceInTerm x y (PlStruct f args) = PlStruct f (map (replaceInTerm x y) args)

varsOfLit (PlLit _ args) = unionMap varsOf args
varsOfLit (PlCond g1 g2 g3) = unionMap varsOfLit (g1++g2++g3)

varsOf (PlVar   v) = [v]
varsOf (PlAtom  _) = []
varsOf (PlInt   _) = []
varsOf (PlFloat _) = []
varsOf (PlStruct _ args) = unionMap varsOf args

isPlVar t = case t of
              PlVar _ -> True
              _       -> False

varOf (PlVar v) = v

unionMap f = foldr union [] . map f

----------------------------------------------------------------------------
