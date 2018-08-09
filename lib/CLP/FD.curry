------------------------------------------------------------------------------
--- Library for finite domain constraint solving.
---
--- An FD problem is specified as an expression of type `FDConstr`
--- using the constraints and expressions offered in this library.
--- FD variables are created by the operation `domain`.
--- An FD problem is solved by calling `solveFD` with labeling options,
--- the FD variables whose values should be included in the output,
--- and a constraint. Hence, the typical program structure to solve
--- an FD problem is as follows:
---
---     main :: [Int]
---     main =
---       let fdvars  = take n (domain u o)
---           fdmodel = {description of FD problem}
---        in solveFD {options} fdvars fdmodel
--- 
--- where `n` are the number of variables and `[u..o]` is the
--- range of their possible values.
---
--- @author Michael Hanus
--- @version December 2016
--- @category general
------------------------------------------------------------------------------

module CLP.FD
  ( FDConstr, FDExpr, FDRel(..), Option(..), fd
  , allDifferent, domain, sum, scalarProduct, count
  , (=#), (/=#), (<#), (<=#), (>#), (>=#)
  , true, (/\), andC, allC
  , solveFD, solveFDAll, solveFDOne
  ) where

import qualified CLPFD as C
import Findall (allValues, someValue)

infixl 7 *#
infixl 6 +#, -#
infix  4 =#, /=#, <#, <=#, >#, >=#
infixr 3 /\

------------------------------------------------------------------------------
--- Operations to construct basic constraints.

--- Returns infinite list of FDVars with a given domain.
--- @param min - minimum value for all variables in xs
--- @param max - maximum value for all variables in xs
domain :: Int -> Int -> [FDExpr]
domain min max = genFDVars min max
 where
  genFDVars :: Int -> Int -> [FDExpr]
  genFDVars l u = FDVar l u _ : genFDVars l u

instance Num FDExpr where
  x + y = x +# y
  x - y = x -# y
  x * y = x *# y

  abs _ = error "abs undefined for FD constraints"

  signum _ = error "signum undefined for FD constraints"

  fromInt x = fd x

--- Represent an integer value as an FD expression.
fd :: Int -> FDExpr
fd v = FDInt v

--- Addition of FD expressions.
(+#) :: FDExpr -> FDExpr -> FDExpr
x +# y = FDBinExp Plus x y

--- Subtraction of FD expressions.
(-#) :: FDExpr -> FDExpr -> FDExpr
x -# y = FDBinExp Minus x y

--- Multiplication of FD expressions.
(*#) :: FDExpr -> FDExpr -> FDExpr
x *# y = FDBinExp Times x y

--- Equality of FD expressions.
(=#) :: FDExpr -> FDExpr -> FDConstr
x =# y = FDRelCon Equ x y

--- Disequality of FD expressions.
(/=#) :: FDExpr -> FDExpr -> FDConstr
x /=# y = FDRelCon Neq x y

--- "Less than" constraint on FD expressions.
(<#) :: FDExpr -> FDExpr -> FDConstr
x <# y = FDRelCon Lt x y

--- "Less than or equal" constraint on FD expressions.
(<=#) :: FDExpr -> FDExpr -> FDConstr
x <=# y = FDRelCon Leq x y

--- "Greater than" constraint on FD expressions.
(>#) :: FDExpr -> FDExpr -> FDConstr
x ># y = FDRelCon Gt x y

--- "Greater than or equal" constraint on FD expressions.
(>=#) :: FDExpr -> FDExpr -> FDConstr
x >=# y = FDRelCon Geq x y

--- The always satisfied FD constraint.
true :: FDConstr
true = FDTrue

--- Conjunction of FD constraints.
(/\) :: FDConstr -> FDConstr -> FDConstr
c1 /\ c2 = FDAnd c1 c2

--- Conjunction of a list of FD constraints.
andC :: [FDConstr] -> FDConstr
andC = foldr (/\) true

--- Maps a constraint abstraction to a list of FD constraints and joins them.
allC :: (a -> FDConstr) -> [a] -> FDConstr
allC c = andC . map c

---------------------------------------------------------------------------
-- Complex constraints.

--- Possible relations between FD values.
--- @cons Equ - Equal
--- @cons Neq - Not equal
--- @cons Lt  - Less than
--- @cons Leq - Less than or equal
--- @cons Gt  - Greater than
--- @cons Geq - Greater than or equal
data FDRel = Equ | Neq | Lt | Leq | Gt | Geq

--- "All different" constraint on FD variables.
--- @param xs - list of FD variables
--- @return satisfied if the FD variables in the argument list xs
---         have pairwise different values.
allDifferent :: [FDExpr] -> FDConstr
allDifferent vs = FDAllDiff vs

--- Relates the sum of FD variables with some integer of FD variable.
sum :: [FDExpr] -> FDRel -> FDExpr -> FDConstr
sum vs rel v = FDSum vs rel v

--- `(scalarProduct cs vs relop v)` is satisfied if `(sum (cs*vs) relop v)`
--- is satisfied.
--- The first argument must be a list of integers. The other arguments are as
--- in 'sum'.
scalarProduct :: [FDExpr] -> [FDExpr] -> FDRel -> FDExpr -> FDConstr
scalarProduct cs vs rel v = FDScalar cs vs rel v

--- `(count v vs relop c)` is satisfied if `(n relop c)`,
--- where `n` is the number of elements in the
--- list of FD variables `vs` that are equal to `v`, is satisfied.
--- The first argument must be an integer. The other arguments are as
--- in 'sum'.
count :: FDExpr -> [FDExpr] -> FDRel -> FDExpr -> FDConstr
count v vs rel c = FDCount v vs rel c

---------------------------------------------------------------------------
--- This datatype defines options to control the instantiation of
--- FD variables in the solver ('solveFD').
---
--- @cons LeftMost - The leftmost variable is selected for instantiation (default)
--- @cons FirstFail - The leftmost variable with the smallest domain is selected
---                   (also known as first-fail principle)
--- @cons FirstFailConstrained - The leftmost variable with the smallest domain
---                              and the most constraints on it is selected.
--- @cons Min - The leftmost variable with the smalled lower bound is selected.
--- @cons Max - The leftmost variable with the greatest upper bound is selected.
--- @cons Step - Make a binary choice between `x=#b` and
---              `x/=#b` for the selected variable
---              `x` where `b` is the lower or
---              upper bound of `x` (default).
--- @cons Enum - Make a multiple choice for the selected variable for all the values
---              in its domain.
--- @cons Bisect - Make a binary choice between `x&lt;=#m` and
---                `x&gt;#m` for the selected variable
---                `x` where `m` is the midpoint
---                of the domain `x`
---                (also known as domain splitting).
--- @cons Up - The domain is explored for instantiation in ascending order (default).
--- @cons Down - The domain is explored for instantiation in descending order.
--- @cons All - Enumerate all solutions by backtracking (default).
--- @cons Minimize v - Find a solution that minimizes the domain variable v
---                    (using a branch-and-bound algorithm).
--- @cons Maximize v - Find a solution that maximizes the domain variable v
---                    (using a branch-and-bound algorithm).
--- @cons Assumptions x - The variable x is unified with the number of choices
---                       made by the selected enumeration strategy when
---                       a solution is found.
--- @cons RandomVariable x - Select a random variable for instantiation
---                          where `x` is a seed value for the random numbers
---                          (only supported by SWI-Prolog).
--- @cons RandomValue x - Label variables with random integer values
---                       where `x` is a seed value for the random numbers
---                       (only supported by SWI-Prolog).


data Option = LeftMost
            | FirstFail
            | FirstFailConstrained
            | Min
            | Max
            | Step
            | Enum
            | Bisect
            | Up
            | Down
            | All
            | Minimize Int
            | Maximize Int
            | Assumptions Int
            | RandomVariable Int
            | RandomValue Int

instance Show Option where
  show _ = error "TODO: Show CLP.FD.Option"

------------------------------------------------------------------------
-- Abstract types to represent FD constraints as data.

-- Abstract type for FD expressions:
data FDExpr = FDVar Int Int Int -- variable with domain and value
            | FDInt Int
            | FDBinExp FDOp FDExpr FDExpr

instance Show FDExpr where
  show _ = error "TODO: Show CLP.FD.FDExpr"

data FDOp = Plus | Minus | Times

-- Abstract type for FD constraints:
data FDConstr = FDTrue
  | FDRelCon  FDRel FDExpr FDExpr
  | FDAnd     FDConstr FDConstr
  | FDAllDiff [FDExpr]
  | FDSum     [FDExpr]          FDRel FDExpr
  | FDScalar  [FDExpr] [FDExpr] FDRel FDExpr
  | FDCount   FDExpr   [FDExpr] FDRel FDExpr

------------------------------------------------------------------------
-- The solver is implemented by a transformation into the old CLPFD solver:


--- Computes (non-deterministically) a solution for the FD variables (second
--- argument) w.r.t. constraint (third argument), where
--- the values in the solution correspond to the list of FD variables.
--- The first argument contains options to control the labeling/instantiation
--- of FD variables.
solveFD :: [Option] -> [FDExpr] -> FDConstr -> [Int]
solveFD options cvars constr =
  let domconstr = all (\ (FDVar l u v) -> C.domain [v] l u) (allFDVars constr)
      tconstr = trC constr
      allvars = map getFDVal cvars
      labelvars = C.labeling (map trO options) allvars
   in (domconstr & tconstr & labelvars) &> allvars

--- Computes all solutions for the FD variables (second
--- argument) w.r.t. constraint (third argument), where
--- the values in each solution correspond to the list of FD variables.
--- The first argument contains options to control the labeling/instantiation
--- of FD variables.
solveFDAll :: [Option] -> [FDExpr] -> FDConstr -> [[Int]]
solveFDAll options cvars constr = allValues (solveFD options cvars constr)

--- Computes a single solution for the FD variables (second
--- argument) w.r.t. constraint (third argument), where
--- the values in the solution correspond to the list of FD variables.
--- The first argument contains options to control the labeling/instantiation
--- of FD variables.
solveFDOne :: [Option] -> [FDExpr] -> FDConstr -> [Int]
solveFDOne options cvars constr = someValue (solveFD options cvars constr)

-- get value (possibly an unbound variable) of an FD expression:
getFDVal :: FDExpr -> Int
getFDVal var = case var of
  FDVar _ _ v -> v
  FDInt i     -> i
  FDBinExp fdop fde1 fde2 -> (arithOp fdop) (valOf fde1) (valOf fde2)
 where
  valOf e = case e of
    FDInt i -> i
    FDBinExp op e1 e2 -> (arithOp op) (valOf e1) (valOf e2)
    _ -> error $ "FD variable or value expected but FD expression found:\n"++
                 show e

  arithOp Plus = (+)
  arithOp Minus = (-)
  arithOp Times = (*)

-- compute (multi-set) of all variables occurring in a constraint:
allFDVars :: FDConstr -> [FDExpr]
allFDVars FDTrue = []
allFDVars (FDRelCon _ fde1 fde2)     = allEFDVars fde1 ++ allEFDVars fde2
allFDVars (FDAnd      c1 c2)         = allFDVars c1 ++ allFDVars c2
allFDVars (FDAllDiff fdvars)         = filterVars fdvars
allFDVars (FDSum fdvars _ fdv)       = filterVars (fdvars ++ [fdv])
allFDVars (FDScalar cs fdvars _ fdv) = filterVars (cs ++ fdvars ++ [fdv])
allFDVars (FDCount fdv fdvars _ c)   = filterVars (fdvars ++ [fdv,c])

-- filter variables in a list of FD expressions
filterVars :: [FDExpr] -> [FDExpr]
filterVars = concatMap allEFDVars

allEFDVars :: FDExpr -> [FDExpr]
allEFDVars e = case e of
  FDVar _ _ _      -> [e]
  FDInt _          -> []
  FDBinExp _ e1 e2 -> allEFDVars e1 ++ allEFDVars e2

trC :: FDConstr -> Bool
trC FDTrue = True
trC (FDRelCon rel fde1 fde2) = (trFDRel rel) (trE fde1) (trE fde2)
trC (FDAnd fde1 fde2) = trC fde1 & trC fde2
trC (FDAllDiff fdvars) = C.allDifferent (map getFDVal fdvars)
trC (FDSum fdvars relop fdv) =
  C.sum (map getFDVal fdvars)
        (trFDRel relop) --(\e1 e2 -> trC (rel (FDInt e1) (FDInt e2)))
        (getFDVal fdv)
trC (FDScalar cs fdvars relop fdv) =
  C.scalarProduct (map getFDVal cs) (map getFDVal fdvars) (trFDRel relop)
                  (getFDVal fdv)
trC (FDCount fdv fdvars relop c) =
  C.count (getFDVal fdv) (map getFDVal fdvars) (trFDRel relop) (getFDVal c)

trE :: FDExpr -> Int
trE (FDVar _ _ v) = v
trE (FDInt i) = i
trE (FDBinExp rel fde1 fde2) = (trFDOp rel) (trE fde1) (trE fde2)

trFDRel :: FDRel -> Int -> Int -> Bool
trFDRel Equ = (C.=#)
trFDRel Neq = (C./=#)
trFDRel Lt  = (C.<#)
trFDRel Leq = (C.<=#)
trFDRel Gt  = (C.>#)
trFDRel Geq = (C.>=#)

trFDOp :: FDOp -> Int -> Int -> Int
trFDOp Plus  = (C.+#)
trFDOp Minus = (C.-#)
trFDOp Times = (C.*#)

trO :: Option -> C.LabelingOption
trO option = case option of
  LeftMost             -> C.LeftMost
  FirstFail            -> C.FirstFail
  FirstFailConstrained -> C.FirstFailConstrained
  Min                  -> C.Min
  Max                  -> C.Max
  Step                 -> C.Step
  Enum                 -> C.Enum                
  Bisect               -> C.Bisect              
  Up                   -> C.Up          
  Down                 -> C.Down                
  All                  -> C.All         
  Minimize n           -> C.Minimize n  
  Maximize n           -> C.Maximize n  
  Assumptions n        -> C.Assumptions n       
  RandomVariable n     -> C.RandomVariable n
  RandomValue n        -> C.RandomValue n

------------------------------------------------------------------------
