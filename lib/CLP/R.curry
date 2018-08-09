------------------------------------------------------------------------------
--- Library for constraint programming with arithmetic constraints over reals.
---
--- @author Michael Hanus
--- @version December 2016
--- @category general
------------------------------------------------------------------------------

module CLP.R(CFloat,minimumFor,minimize,maximumFor,maximize) where

-- The operator declarations are similar to the standard arithmetic operators.

infixl 7 *., /.
infixl 6 +., -.
infix  4 <., >., <=., >=.

--- Abstract type to represent floats used in constraints.
data CFloat = CF Float

instance Eq CFloat where
  (CF f1) == (CF f2) = f1 == f2

instance Ord CFloat where
  compare (CF f1) (CF f2) = compare f1 f2
  x < y = x <. y
  x > y = x >. y
  x <= y = x <=. y
  x >= y = x >=. y

instance Show CFloat where
  show (CF f) = show f

instance Num CFloat where
  x + y = x +. y
  x - y = x -. y
  x * y = x *. y

  negate (CF x) = CF (negateFloat x)

  abs x | x >= 0 = x
        | otherwise = negate x


  signum x | x > 0     = 1
           | x == 0    = 0
           | otherwise = -1

  fromInt x = i2f x

instance Fractional CFloat where
  x / y = x /. y

  fromFloat x = CF x

--- Addition on floats in arithmetic constraints.

(+.)   :: CFloat -> CFloat -> CFloat
(CF x) +. (CF y) = CF ((prim_CLPR_plus $! y) $! x)

prim_CLPR_plus :: Float -> Float -> Float
prim_CLPR_plus external

--- Subtraction on floats in arithmetic constraints.

(-.)   :: CFloat -> CFloat -> CFloat
(CF x) -. (CF y) = CF ((prim_CLPR_minus $! y) $! x)

prim_CLPR_minus :: Float -> Float -> Float
prim_CLPR_minus external

--- Multiplication on floats in arithmetic constraints.

(*.)   :: CFloat -> CFloat -> CFloat
(CF x) *. (CF y) = CF ((prim_CLPR_times $! y) $! x)

prim_CLPR_times :: Float -> Float -> Float
prim_CLPR_times external

--- Division on floats in arithmetic constraints.

(/.)   :: CFloat -> CFloat -> CFloat
(CF x) /. (CF y)  = CF ((prim_CLPR_div $! y) $! x)

prim_CLPR_div :: Float -> Float -> Float
prim_CLPR_div external

--- "Less than" constraint on floats.

(<.)   :: CFloat -> CFloat -> Bool
(CF x) <. (CF y) = (prim_CLPR_le $! y) $! x

prim_CLPR_le :: Float -> Float -> Bool
prim_CLPR_le external

--- "Greater than" constraint on floats.

(>.)   :: CFloat -> CFloat -> Bool
(CF x) >. (CF y) = (prim_CLPR_ge $! y) $! x

prim_CLPR_ge :: Float -> Float -> Bool
prim_CLPR_ge external

--- "Less than or equal" constraint on floats.

(<=.)  :: CFloat -> CFloat -> Bool
(CF x) <=. (CF y) = (prim_CLPR_leq $! y) $! x

prim_CLPR_leq :: Float -> Float -> Bool
prim_CLPR_leq external

--- "Greater than or equal" constraint on floats.

(>=.)  :: CFloat -> CFloat -> Bool
(CF x) >=. (CF y) = (prim_CLPR_geq $! y) $! x

prim_CLPR_geq :: Float -> Float -> Bool
prim_CLPR_geq external

--- Conversion function from integers to floats.
--- Rigid in the first argument, i.e., suspends until the first argument
--- is ground.

i2f    :: Int -> CFloat
i2f x = CF (prim_CLPR_i2f $# x)

prim_CLPR_i2f :: Int -> Float
prim_CLPR_i2f external


--- Computes the minimum with respect to a given constraint.
--- (minimumFor g f) evaluates to x if (g x) is satisfied and
--- (f x) is minimal. The evaluation fails if such a minimal value
--- does not exist. The evaluation suspends if it contains
--- unbound non-local variables.

minimumFor :: (a -> Bool) -> (a -> Float) -> a
minimumFor external

--- Minimization constraint.
--- (minimize g f x) is satisfied if (g x) is satisfied and
--- (f x) is minimal. The evaluation suspends if it contains
--- unbound non-local variables.

minimize :: (a -> Bool) -> (a -> Float) -> a -> Bool
minimize g f x = minimumFor g f =:= x

--- Computes the maximum with respect to a given constraint.
--- (maximumFor g f) evaluates to x if (g x) is satisfied and
--- (f x) is maximal. The evaluation fails if such a maximal value
--- does not exist. The evaluation suspends if it contains
--- unbound non-local variables.

maximumFor :: (a -> Bool) -> (a -> Float) -> a
maximumFor external

--- Maximization constraint.
--- (maximize g f x) is satisfied if (g x) is satisfied and
--- (f x) is maximal. The evaluation suspends if it contains
--- unbound non-local variables.

maximize :: (a -> Bool) -> (a -> Float) -> a -> Bool
maximize g f x = maximumFor g f =:= x


-- end of CLP.R
