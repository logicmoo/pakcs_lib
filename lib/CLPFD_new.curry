------------------------------------------------------------------------------
--- Library for finite domain constraint solving.
--- <p>
--- The general structure of a specification of an FD problem is as follows:
--- 
--- <code>domain_constraint & fd_constraint & labeling</code>
--- 
--- where:
--- 
--- <code>domain_constraint</code>
--- specifies the possible range of the FD variables (see constraint <code>domain</code>)
--- 
--- <code>fd_constraint</code>
--- specifies the constraint to be satisfied by a valid solution
--- (see constraints #+, #-, all_different, etc below)
--- 
--- <code>labeling</code>
--- is a labeling function to search for a concrete solution.
---
--- Note: This library is based on the corresponding library of Sicstus-Prolog
--- but does not implement the complete functionality of the Sicstus-Prolog library.
--- However, using the PAKCS interface for external functions, it is relatively
--- easy to provide the complete functionality.
------------------------------------------------------------------------------

module CLPFD_new(domain, (+#), (-#), (*#), (=#), (/=#), (<#), (<=#), (>#), (>=#),
             sum, scalar_product, all_different, count,
             indomain, labeling, LabelingOption(..)) where

-- The operator declarations are similar to the standard arithmetic operators.

infixl 7 *#
infixl 6 +#, -#
infix  4 =#, /=#, <#, <=#, >#, >=#



--- Constraint to specify the domain of all finite domain variables.
--- @param xs - list of finite domain variables
--- @param min - minimum value for all variables in xs
--- @param max - maximum value for all variables in xs

domain :: [Int] -> Int -> Int -> Success
domain vs l u = seq (normalForm (ensureSpine vs))
                    (seq (ensureNotFree l)
                         (seq (ensureNotFree u) (prim_domain vs l u)))

prim_domain :: [Int] -> Int -> Int -> Success
prim_domain external

--- Addition of FD variables.
(+#)   :: Int -> Int -> Int
(+#) external

--- Subtraction of FD variables.
(-#)   :: Int -> Int -> Int
(-#) external

--- Multiplication of FD variables.
(*#)   :: Int -> Int -> Int
(*#) external

--- Equality of FD variables.
(=#)   :: Int -> Int -> Success
(=#) external

--- Disequality of FD variables.
(/=#)  :: Int -> Int -> Success
(/=#) external

--- "Less than" constraint on FD variables.
(<#)   :: Int -> Int -> Success
(<#) external

--- "Less than or equal" constraint on FD variables.
(<=#)  :: Int -> Int -> Success
(<=#) external

--- "Greater than" constraint on FD variables.
(>#)   :: Int -> Int -> Success
(>#) external

--- "Greater than or equal" constraint on FD variables.
(>=#)  :: Int -> Int -> Success
(>=#) external

--- Relates the sum of FD variables with some integer of FD variable.
sum :: [Int] -> (Int -> Int -> Success) -> Int -> Success
sum vs rel v = seq (normalForm (ensureSpine vs))
                   (seq (ensureNotFree rel) (seq v (prim_sum vs rel v)))

prim_sum :: [Int] -> (Int -> Int -> Success) -> Int -> Success
prim_sum external

--- (scalar_product cs vs relop v) is satisfied if ((cs*vs) relop v) is satisfied.
--- The first argument must be a list of integers. The other arguments are as
--- in <code>sum</code>.
scalar_product :: [Int] -> [Int] -> (Int -> Int -> Success) -> Int -> Success
scalar_product cs vs rel v =
  seq (groundNormalForm cs)
      (seq (normalForm (ensureSpine vs))
           (seq (ensureNotFree rel) (seq v (prim_scalar_product cs vs rel v))))

prim_scalar_product :: [Int] -> [Int] -> (Int -> Int -> Success) -> Int -> Success
prim_scalar_product external


--- (count v vs relop c) is satisfied if (n relop c), where n is the number of
--- elements in the list of FD variables vs that are equal to v, is satisfied.
--- The first argument must be an integer. The other arguments are as
--- in <code>sum</code>.
count :: Int -> [Int] -> (Int -> Int -> Success) -> Int -> Success
count v vs rel c =
  seq (ensureNotFree v)
      (seq (normalForm (ensureSpine vs))
           (seq (ensureNotFree rel) (seq c (prim_count v vs rel c))))

prim_count :: Int -> [Int] -> (Int -> Int -> Success) -> Int -> Success
prim_count external


--- "All different" constraint on FD variables.
--- @param xs - list of FD variables
--- @return satisfied if the FD variables in the argument list xs
---         have pairwise different values.

all_different :: [Int] -> Success
all_different vs = seq (normalForm (ensureSpine vs)) (prim_all_different vs)

prim_all_different :: [Int] -> Success
prim_all_different external

--- Instantiate a single FD variable to its values in the specified domain.
indomain :: Int -> Success
indomain x = seq x (prim_indomain x)

prim_indomain :: Int -> Success
prim_indomain external



--- Instantiate FD variables to their values in the specified domain.
--- @param options - list of option to control the instantiation of FD variables
--- @param xs - list of FD variables that are non-deterministically
---             instantiated to their possible values.

labeling :: [LabelingOption] -> [Int] -> Success
labeling options vs = seq (normalForm (map ensureNotFree (ensureSpine options)))
                          (seq (normalForm (ensureSpine vs)) (prim_labeling options vs))

prim_labeling :: [LabelingOption] -> [Int] -> Success
prim_labeling external

--- This datatype contains all options to control the instantiated of FD variables
--- with the enumeration constraint <code>labeling</code>.
--- @cons LeftMost - The leftmost variable is selected for instantiation (default)
--- @cons FirstFail - The leftmost variable with the smallest domain is selected
---                   (also known as first-fail principle)
--- @cons FirstFailConstrained - The leftmost variable with the smallest domain
---                              and the most constraints on it is selected.
--- @cons Min - The leftmost variable with the smalled lower bound is selected.
--- @cons Max - The leftmost variable with the greatest upper bound is selected.
--- @cons Step - Make a binary choice between x=#b and x/=#b for the selected variable
---              x where b is the lower or upper bound of x (default).
--- @cons Enum - Make a multiple choice for the selected variable for all the values
---              in its domain.
--- @cons Bisect - Make a binary choice between x<=#m and x>#m for the selected variable
---                x where m is the midpoint of the domain x
---                (also known as domain splitting).
--- @cons Up - The domain is explored for instantiation in ascending order (default).
--- @cons Down - The domain is explored for instantiation in descending order.
--- @cons All - Enumerate all solutions by backtracking (default).
--- @cons Minimize v - Find a solution that minimizes the domain variable v
---                    (using a branch-and-bound algorithm).
--- @cons Maximize v - Find a solution that maximizes the domain variable v
---                    (using a branch-and-bound algorithm).
--- @cons Assumptions x - The variable x is unified with the number of choices
---                       made by the selected enumeration strategy when a solution
---                       is found.

data LabelingOption = LeftMost
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


-- end of program
