------------------------------------------------------------------------------
--- Some tests for library RedBlackTree.
---
--- To run all tests automatically by the currycheck tool, use the command:
--- "currycheck testRedBlackTree"
--- 
--- @author Bernd Brassel, Michael Hanus
--- @version October 2012
------------------------------------------------------------------------------

import Random
import List(nub)
import RedBlackTree
import Test.EasyCheck

intList2Tree = foldr update (empty (\ _ _ -> False) (==) (<))

rndTree n =
  getRandomSeed >>= return . nub . take n . (flip nextIntRange 100000) >>=
  \is -> return (intList2Tree is,is)

sorted [] = True
sorted [_] = True
sorted (x:y:xs) = x < y && sorted (y:xs)

rndDels n x = getRandomSeed >>= return . take n . (flip nextIntRange x)

deleteTest t _ [] = t
deleteTest t is (x:xs) = deleteTest (delete (is !! x) t) is xs

testIO m n =   
          rndTree m >>= \ (t,is) -> 
          rndDels n (length is) >>= \ ds ->
          let newt = deleteTest t is ds
           in return (sorted (tree2list newt))

-- Create tree with 1000 random entries, then randomly delete 100.
-- Test, if result is sorted.
testCreateRBTreeAndDeleteAndCheckSorted =
  (testIO 1000 100) `returns` True
