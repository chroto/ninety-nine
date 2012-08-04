-- # Problem 10
-- Run-length encoding of a list. Use the result of problem 
-- P09 to implement the so-called run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) where N is 
-- the number of duplicates of the element E.
import Test.HUnit
import Data.List

-- Solution (got this on the first try..I'm learning ⊙▃⊙
--
encode :: Eq a => [a] -> [ (Int, a) ]
encode [] = []
encode xs = map func (group xs)
    where
        func (x:xs) = (length (x:xs), x)
          

-- Tests

testcase1 = encode "aaaabccaadeeee"
assertion1 = [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

assertEmptyList :: (Eq a, Show a) => String -> [a] -> Assertion
assertEmptyList str xs = assertEqual str xs []

test1 = TestCase $ assertEqual "" testcase1 assertion1

tests = TestList [test1]

-- Main
main = runTestTT tests
