-- # Problem 14
-- Duplicate the elements of a list.
import Test.HUnit
--
-- Solution
twice = replicate 2

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = twice x ++ dupli xs

-- Tests
testcase1 = dupli [1,2,3]
assertion1 = [1,1,2,2,3,3]

test1 = TestCase $ assertEqual "" testcase1 assertion1

tests = TestList [test1]

-- Main
main = runTestTT tests
