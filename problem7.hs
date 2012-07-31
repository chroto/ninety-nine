-- # Problem 7
-- Flatten a nested list structure.

import Test.HUnit

-- Solution
--
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- Tests

testcase1 = flatten (Elem 5)
assertion1 = [5]

testcase2 = flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
assertion2 = [1,2,3,4,5]

testcase3 :: Num a => [a]
testcase3 = flatten (List [])

assertEmptyList :: (Eq a, Show a) => String -> [a] -> Assertion
assertEmptyList str xs = assertEqual str xs []

test1 = TestCase $ assertEqual "" testcase1 assertion1
test2 = TestCase $ assertEqual "" testcase2 assertion2
test3 = TestCase $ assertEmptyList "" testcase3

tests = TestList [test1, test2, test3]

-- Main
main = runTestTT tests
