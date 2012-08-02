-- # Problem 8
-- Eliminate consecutive duplicates of list elements.

import Test.HUnit

-- Solution

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = reverse $ foldl (\acc y -> if y == head acc then acc else (y:acc)) [x] xs

-- Tests

testcase1 = compress ["a"]
assertion1 = ["a"]

testcase2 = compress ["a", "a"]
assertion2 = ["a"]

testcase3 = compress ["a", "a", "b"]
assertion3 = ["a", "b"]

testcase4 = compress ["a", "a", "b", "a"]
assertion4 = ["a", "b", "a"]
testcase5 = compress ["a","a","a","a","b","c","c","a","a","d","e","e","e","e"]
assertion5 = ["a","b","c","a","d","e"]

assertEmptyList :: (Eq a, Show a) => String -> [a] -> Assertion
assertEmptyList str xs = assertEqual str xs []

test1 = TestCase $ assertEqual "" testcase1 assertion1
test2 = TestCase $ assertEqual "" testcase2 assertion2
test3 = TestCase $ assertEqual "" testcase3 assertion3
test4 = TestCase $ assertEqual "" testcase4 assertion4
test5 = TestCase $ assertEqual "" testcase5 assertion5

tests = TestList [test1, test2, test3, test4, test5]

-- Main
main = runTestTT tests
