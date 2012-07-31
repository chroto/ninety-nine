-- # Problem 4
-- Find the number of elements of a list.
import Test.HUnit

-- Solution
myLength :: [a] -> Int
myLength [] = 0
myLength list = foldl (\acc _ -> acc + 1) 0 list

-- with curried const func
myLength' list = foldr (const (+1)) 0

-- Tests

test1 = TestCase $ assertEqual "" (myLength [123, 456, 789]) 3
test2 = TestCase $ assertEqual "" (myLength "Hello, world!") 13

tests = TestList [test1, test2]

main = runTestTT tests
