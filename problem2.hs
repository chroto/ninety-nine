-- # Problem 2
-- Find the last but one element of a list.
import Test.HUnit

myButLast :: [a] -> a
myButLast [] = error "no"
myButLast [x,y] = x
myButLast (_:xs) = myButLast xs

test1 = TestCase $ assertEqual "myButLast" (myButLast [1, 2, 3, 4]) 3
test2 = TestCase $ assertEqual "myButLast" (myButLast ['a'..'z']) 'y'

tests = TestList [test1, test2]

main = runTestTT tests
