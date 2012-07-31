-- # Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
import Test.HUnit

-- Function
elementAt :: [a] -> Int -> a
elementAt list i = list !! (i - 1)

elementAt' :: [a] -> Int -> a
elementAt' (x:_) 1 = x
elementAt' [] _ = error "nope"
elementAt' (_:xs) k
    | k < 1 = error "Index out of bounds"
    | otherwise = elementAt' xs (k - 1)

elementAt_w'pf = (last .) . take . (+ 1)


-- Tests
test1 = TestCase $ assertEqual "" (elementAt [1, 2, 3] 2) 2
test2 = TestCase $ assertEqual "" (elementAt "haskell" 5) 'e'

tests = TestList [test1, test2]

main = runTestTT tests
