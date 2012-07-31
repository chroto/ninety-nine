-- # Problem 5
-- Reverse a list
import Test.HUnit

-- Solution
reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

-- Prelude def
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- more readable prelude def
reverse''' :: [a] -> [a]
reverse''' list = reverse'''' list []
    where
        reverse'''' [] reversed = reversed
        reverse'''' (x:xs) reversed = reverse'''' xs (x:reversed)

-- Tests

test1 = TestCase $ assertEqual "" (reverse' "A man, a plan, a canal, panama!") "!amanap ,lanac a ,nalp a ,nam A"
test2 = TestCase $ assertEqual "" (reverse' [1,2,3,4]) [4,3,2,1]

tests = TestList [test1, test2]

main = runTestTT tests
