-- # Problem 11
-- Modify the result of problem 10 in such a way that if an element has no 
-- duplicates it is simply copied into the result list. Only elements with 
-- duplicates are transferred as (N E) lists.
import Test.HUnit
import Data.List

-- Solution
data Encoded a = Multiple Int a 
    | Single a
    deriving (Eq, Show)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified [] = []
encodeModified xs = map func (group xs)
    where
        func [x] = Single x
        func (x:xs) = (Multiple (length (x:xs)) x)

-- Tests
testcase1 = encodeModified "aaaabccaadeeee"
assertion1 = [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a',
    Single 'd', Multiple 4 'e']

test1 = TestCase $ assertEqual "" testcase1 assertion1

tests = TestList [test1]

-- Main
main = runTestTT tests
