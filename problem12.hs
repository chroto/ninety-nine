-- # Problem 12
-- Decode a run-length encoded list.
import Test.HUnit
import Data.List
import Encoding
-- Solution

decodeModified :: Eq a => [Encoded a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decode x ++ decodeModified xs
    where
        decode :: Eq a => Encoded a -> [a]
        decode (Single x) = [x]
        decode (Multiple n x) = take n (repeat x)

decodeModified' :: Eq a => [Encoded a] -> [a]
decodeModified' = concatMap decode
    where
        decode (Single x) = [x]
        decode (Multiple n x) = replicate n x

-- Tests
testcase1 = decodeModified
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
assertion1 = "aaaabccaadeeee"

test1 = TestCase $ assertEqual "" testcase1 assertion1

tests = TestList [test1]

-- Main
main = runTestTT tests
