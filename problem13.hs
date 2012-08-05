-- # Problem 12
-- Decode a run-length encoded list.
import Test.HUnit
import Data.List
import Encoding
-- Solution

encode :: Eq a => [a] -> [ (Int, a) ]
encode = foldr func []
    where
        func x [] = [(1,x)]
        func x (y@(n,a):ys)
            |   x == a    = (1 + n, x):ys
            |   otherwise = (1,x):y:ys

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = map convertToEncodedType . encode
    where
        convertToEncodedType (1,x) = Single x
        convertToEncodedType (n,x) = Multiple n x

-- Tests
testcase1 = encodeDirect "aaaabccaadeeee"
assertion1 = 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']

test1 = TestCase $ assertEqual "" testcase1 assertion1

tests = TestList [test1]

-- Main
main = runTestTT tests
