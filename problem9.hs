-- # Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list 
-- contains repeated elements they should be placed in separate sublists.
import Test.HUnit

-- Solution
pack :: [a] -> [[a]]
pack = foldr pack' []
    where
     pack' [] = [ [] ]
     pack' x (y:ys) = if x == ( head y ) then ( (x:y):ys ) else ( [x]:y:ys )
          

-- Tests
testcase1 = pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
assertion1 = ["aaaa","b","cc","aa","d","eeee"]

assertEmptyList :: (Eq a, Show a) => String -> [a] -> Assertion
assertEmptyList str xs = assertEqual str xs []

test1 = TestCase $ assertEqual "" testcase1 assertion1

tests = TestList [test1]

-- Main
main = runTestTT tests
