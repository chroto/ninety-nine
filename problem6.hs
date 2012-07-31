-- # Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
import Test.HUnit

-- Solution
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- Tests

testcase1 = isPalindrome [1,2,3]
assertion1 = False

testcase2 = isPalindrome "madamimadam"
assertion2 = True


test1 = TestCase $ assertEqual "" (testcase1) assertion1
test2 = TestCase $ assertEqual "" (testcase2) assertion2

tests = TestList [test1, test2]

main = runTestTT tests
