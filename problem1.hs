import Test.HUnit

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

test1 = TestCase $ assertEqual "myLast" (myLast [1, 2, 3]) 3
test2 = TestCase $ assertEqual "myLast" (myLast ['a', 'b', 'c']) 'c'

tests = TestList [test1, test2]

main = runTestTT tests
