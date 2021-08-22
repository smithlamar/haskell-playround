module Main where

-- Exercises from Whitington, John. Haskell from the Very Beginning . Coherent Press. Kindle Edition.
-- Navigate to the path of this file and use the command `cabal run` to compile and run this. 

myIdentity :: x -> x
myIdentity x = x
myIdentityOutput = myIdentity 5

timesTen :: Num a => a -> a
timesTen x = x * 10
timesTenOutput = timesTen 2


areBothFalse :: Bool -> Bool -> Bool
areBothFalse a b = not a && not b
areBothFalseOutput = areBothFalse False False


areBothPositive :: (Num a , Ord a) => a -> a -> Bool
areBothPositive a b = if a > 0 && b > 0 then True else False
areBothPositiveOutput = areBothPositive 0 3


sumIt :: (Num a, Eq a) => a -> a
sumIt a = if a == 1 then 1 else a + sumIt (a - 1)
sumItOutput = sumIt 4


toPowerOf :: Floating a => a -> a -> a
toPowerOf value exponent = value ** exponent
toPowerOfOutput = toPowerOf 2 (-1)

toPowerOfAlt :: Floating a => a -> a -> a
toPowerOfAlt = (**)
toPowerOfAltOutput = toPowerOf 2 (-1)


vowels = ['a', 'e', 'i', 'o', 'u']
isVowel char = any (char ==) vowels
isConsonant char = not (isVowel char)
isConsonantOutput = isConsonant 'b'


letOfLets = let x = 1 in let x = 2 in x + x
letOfLetsOutput = letOfLets


isVowel2 'a' = True
isVowel2 'e' = True
isVowel2 'i' = True
isVowel2 'o' = True
isVowel2 'u' = True
isVowel2  _  = False
isConsonant2 char = not (isVowel2 char)
isConsonant2Output = isConsonant2 'o'


sumMatch :: Integral a => a -> a
sumMatch n  | n < 1     = 0
            | otherwise = n + sumMatch (n - 1)
sumMatchOutput = sumMatch 10


-- Use guarded equations to write a function which categorises
-- characters into three kinds:
-- kind 0 for the lowercase letters a…z, kind 1 for the uppercase letters a…z, and kind 2 for everything else.
categorizeLetter char   | char >= 'a' && char <= 'z'    = 0
                        | char >= 'A' && char <= 'Z'    = 1
                        | otherwise                     = 2
categorizeLetterOutput = categorizeLetter 'G'


-- Questions from Chapter 4 - making lists

-- 1) write a function evenElements which returns the even elements from a list e.g. for list [2, 4, 2, 4, 2] return [4, 4]
evenElements :: [a] -> [a]
evenElements [] = []
evenElements [x] = []
evenElements (_:x:xs) = x : evenElements xs
evenElementsOutpout = evenElements [2, 4, 2, 4, 2]

-- 2) Write a function countTrue which counts the number of True elements in a list. For example, countTrue [True, False, True] should return 2.

boolToBinary x = if x == True then 1 else 0

countTrue :: Num a => [Bool] -> a
countTrue [] = 0
countTrue (x:xs) =  (boolToBinary x) + countTrue xs 
countTrueOutput = countTrue [True, False, True]


-- 3) Write a function which, given a list, builds a palindrome from it. A palindrome is a list which equals its own reverse. Write another function which determines if a list is a palindrome.

toPalindrome :: [a] -> [a]
toPalindrome [] = []
toPalindrome xs = xs ++ (reverse xs)
toPalindromeOutput = toPalindrome [1..4]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
isPalindromeOutput = isPalindrome toPalindromeOutput 

-- 4) Write a function dropLast which returns all but the last element of a list. If the list is empty, it should return the empty list. So, for example, dropLast [1, 2, 4, 8] should return [1, 2, 4].
dropLast :: [a] -> [a]
dropLast [] = []
dropLast [x] = [x]
dropLast xs = take (length xs - 1) xs
dropLastOutput = dropLast [1 .. 4]


-- 5) Write a function elem' of type Eq a ⇒ a → [a] → Bool which returns True if an element exists in a list, or False if not. For example, elem' 2 [1, 2, 3] should evaluate to True, but elem' 3 [1, 2] should evaluate to False.
elem' :: Eq a => a -> [a] -> Bool
elem' targetElement elements = any (targetElement ==) elements
elemOutput = elem' 1 [2, 3, 1]


-- 6) Use your elem' function to write a function makeSet which, given a list, returns a list which contains all the elements of the original list, but has no duplicate elements. For example, makeSet [1, 2, 3, 3, 1] might return [2, 3, 1].
makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs)  | elem x xs     = makeSet xs
                | otherwise     = x : makeSet xs 
makeSetOutput = makeSet [1, 1, 2, 3, 1, 2, 4]

-- Exercise Output
main :: IO ()
main = do
    -- chapters 1 - 3 output

    -- print myIdentityOutput
    -- print timesTenOutput
    -- print areBothFalseOutput
    -- print areBothPositiveOutput
    -- print sumItOutput
    -- print toPowerOfOutput
    -- print toPowerOfAltOutput
    -- print isConsonantOutput
    -- print letOfLetsOutput
    -- print isConsonant2Output
    -- print sumMatchOutput
    -- print categorizeLetterOutput

    -- Chapter 4 output
    print evenElementsOutpout
    print countTrueOutput
    print toPalindromeOutput
    print isPalindromeOutput
    print dropLastOutput
    print elemOutput
    print makeSetOutput