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

main :: IO ()
main = do
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
    print evenElementsOutpout
    print countTrueOutput
