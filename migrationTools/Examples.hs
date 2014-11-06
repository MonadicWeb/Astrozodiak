{-# LANGUAGE BangPatterns #-}
-- module Main where

import Data.List
import Debug.Trace

mean' :: (Fractional a) => [a] -> a
mean' xs = mean'' xs 0 0 where
    mean'' (x:xs) !lengthAcc !sumAcc = mean'' xs (1 + lengthAcc) (x + sumAcc)
    mean'' [] !lengthAcc !sumAcc     = lengthAcc / sumAcc

palindrome' :: [a] -> [a]
palindrome' lst = lst ++ (reverse lst)
-- length' xs = length'' 0 xs where
--     length'' !acc (x:xs) = length'' (acc + 1) xs
--     length'' !acc [] = acc

--isPalindrome :: [a] -> Bool
-- isPalindrome [] = True
-- isPalindrome l = recurse l == Just []
--     where
--     recurse (x:[]) = Just [x]
--     recurse (x:xs)
--      | x == head xs = Just (tail xs) -- encounters the first double
--      | otherwise = case (recurse xs) of
--         Just (y:ys) | x == y -> Just (ys) -- roll the tail back to the previous item
--         _ -> Nothing

trace'' :: String -> a -> a
--trace'' str a = trace str a
trace'' str a = a

isPalindrome :: (Show a, Eq a) => [a] -> Bool
isPalindrome lst = isPalindrome' lst [] [] where
    --isPalindrome' heads equals left
    isPalindrome' :: (Show a, Eq a) => [a] -> [a] -> [a] -> Bool
    isPalindrome' []     [] [] = trace'' "True - a:[] b:[] c:[]" True
    isPalindrome' []     e  [] = trace'' ("True - a:[] b:" ++ show e ++ " c:[]") True
    isPalindrome' []     e  l  = trace'' ("False - a:[] b:" ++ show e ++ " c:" ++ show l) False
    isPalindrome' [el]   [] [] = trace'' ("False - a:" ++ show el ++ " b:[] c:[]") False
    isPalindrome' (x:y:xs) e [] 
        | x == y && null e = trace' 1 $ isPalindrome' xs     (x:e) []
        | x == y           = trace' 2 $ isPalindrome' xs     [x]   (e ++ reverse e)
        | otherwise        = trace' 3 $ isPalindrome' (y:xs) e     [x] where
        trace' indx a = trace'' (show (indx) ++ " a:" ++ show (x:y:xs) ++ " b:" ++ show e ++ " c:[]") a
    isPalindrome' (x:xs) e (l:left)
        | x == l    = trace' 11 $ isPalindrome' xs     (x:e) left
        | null e    = trace' 12 $ isPalindrome' xs     []    (x : l : left)
        | otherwise = trace' 13 $ isPalindrome' (x:xs) []    (e ++ reverse e ++ (l : left)) where
            trace' indx a = trace'' (show (indx) ++ " a:" ++ show (x:xs) ++ " b:" ++ show e ++ " c:" ++ show (l:left)) a

main = do
    --print "start"
    print $ (reverse [] :: [Int])
    print $ (isPalindrome ([] :: [Int])) == True
    print $ isPalindrome [2] == False
    print $ isPalindrome [2,2,3,2,2] == False
    print $ isPalindrome [2,2,3,3,2,2] == True
    print $ isPalindrome [4,1,2,2,1,5] == False
    print $ isPalindrome [4,1,2,2,1,4] == True
    print $ isPalindrome [4,1,2,3,2,1,4] == False
    print $ (isPalindrome $ palindrome' [4,1,2,2,1]) == True
