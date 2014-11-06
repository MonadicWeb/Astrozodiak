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

isPalindrome :: (Show a, Eq a) => [a] -> Bool
isPalindrome lst = isPalindrome' lst [] [] where
    --isPalindrome' heads equals left
    isPalindrome' :: (Show a, Eq a) => [a] -> [a] -> [a] -> Bool
    isPalindrome' []     [] [] = True
    isPalindrome' []     _  [] = True
    isPalindrome' []     _  _  = False
    isPalindrome' [_]    [] [] = False
    isPalindrome' (x:y:xs) e [] 
        | x == y    = isPalindrome' xs     (x:e) []
        | otherwise = isPalindrome' (y:xs) e     [x]
    isPalindrome' (x:xs) e (l:left)
        | x == l    = trace' 1 $ isPalindrome' xs (x:e) left
        | otherwise = trace' 2 $ isPalindrome' xs e     (x : l : left ++ e ++ reverse e) where
            trace' indx a = trace (show (indx) ++ " a:" ++ show (x:xs) ++ " b:" ++ show e ++ " c:" ++ show (l:left)) a

main = do
    --print "start"
    -- print $ (reverse [] :: [Int])
    -- print $ (isPalindrome ([] :: [Int])) == True
    -- print $ isPalindrome [2] == False
    -- print $ isPalindrome [2,2,3,2,2] == False
    -- print $ isPalindrome [2,2,3,3,2,2] == True
    -- print $ isPalindrome [4,1,2,2,1,5] == False
    -- print $ isPalindrome [4,1,2,2,1,4] == True
    -- print $ isPalindrome [4,1,2,3,2,1,4] == False
    print $ (isPalindrome $ palindrome' [4,1,2,3,4,3,2,1]) == False
