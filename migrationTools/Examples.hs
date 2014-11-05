{-# LANGUAGE BangPatterns #-}
-- module Main where

import Data.List

mean' :: (Fractional a) => [a] -> a
mean' xs = mean'' xs 0 0 where
    mean'' (x:xs) !lengthAcc !sumAcc = mean'' xs (1 + lengthAcc) (x + sumAcc)
    mean'' [] !lengthAcc !sumAcc     = lengthAcc / sumAcc

palindrome' :: [a] -> [a]
palindrome' lst = lst ++ (reverse lst)
-- length' xs = length'' 0 xs where
--     length'' !acc (x:xs) = length'' (acc + 1) xs
--     length'' !acc [] = acc

main = do
    print "start"
    print $ palindrome' [1,2..] --00 000 000000000 00000000000000000
