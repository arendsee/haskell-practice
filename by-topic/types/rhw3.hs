#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

length' :: [a] -> Integer
length' []     = 0
length' (x:xs) = 1 + length' xs

sum' :: [Double] -> Double
sum' []     = 0
sum' (x:xs) = x + sum' xs

mean' :: [Double] -> Double
mean' [] = 0
mean' xs = sum' xs / (fromIntegral $ length' xs)

palindrome' :: [a] -> [a]
palindrome' [] = []
palindrome' x = x ++ reverse x

join' :: [[Char]] -> [Char] -> [Char] 
join' []     _ = ""
join' (x:[]) _ = x
join' (x:xs) d = x ++ d ++ (join' xs d)

main = do
    print $ length' "hi"
    print $ length' []
    print $ mean' [1.1,2,3]
    print $ palindrome' [1,2,3]
    print $ join' ["a", "asdf", "er"] "|"
