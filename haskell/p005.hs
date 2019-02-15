{- Smallest multiple
 -
 - 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without a remainder.
 -
 - What is the smallest positive number that is *evenly divisible* by all numbers from 1 to 20?
 -}


main = putStrLn (show ans)

ans = head [ k | k <- [20, 40..], all (0 ==) [mod k n | n <- [20, 19..1]] ]
