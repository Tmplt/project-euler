{- Solution to Project Euler problem 3.
 -
 - The prime factors of 13195 are 5, 7, 7, 13 and 29.
 - What is the largest prime factor of the number 600851475143?
 -}

main = putStrLn (show ans)
ans = largestFactor (600851475143 :: Integer)

-- By the fundamental theorem of arithmetic...
smallestFactor n = head $ [ k | k <- [2 .. (truncate . sqrt . fromIntegral) n], mod n k == 0] ++ [n]

largestFactor n =
    let p = smallestFactor n in
    if p < n then largestFactor (n `div` p) else n
