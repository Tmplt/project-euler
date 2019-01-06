{- Largest Palindrome Product
 -
 - A palindrome number reads the same both ways.
 - The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 x 99.
 -
 - Find the largest palindrome made from the product of two 3-digit numbers.
 - ----------------------------------------------------------------------------------------
 - This solution is general and does no list manipulations. Instead, the integer is "split"
 - and the first half is compared to a reversed second half. Implementation contains ad-hoc,
 - and is defenitely far from idiomic Haskell. For a proper solution (incl. pen and paper),
 - see <https://projecteuler.net/thread=4>.
 -
 - Completely disregarding lists because I associated them with a character string of the integer
 - was a bad idea. Lesson learned.
 -}

main = putStrLn (show ans)
ans = largestPalindromeProduct

-- e.g. 12345 -> 5
digitCount :: Int -> Int
digitCount n
  | n < 10 = 1
  | otherwise = 1 + digitCount (div n 10)

-- e.g. 12345 -> 54321
reverseDigits :: Int -> Int
reverseDigits n
  | n < 10 = n
  | otherwise = (mod n 10) * 10 ^ ((digitCount n) - 1) + reverseDigits (div n 10)

-- e.g. 10022001 -> True
isPalindrome :: Int -> Bool
isPalindrome n = fstHalf == sndHalfReversed
  where
    exp = digitCount n `div` 2
    -- If the digit count is odd, we ignore the middle-most digit.
    fstHalf = div n (10 ^ (exp + if digitCount n `mod` 2 /= 0 then 1 else 0))
    sndHalf = mod n (10 ^ exp)
    -- Reversing some halves (e.g. 09 from n = 9009) results in an invalid tenfold,
    -- increase it here if we're processing such a half.
    sndHalfReversed = let r = reverseDigits sndHalf in
                r * 10 ^ (maximum [exp - digitCount r, 0])

largestPalindromeProduct = maximum [ (i * j) | i <- [1..999], j <- [1..999], isPalindrome (i * j) ]
