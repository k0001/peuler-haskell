import Control.Applicative
import System.CPUTime
import System.Environment
import System.Exit
import Text.Printf

--
-- Utils
--

-- Ordered List: difference
minus :: (Ord a) => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of
    LT -> x : minus  xs  (y:ys)
    EQ ->     minus  xs     ys
    GT ->     minus (x:xs)  ys
minus xs _ = xs

-- Ordered List: union
union :: (Ord a) => [a] -> [a] -> [a]
union (x:xs) (y:ys) = case (compare x y) of
    LT -> x : union  xs  (y:ys)
    EQ -> x : union  xs     ys
    GT -> y : union (x:xs)  ys
union xs [] = xs
union [] ys = ys


-- Fibonacci series
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Primes: Eratosthenes Sieve
-- http://www.haskell.org/haskellwiki/Prime_numbers
primesTo :: Int -> [Int]
primesTo n = 2 : sieve [3,5..n] where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p.. n])

-- Primes: Factorization
primeFactors :: Int -> [Int]
primeFactors n = fact n 2 where
    fact n start
        | n == start         = [n]
        | n `mod` start == 0 = start : (fact (n `div` start) start)
        | start == 2         = fact n 3
        | otherwise          = fact n (start + 2)

-- Digits
digitsRev :: Int -> [Int]
digitsRev = map (`mod` 10) . takeWhile (>0) . iterate (`div` 10)

digits :: Int -> [Int]
digits = reverse . digitsRev

-- Reverse an integer
-- eg: 123 -> 321, 530 -> 35
intReverse :: Int -> Int
intReverse n = foldl (\acc x -> acc * 10 + x) 0 (digitsRev n) where

-- Palindromes: integer check
intIsPalindrome :: Int -> Bool
intIsPalindrome n = n == intReverse n


-- Problem solutions
--
-- Instructions for each problem at http://projecteuler.net/problem=N
--

p1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
-- 233168

p2 = sum [x | x <- takeWhile (<= 4000000) fibs, even x]
-- 4613732

p3 = last $ primeFactors 600851475143
-- 6857

p4 = maximum $ filter intIsPalindrome $ (*) <$> [100..999] <*> [100..999]
-- 906609

p5 = foldl1 lcm [1..20]
-- 232792560


problems :: [Int]
problems = [p1, p2, p3, p4, p5]

usageFailText :: String
usageFailText = printf "Sorry, pick one of [1-%d] problems." (length problems)

main = do
    args <- getArgs
    case args of
        []     -> putStrLn usageFailText >> exitFailure
        (pn:_) -> do
             let pn' = (read pn :: Int) in
                if pn' > 0 && pn' <= length problems then
                    printf "Problem %d: %d\n" pn' (problems !! (pn' - 1)) >>
                    exitWith ExitSuccess
                else
                    putStrLn usageFailText >> exitFailure

