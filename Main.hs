import Control.Applicative
import Control.Arrow
import Control.Monad (guard)
import Data.Char (digitToInt)
import Data.List (foldl', tails)
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
primesTo n = 2 : sieve [3,5..n]
  where sieve []     = []
        sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p.. n])

primes :: [Int]
primes = 2 : sieve [3,5..]
  where sieve []     = []
        sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p..])

-- Primes: Factorization
primeFactors :: Int -> [Int]
primeFactors n = fact n 2
  where fact n start
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
intReverse n = foldl' (\acc x -> acc * 10 + x) 0 (digitsRev n)

-- Palindromes: integer check
intIsPalindrome :: Int -> Bool
intIsPalindrome n = n == intReverse n

-- sliding 2 [1,2,3,4,5] == [[1,2],[2,3],[3,4],[4,5]]
-- sliding 3 [1,2,3]     == [[1,2,3]]
-- sliding 2 [1]         == []
-- sliding 1 [1]         == [[1]]
sliding :: Int -> [a] -> [[a]]
sliding n = filter ((>= n) . length) . map (take n) . tails

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
--

p4 = maximum $ filter intIsPalindrome $ (*) <$> [100..999] <*> [100..999]
-- 906609


p5 = foldl1 lcm [1..20]
-- 232792560


p6 = let (a, b) = ((^2) . sum) &&& (sum . map (^2)) $ [1..100]
        in a - b
-- 25164150

-- other approach using a monad to apply the same argument to more than one fun
p6' = foldr (-) 0 $ [(^2) . sum, sum . map (^2)] >>= return . ($ [1..100])


p7 = primes !! 10000
-- 104743

p8 = maximum $ map (product . map digitToInt) (sliding 5 num)
  where readInt x = read x :: Int
        num = "73167176531330624919225119674426574742355349194934\
              \96983520312774506326239578318016984801869478851843\
              \85861560789112949495459501737958331952853208805511\
              \12540698747158523863050715693290963295227443043557\
              \66896648950445244523161731856403098711121722383113\
              \62229893423380308135336276614282806444486645238749\
              \30358907296290491560440772390713810515859307960866\
              \70172427121883998797908792274921901699720888093776\
              \65727333001053367881220235421809751254540594752243\
              \52584907711670556013604839586446706324415722155397\
              \53697817977846174064955149290862569321978468622482\
              \83972241375657056057490261407972968652414535100474\
              \82166370484403199890008895243450658541227588666881\
              \16427171479924442928230863465674813919123162824586\
              \17866458359124566529476545682848912883142607690042\
              \24219022671055626321111109370544217506941658960408\
              \07198403850962455444362981230987879927244284909188\
              \84580156166097919133875499200524063689912560717606\
              \05886116467109405077541002256983155200055935729725\
              \71636269561882670428252483600823257530420752963450"

p9 = head [a*b*c | c <- [1..1000],
                   b <- [1..c-1],
                   a <- [1..b-1],
                   a + b + c == 1000,
                   a^2 + b^2 == c^2]



problems :: [Int]
problems = [p1, p2, p3, p4, p5, p6, p7, p8, p9]

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

