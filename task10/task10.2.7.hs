import System.Random
import Control.Monad
import Control.Monad.State

avgdev'' :: Int -> Int -> Double
avgdev'' k n = helper k n generateFlip 0 n / fromIntegral k

gen :: StdGen
gen = mkStdGen 622

generateFlip :: [Int]
generateFlip = randomRs (0, 1) gen

helper :: Int -> Int -> [Int] -> Int -> Int -> Double
helper 0 _ _ _ _ = 0
helper _ _ [] _ _ = 0
helper k 0 (_ : xs) summ n' = abs (fromIntegral n' / 2 - fromIntegral summ) + helper (k - 1) n' xs 0 n'
helper k n (x : xs) summ n' = helper k (n - 1) xs (x + summ) n'