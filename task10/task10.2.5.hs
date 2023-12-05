import Control.Monad
import System.Random

avgdev :: Int -> Int -> IO Double
avgdev k n = do
    l <- replicateM k $ iter n
    return $ sum l / fromIntegral k
    where
        iter i = do
            l <- replicateM i generateFlip
            return $ abs (fromIntegral i / 2 - fromIntegral (sum l))

generateFlip :: IO Int
generateFlip = randomRIO (0, 1) :: IO Int