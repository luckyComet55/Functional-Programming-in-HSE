import System.Random
import Control.Monad
import Control.Monad.State

randomRState :: (Random a, RandomGen g) => (a, a) -> State g a 
randomRState (x,y) = do
    gen <- get
    let (a, gen') = randomR (x, y) gen
    put gen'
    return a

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
    l <- replicateM k $ iter n
    return $ sum l / fromIntegral k
    where
        iter i = do
            l <- replicateM i generateFlip
            return $ abs (fromIntegral i / 2 - fromIntegral (sum l))

generateFlip :: State StdGen Int
generateFlip = randomRState (0::Int, 1::Int)