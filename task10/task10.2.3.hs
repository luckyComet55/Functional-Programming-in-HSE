import Control.Monad.State (execState, replicateM, State, MonadState (get, put))
fib :: Int -> Integer
fib n = fst $ execState (replicateM n fibStep) (0, 1)

fibStep :: State (Integer, Integer) ()
fibStep = do
    (a, b) <- get
    put (b, a + b)