import Control.Monad.Trans.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import Data.Foldable (msum)
import System.IO
import Data.Char (isNumber, isPunctuation)

type PwdErrorIOMonad = ExceptT PwdError IO

instance Semigroup PwdError where
     (PwdError e) <> _ = PwdError e

instance Monoid PwdError where
    mempty                 = PwdError ""
    mappend (PwdError e) _ = PwdError e
 
getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
    s <- liftIO getLine    
 
    let handleError (PwdError e) = liftIO $ putStrLn e >> return False
 
    validation <- isValid s `catchError` handleError
    guard validation
 
    return s
 
isValid :: String -> PwdErrorIOMonad Bool
isValid s = 
    do
        let errorPref = "Incorrect input: "

        lengthValid  <- 
            validator ((>= 8) . length) (errorPref ++ "password is too short!") s
        digitValid <- 
            validator (any isNumber) (errorPref ++ "password must contain some digits!") s
        punctValid <- 
            validator (any isPunctuation) (errorPref ++ "password must contain some punctuations!") s
 
        return $ lengthValid && digitValid && punctValid

validator :: (String -> Bool) -> String -> String -> PwdErrorIOMonad Bool
validator f errMsg s  |  f s =  return True
                      |  otherwise = throwError $ PwdError errMsg