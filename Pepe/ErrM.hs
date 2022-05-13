module Pepe.ErrM where


import           Control.Applicative (Alternative (..), Applicative (..))
import           Control.Monad       (MonadPlus (..), liftM)

data Err a = Ok a | Bad String
    deriving (Read, Show, Eq, Ord)

instance Monad Err where
    return      = Ok
    fail        = Bad
    Ok a  >>= f = f a
    Bad s >>= _ = Bad s

instance Applicative Err where
    pure = Ok
    (Bad s) <*> _ = Bad s
    (Ok f) <*> o  = liftM f o


instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
    mzero = Bad "Err.mzero"
    mplus (Bad _) y = y
    mplus x       _ = x

instance Alternative Err where
    empty = mzero
    (<|>) = mplus
