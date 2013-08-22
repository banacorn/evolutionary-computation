{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts  #-}

module EC.ES where

import qualified Control.Monad.Primitive as Prim
--import qualified System.Random.MWC as MWC
import System.Random.MWC
import Control.Monad.ST
import Control.Monad
import Control.Monad.State

newtype S s a = S { runS :: StateT (GenST s) (ST s) a }
    deriving (Monad, MonadState (GenST s))


--init :: S s ()
--init :: (MonadTrans t, Prim.PrimMonad (ST s), MonadState (GenST s) (t (ST s))) => t (ST s) ()
--init = lift create >>= put

--a :: ST s [Int]
--a = do
--    gen <- create 
--    i <- replicateM 10 $ uniform gen
--    return (i :: [Int])

--b = do
--    i <- a
--    j <- a
--    return $ i ++ j