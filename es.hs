
module ES where

import Control.Monad (forever, replicateM)
--import Data.List (intercalate)
import qualified Control.Monad.Primitive as Prim
import qualified System.Random.MWC as MWC
import System.Random.MWC.Distributions (normal)
import Data.Vector (singleton)

type Variable = Double
type Variance = Double
type Covariance = Double
data Genotype   = Fixed [Variable] Variance
                | Uncorrelated [Variable] [Variance]
                | Correlated [Variable] [Variance] [Covariance] deriving (Show, Eq)
data Pheonotype = Sphere [Double] deriving (Show, Eq)

instance Ord Pheonotype where
    compare a b = compare (fitness a) (fitness b)
instance Ord Genotype where
    compare a b = compare (decode a) (decode b)

type Seed = MWC.Seed
type Gen = MWC.Gen (Prim.PrimState IO)


tau = 1
boundary = 0.005



--data State = State {
--    getSeed            :: Seed,
--    --getDeviation       :: Double,
--    getTau             :: Double,
--    getBoundary        :: Double
--} deriving (Show, Eq)

--restoreGen :: State -> IO Gen
--restoreGen state = MWC.restore (getSeed state)

--saveGen :: State -> Gen -> IO State
--saveGen state gen = do
--    seed' <- MWC.save gen
--    return $ State {
--        getSeed = seed',
--        --getDeviation = getDeviation state,
--        getTau = getTau state,
--        getBoundary = getBoundary state
--    }


decode :: Genotype -> Pheonotype
decode (Fixed variables _) = Sphere variables
decode (Uncorrelated variables _) = Sphere variables
decode (Correlated variables _ _) = Sphere variables

fitness :: Pheonotype -> Double
fitness (Sphere sphere) = sum . map square $ sphere
    where   square x = x * x 

mutate :: Genotype -> Gen -> IO (Genotype, Gen)
mutate (Fixed variables sigma) gen = do

    -- normal distribution for step size
    --n0 <- normal 0 1 gen
    --let sigma' = max boundary (sigma * exp (tau * n0))

    -- x' = x + d x N(0, 1)
    variances <- replicateM (length variables) (normal 0 1 gen)
    let variables' = zipWith (\x v -> x + sigma * v) variables variances

    return (Fixed variables' sigma, gen)

run threshold stop n f x
    | stop <= n = do
        print n
    | otherwise = do
        if g <= threshold then do
            print $ "********** " ++ show n ++ "  - " ++ (show g)
        else do
            x' <- f x
            print $ "generation " ++ show n ++ "  - " ++ (show g)
            run threshold stop (succ n) f x'
        where   
                g = fitness $ decode x


run' threshold stop n f x
    | stop <= n = do
        print n
    | otherwise = do
        if g < threshold then do
            print $ "********** " ++ show (succ n) ++ "  - " ++ (show g)
        else do
            x' <- f x
            gen' <- extractGen x'
            print $ "generation " ++ show (succ n) ++ "  - " ++ (show . fitness' $ min x x') ++ "  " ++ show (get x) ++ show (get x')
            x'' <- injectGen x gen'
            run' threshold stop (succ n) f (min x'' x')
        where   fitness' = fitness . decode
                g = fitness' x
                extractGen (Fixed _ _ state) = restoreGen state
                injectGen (Fixed v s state) gen = do
                    state' <- saveGen state gen
                    return $ Fixed v s state'


unicorn = State {
    getSeed = MWC.toSeed (singleton 42),
    getTau = 1,
    getBoundary = 0.005
}

test = Fixed (replicate 1 1) 0.01 unicorn
get (Fixed v _ _) = v

go = do
    gen <- MWC.initialize (singleton 40)
    n <- replicateM 10 $ normal 0 1 gen
    print n




----seed :: a -> IO Seed
--seed n = MWC.initialize (V.singleton n) >>= MWC.save

--gauss' :: Seed -> IO (Seed, Double)
--gauss' seed = do
--    gen <- MWC.restore seed
--    arr <- MWC.uniformVector gen times
--    let n = (V.sum arr) / 20.0
--    seed' <- MWC.save gen
--    return (seed', n)
--    where   times = 20
