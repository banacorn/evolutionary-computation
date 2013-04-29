
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
    compare a b = compare (evaluate a) (evaluate b)
instance Ord Genotype where
    compare a b = compare (decode a) (decode b)

type Seed = MWC.Seed
type Gen = MWC.Gen (Prim.PrimState IO)


tau = 1
boundary = 0.005



decode :: Genotype -> Pheonotype
decode (Fixed variables _) = Sphere variables
decode (Uncorrelated variables _) = Sphere variables
decode (Correlated variables _ _) = Sphere variables

evaluate :: Pheonotype -> Double
evaluate (Sphere sphere) = sum . map square $ sphere
    where   square x = x * x 


evaluateG :: Genotype -> Double
evaluateG = evaluate . decode


mutate :: (Genotype, Gen) -> IO (Genotype, Gen)
mutate (Fixed variables sigma, gen) = do

    --n0 <- normal 0 1 gen
    --let sigma' = max boundary (sigma * exp (tau * n0))

    -- x' = x + d x N(0, 1)
    variances <- replicateM (length variables) (normal 0 1 gen)
    let variables' = zipWith (\x v -> x + sigma * v) variables variances

    return (Fixed variables' sigma, gen)
--mutate (Uncorrelated variables sigmas, gen) = do

--    n0 <- normal 0 1 gen
--    let sigma' = max boundary (sigma * exp (tau * n0))


-- (1 + 1)-ES
generate (father, gen) = do
    (child, gen') <- mutate (father, gen)
    return (min father child, gen')    

-- (1, 1)-ES
generate' = mutate 

run n (x, gen) threshold times elitism
    | times <= n = print n
    | otherwise = do
        if evaluateG x <= threshold then do
            print $ "********** " ++ show n ++ "  - " ++ (show $ evaluateG x)
        else do
            (x', gen') <- if elitism then generate (x, gen) else generate' (x, gen)
            --print $ "generation " ++ show n ++ "  - " ++ (show $ evaluateG x)
            run (succ n) (x', gen') threshold times elitism



get (Fixed v _) = v

go sigma threshold times elitism= do
    gen <- MWC.initialize (singleton 40)
    run 0 (test, gen) threshold times elitism
    where   test = Fixed (replicate 10 1) sigma

