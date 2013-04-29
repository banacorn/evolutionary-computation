
module Main where

import Control.Monad (forever, replicateM, replicateM_)
--import Data.List (intercalate)
import qualified Control.Monad.Primitive as Prim
import qualified System.Random.MWC as MWC
import System.Random.MWC.Distributions (normal)
import Data.Vector (singleton)
import GHC.Word (Word32)

type Variable = Double
type Variance = Double
type Covariance = Double
type Total = Int
type Success = Int
data Genotype   = Fixed [Variable] Variance
                | Uncorrelated [Variable] [Variance]
                | EinFuenftel [Variable] Variance (Success, Total) deriving (Show, Eq)
data Pheonotype = Sphere [Double] deriving (Show, Eq)

instance Ord Pheonotype where
    compare a b = compare (evaluate a) (evaluate b)
instance Ord Genotype where
    compare a b = compare (decode a) (decode b)

type Seed = MWC.Seed
type Gen = MWC.Gen (Prim.PrimState IO)


tau = 0.7071
boundary = 0.005
a = 0.9

decode :: Genotype -> Pheonotype
decode (Fixed variables _) = Sphere variables
decode (Uncorrelated variables _) = Sphere variables
decode (EinFuenftel variables _ _) = Sphere variables

evaluate :: Pheonotype -> Double
evaluate (Sphere sphere) = sum . map square $ sphere
    where   square x = x * x 


evaluateG :: Genotype -> Double
evaluateG = evaluate . decode


mutate :: (Genotype, Gen) -> IO (Genotype, Gen)
mutate (Fixed variables sigma, gen) = do

    -- x' = x + S x N(0, 1)
    variances <- replicateM (length variables) (normal 0 1 gen)
    let variables' = zipWith (\x v -> x + sigma * v) variables variances

    return (Fixed variables' sigma, gen)


mutate (EinFuenftel variables sigma (success, total), gen) = do

    let successRate = fromIntegral success / fromIntegral total
    let sigma' = changeSigma sigma successRate

    variances <- replicateM (length variables) (normal 0 1 gen)
    let variables' = zipWith (\x v -> x + sigma' * v) variables variances

    return (EinFuenftel variables' sigma' (success, total), gen)
    where   changeSigma sigma successRate
                | successRate > 0.2 = sigma / a
                | successRate < 0.2 = sigma * a
                | otherwise = sigma


mutate (Uncorrelated variables sigmas, gen) = do

    shared <- normal 0 1 gen
    norms <- replicateM (length variables) (normal 0 1 gen)
    let sigmas' = zipWith (\original norm -> max boundary (original * exp (tau * shared + tau * norm))) sigmas norms
    
    -- x' = x + Si x Ni(0, 1)
    let variables' = zipWith3 (\x sigma norm-> x + sigma * norm) variables sigmas' norms

    return (Uncorrelated variables' sigmas', gen)



-- (1 + 1)-ES
generate (father, gen) = do
    (child, gen') <- mutate (father, gen)
    return (min father child, gen')    

-- (1, 1)-ES
generate' = mutate 

-- (1 + 1)-1/5-ES
generateEinFuenftel (father, gen) = do
    (child, gen') <- mutate (father, gen)
    let result = case child < father of
            True -> tagSuccess child
            False -> tagFailure father
    return (result, gen')
-- (1, 1)-1/5-ES
generateEinFuenftel' (father, gen) = do
    (child, gen') <- mutate (father, gen)
    let result = case child < father of
            True -> tagSuccess child
            False -> tagFailure child
    return (result, gen')

tagSuccess (EinFuenftel v s (success, total)) = EinFuenftel v s (succ success, succ total)
tagFailure (EinFuenftel v s (success, total)) = EinFuenftel v s (     success, succ total)


run n (x, gen) threshold times elitism
    | times <= n = print n
    | otherwise = do
        if evaluateG x <= threshold then do
            print $ "********** " ++ show n ++ "  - " ++ (show $ evaluateG x)
        else do
            (x', gen') <- if elitism then generate (x, gen) else generate' (x, gen)
            --print $ "generation " ++ show n ++ "  - " ++ (show $ evaluateG x)
            run (succ n) (x', gen') threshold times elitism

runEinFuenftel n (x, gen) threshold times elitism
    | times <= n = print n
    | otherwise = do 
        if evaluateG x <= threshold then do
            print $ "********** " ++ show n ++ "  - " ++ (show $ evaluateG x)
        else do
            (x', gen') <- if elitism then generateEinFuenftel (x, gen) else generateEinFuenftel' (x, gen)
            runEinFuenftel (succ n) (x', gen') threshold times elitism

get (Fixed v _) = v

go sigma threshold times elitism = do
    a <- (MWC.withSystemRandom . MWC.asGenIO $ \gen -> MWC.uniform gen) :: IO Word32
    gen <- MWC.initialize (singleton a)
    run 0 (test, gen) threshold times elitism
    where   test = Fixed (replicate 10 1) sigma

go' sigma threshold times elitism = do
    a <- (MWC.withSystemRandom . MWC.asGenIO $ \gen -> MWC.uniform gen) :: IO Word32
    gen <- MWC.initialize (singleton a)
    run 0 (uncorrelated, gen) threshold times elitism
    where uncorrelated = Uncorrelated (replicate 10 1) (replicate 10 sigma)

go'' sigma threshold times elitism = do
    a <- (MWC.withSystemRandom . MWC.asGenIO $ \gen -> MWC.uniform gen) :: IO Word32
    gen <- MWC.initialize (singleton a)
    runEinFuenftel 0 (uncorrelated, gen) threshold times elitism
    where uncorrelated = EinFuenftel (replicate 10 1) sigma (0, 0)

runF s True = do
    print $ "(1+1)-ES fixed  " ++ show s
    replicateM_ 10 (go s 0.005 10000000 True)
runF s False = do
    print $ "(1,1)-ES fixed  " ++ show s
    replicateM_ 10 (go s 0.005 10000000 False)

allF = do
    runF 1 True
    runF 0.1 True
    runF 0.01 True
    runF 1 False
    runF 0.1 False
    runF 0.01 False

runU s True = do
    print $ "(1+1)-ES uncorrelated  " ++ show s
    replicateM_ 10 (go' s 0.005 10000000 True)
runU s False = do
    print $ "(1,1)-ES uncorrelated  " ++ show s
    replicateM_ 10 (go' s 0.005 10000000 False)

allU = do
    runU 1 True
    runU 0.1 True
    runU 0.01 True
    runU 1 False
    runU 0.1 False
    runU 0.01 False

runE s True = do
    print $ "(1+1)-ES with 1/5 rule  " ++ show s
    replicateM_ 10 (go'' s 0.005 10000000 True)
runE s False = do
    print $ "(1,1)-ES with 1/5 rule  " ++ show s
    replicateM_ 10 (go'' s 0.005 10000000 False)

allE = do
    runE 1 True
    runE 0.1 True
    runE 0.01 True
    runE 1 False
    runE 0.1 False
    runE 0.01 False


main = allE
