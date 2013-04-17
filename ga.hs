 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE FunctionalDependencies #-}

module GA where

import Control.Monad (forever, replicateM)
import Data.List (intercalate)

import qualified System.Random.MWC as MWC
import qualified Control.Monad.Primitive as Prim
import qualified Data.Vector as V

type Seed = MWC.Seed
type Gen = MWC.Gen (Prim.PrimState IO)

class Phenotype p where
    evaluate :: p -> Int

class Gene n where
    mutate :: Gen -> Float -> n -> IO n

class (Functor f, Gene n) => Genotype f n | n -> f where
    crossover :: Gen -> (f g, f g) -> IO (f g, f g)
    initialize :: Gen -> IO (f n)

class (Genotype f genotype, Phenotype phenotype) => Representation f genotype phenotype | phenotype -> f genotype, f genotype -> phenotype where
    encode :: phenotype -> f genotype
    decode :: f genotype -> phenotype

--data Individual g = Individual g Int

--instance Eq g => Eq (Individual g) where
--    Individual a _ == Individual b _ = a == b

--instance Eq g => Ord (Individual g) where
--    compare (Individual _ a) (Individual _ b) = compare a b 

--data Population g p = Population Config [Individual g]

--instance (Show g, Show p) => Show (Population g p) where
--    show (Population config population) = 
--        chromosomes ++
--        "\nsize " ++ (show . size $ config)
--        where   chromosomes = intercalate "\n" $ map show population

--instance Show g => Show (Individual g) where
--    show (Individual chromosome fitness) = "  " ++ show fitness ++ " " ++ show chromosome ++ " "

--data Config = Config {
--    size              :: Int,
--    mutationProb      :: Double,
--    seed              :: Seed
--} deriving (Show)

----restoreGen :: Config -> IO Gen
----restoreGen config = MWC.restore (seed config)

----saveGen :: Config -> Gen -> IO Config
----saveGen config gen = do
----    seed' <- MWC.save gen
----    return $ Config {
----        size = size config,
----        mutationProb = mutationProb config,
----        seed = seed'
----    }
----    where seed' = seed config

----initializeP :: (Representation g p) => Config -> IO (Population g p)
----initializeP config = do
----    gen <- restoreGen config
----    chomosomes <- replicateM (size config) (initialize gen)
----    config' <- saveGen config gen
----    return $ Population config' (fmap (flip Individual 0) chomosomes)

----evaluateP :: (Representation g p) => Population g p -> IO (Population g p)
----evaluateP (Population config population) = return $ Population config population'
----    where   population' = map evaluateI population
----            evaluateI (Individual chomosome _) = Individual chomosome (evaluate (decode chomosome))

----selectP :: (Representation g p) => (Gen -> [Individual g] -> IO (Individual g)) -> Population g p -> IO (Population g p)
----selectP f (Population config population) = do
----    gen <- restoreGen config
----    population' <- replicateM populationSize (f gen population)
----    config' <- saveGen config gen
----    return $ Population config' population'
----    where   populationSize = size config

----tournament :: (Representation g p) => Gen -> [Individual g] -> IO (Individual g)
----tournament gen population = do
----    indexA <- MWC.uniformR (0, populationSize - 1) gen
----    indexB <- MWC.uniformR (0, populationSize - 1) gen
----    let a = population !! indexA 
----    let b = population !! indexB
----    return $ max a b
----    where   populationSize = length population

----rouletteWheel :: (Representation g p) => Gen -> [Individual g] -> IO (Individual g)
----rouletteWheel gen population = do
----    let total = totalFitness population
----    number <- MWC.uniformR (0, total) gen
----    return (spinWheel number population)
----    where   
----            totalFitness [] = 0
----            totalFitness ((Individual _ fitness):xs) =  fitness + totalFitness xs
----            spinWheel number [x] = x
----            spinWheel number (x:xs)
----                | number <= fitness = x
----                | otherwise         = spinWheel (number - fitness) xs
----                where (Individual _ fitness) = x

----crossoverP :: (Representation g p) => Population g p -> IO (Population g p)
----crossoverP (Population config population) = do
----    gen <- restoreGen config
----    (p0', p1') <- fmap unzip $ mapM (crossoverI gen) (group population)
----    let population' = p0' ++ p1'
----    config' <- saveGen config gen
----    return (Population config' population')
----    where   group [] = []
----            group [x] = [(x, x)]
----            group (x:y:xs) = (x, y) : group xs
----            crossoverI gen (Individual a _, Individual b _) = do
----                (a', b') <- crossover gen (a, b)
----                return (Individual a' 0, Individual b' 0)

----mutateP :: (Representation g p) => Population g p -> IO (Population g p)
----mutateP (Population config population) = do
----    gen <- restoreGen config
----    population' <- mapM (mutateI gen (0)) population
----    config' <- saveGen config gen
----    return $ Population config' population
----    where   mutateI gen prob (Individual chromosome _) = do
----                chromosome' <- mutate gen prob chromosome
----                return $ Individual chromosome' 0

----print' :: Show a => a -> IO a
----print' a = putStrLn (show a) >> return a


----iterateM :: Monad m => Int -> (a -> m a) -> m a -> m a
----iterateM 0 f c = c
----iterateM n f c = iterateM (n - 1) f c >>= f

----printAvg :: (Representation g p) => Population g p -> IO (Population g p)
----printAvg (Population config population) = do
----    putStrLn $ "average " ++ show ((fromIntegral summation) / (fromIntegral . size $ config))
----    return (Population config population)
----    where summation = foldl (\ e (Individual _ f) -> e + f ) 0 population

----main = do
----    unicorn <- initializeP unicornConfig >>= evaluateP :: IO (Population BoolString BitString) 
----    iterateM 3000 (\ s -> printAvg s >>= selectP tournament >>= crossoverP >>= mutateP >>= evaluateP) (return unicorn)



--data BoolString = BoolString [Bool] deriving Eq
--data BitString = BitString [Int] deriving Show

--newtype BoolString = BoolString [Bool]


instance Gene Bool where
    mutate gen prob b = do
        n <- MWC.uniform gen :: IO Float
        case n > prob of
            True -> return b 
            False -> return $ not b

instance Genotype [] Bool where
    initialize gen = fmap V.toList (MWC.uniformVector gen 100 :: IO (V.Vector Bool))
    --initialize gen = fmap (BoolString . map even . V.toList) (MWC.uniformVector gen 100 :: IO (V.Vector Int))
    --mutate gen prob boolString = return boolString
    --mutate gen prob (BoolString list) = do
    --    n <- MWC.uniform gen
    --    --return . BoolString $ mutate' n list
    --    --where   len = length list
    --    --        mutate' _ [] = []
    --    --        mutate' 0 (b:bs) = not b : bs
    --    --        mutate' n (b:bs) = b : mutate' (n - 1) bs
    --crossover gen (BoolString l0, BoolString l1) = do
    --    n <- MWC.uniformR (0, len - 1) gen :: IO Int
    --    let (head0, tail0) = splitAt n l0
    --    let (head1, tail1) = splitAt n l1
    --    return (BoolString (head0 ++ tail1), BoolString (head1 ++ tail0))
    --    where   len = min (length l0) (length l1)



----instance Show BoolString where
----    show (BoolString boolString) = map toChar boolString
----        where   toChar True = '1'
----                toChar False = '0'

----asBoolString :: BoolString -> BoolString
----asBoolString = id

----instance Phenotype BitString where
----    evaluate (BitString bitString) = sum bitString

----instance Representation BoolString BitString where
----    encode (BitString bitString) = BoolString . map odd $ bitString
----    decode (BoolString boolString) = BitString . map toBit $ boolString
----        where   toBit True = 1
----                toBit False = 0

----unicornConfig = Config {
----    size = 20,
----    mutationProb = 0.3,
----    seed = MWC.toSeed (V.singleton 42)
----}

----unicorn = Population unicornConfig [] 