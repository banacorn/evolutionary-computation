 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE FunctionalDependencies #-}

module GA where

import Control.Monad (forever, replicateM)

import qualified System.Random.MWC as MWC
import qualified Control.Monad.Primitive as Prim
import qualified Data.Vector as V

type Seed = MWC.Seed
type Gen = MWC.Gen (Prim.PrimState IO)

class Phenotype p where
    evaluate :: p -> Int

class Eq g => Genotype g where
    mutate :: Gen -> g -> IO g
    crossover :: Gen -> [g] -> IO [g]
    initialize :: Gen -> IO g

class (Genotype genotype, Phenotype phenotype) => Representation genotype phenotype | phenotype -> genotype, genotype -> phenotype where
    encode :: phenotype -> genotype
    decode :: genotype -> phenotype

data Individual g = Individual g Int

instance Eq g => Eq (Individual g) where
    Individual a _ == Individual b _ = a == b


instance Eq g => Ord (Individual g) where
    compare (Individual _ a) (Individual _ b) = compare a b 

data Population g p = Population Config [Individual g]

instance (Show g, Show p) => Show (Population g p) where
    show (Population config population) = 
        show population ++ 
        "\nsize " ++ (show . size $ config)
        --"\nseed " ++ (show . MWC.fromSeed . seed $ config)


instance Show g => Show (Individual g) where
    show (Individual chromosome fitness) = " " ++ show fitness ++ " " ++ show chromosome ++ " "

data Config = Config {
    size              :: Int,
    seed              :: Seed
} deriving (Show)

restoreGen :: Config -> IO Gen
restoreGen config = MWC.restore (seed config)

saveGen :: Config -> Gen -> IO Config
saveGen config gen = do
    seed' <- MWC.save gen
    return $ Config {
        size = size config,
        seed = seed'
    }
    where seed' = seed config

initializeP :: (Representation g p) => Config -> IO (Population g p)
initializeP config = do
    gen <- restoreGen config
    chomosomes <- replicateM (size config) (initialize gen)
    config' <- saveGen config gen
    return $ Population config' (fmap (flip Individual 0) chomosomes)

evaluateP :: (Representation g p) => Population g p -> IO (Population g p)
evaluateP (Population config population) = return $ Population config population'
    where   population' = map evaluateI population
            evaluateI (Individual chomosome _) = Individual chomosome (evaluate (decode chomosome))

selectP :: (Representation g p) => (Gen -> [Individual g] -> IO (Individual g)) -> Population g p -> IO (Population g p)
selectP f (Population config population) = do
    gen <- restoreGen config
    population' <- replicateM populationSize (f gen population)
    config' <- saveGen config gen
    return $ Population config' population'
    where   populationSize = size config

tournament :: (Representation g p) => Gen -> [Individual g] -> IO (Individual g)
tournament gen population = do
    indexA <- MWC.uniformR (0, populationSize - 1) gen
    indexB <- MWC.uniformR (0, populationSize - 1) gen
    let a = population !! indexA 
    let b = population !! indexB
    return $ max a b
    where   populationSize = length population

rouletteWheel :: (Representation g p) => Gen -> [Individual g] -> IO (Individual g)
rouletteWheel gen population = do
    let total = totalFitness population
    number <- MWC.uniformR (0, total) gen
    return (spinWheel number population)
    where   
            totalFitness [] = 0
            totalFitness ((Individual _ fitness):xs) =  fitness + totalFitness xs
            spinWheel number [x] = x
            spinWheel number (x:xs)
                | number <= fitness = x
                | otherwise         = spinWheel (number - fitness) xs
                where (Individual _ fitness) = x

evaluateP :: (Representation g p) => Population g p -> IO (Population g p)
evaluateP (Population crossover population) = do 

print' :: Show a => a -> IO a
print' a = putStrLn (show a) >> return a

avg :: (Representation g p, Fractional a) => Population g p -> IO a
avg (Population config population) = return $ (fromIntegral summation) / (fromIntegral . size $ config)
    where summation = foldl (\ e (Individual _ f) -> e + f ) 0 population

main = do
    unicorn <- initializeP unicornConfig :: IO (Population BoolString BitString) 
    
    a <- evaluateP unicorn >>= selectP rouletteWheel
    avg a >>= print
    b <- selectP rouletteWheel a
    avg b >>= print
    c <- selectP rouletteWheel b
    avg c >>= print
    d <- selectP rouletteWheel c
    avg d >>= print



data BoolString = BoolString [Bool] deriving Eq
data BitString = BitString [Int] deriving Show


instance Genotype BoolString where
    initialize gen = fmap (BoolString . map even . V.toList) (MWC.uniformVector gen 10 :: IO (V.Vector Int))
    mutate gen (BoolString list) = do
        n <- MWC.uniformR (0, len - 1) gen
        return . BoolString $ mutate n list
        where   len = length list
                mutate _ [] = []
                mutate 0 (b:bs) = not b : bs
                mutate n (b:bs) = b : mutate (n - 1) bs
    crossover gen [BoolString l0, BoolString l1] = do
        n <- MWC.uniformR (0, len - 1) gen :: IO Int
        let (head0, tail0) = splitAt n l0
        let (head1, tail1) = splitAt n l1
        return [BoolString (head0 ++ tail1), BoolString (head1 ++ tail0)]
        where   len = min (length l0) (length l1)



instance Show BoolString where
    show (BoolString boolString) = map toChar boolString
        where   toChar True = '*'
                toChar False = '_'

asBoolString :: BoolString -> BoolString
asBoolString = id

instance Phenotype BitString where
    evaluate (BitString bitString) = sum bitString

instance Representation BoolString BitString where
    encode (BitString bitString) = BoolString . map odd $ bitString
    decode (BoolString boolString) = BitString . map toBit $ boolString
        where   toBit True = 1
                toBit False = 0

unicornConfig = Config {
    size = 100,
    seed = MWC.toSeed (V.singleton 42)
}

unicorn = Population unicornConfig [] 