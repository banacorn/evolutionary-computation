
import Control.Monad
import Control.Monad.State
import Data.List
--import qualified Data.List as L
import qualified System.Random.MWC as MWC
import qualified Control.Monad.Primitive as Prim
import qualified Data.Vector as V

type Fitness = Int
type Prob = Double
type Gene = Bool
type Gen = MWC.Gen (Prim.PrimState IO)
type Seed = MWC.Seed
data Chromosome = Chromosome [Bool] deriving (Eq)

data Config = Config {
    representationLength    :: Int,
    population              :: Int,
    seed                    :: MWC.Seed,
    best                    :: [Fitness]
} deriving (Show)

restore :: Config -> IO Gen
restore config = MWC.restore seed'
    where seed' = seed config

save :: Config -> Gen -> IO Config
save config gen = do
    seed' <- MWC.save gen
    return $ Config {
        representationLength = representationLength config,
        population = population config,
        seed = seed',
        best = best config
    }
    where seed' = seed config

addRecord :: Race -> IO Race
addRecord (Race pop config) = return $ Race pop $ Config {
        representationLength = representationLength config,
        population = population config,
        seed = seed config,
        best = bestFittest pop : oldBestFittest
    }
    where   oldBestFittest = best config
            bestFittest [Individual _ f] = f
            bestFittest ((Individual _ f):xs) = f `max` bestFittest xs

data Individual = Individual Chromosome Fitness deriving (Eq)
data Race = Race [Individual] Config
data MatingPool = MatingPool [(Individual, Individual)] Config


instance Show Chromosome where
    show (Chromosome chromosome) = bitstring ++ ":" ++ (show $ length chromosome)
        where   bitstring = map toBit chromosome
                toBit True = '1'
                toBit False = '0'
instance Show Individual where
    show (Individual chromosome fitness) = " " ++ show fitness ++ " " ++ show chromosome ++ " " 
instance Show Race where
    show (Race pop config) = intercalate "\n" (map show pop)
instance Show MatingPool where
    show (MatingPool pop config) = intercalate "\n" (map show pop)
instance Ord Individual where
    compare (Individual _ fitness0) (Individual _ fitness1) = compare fitness0 fitness1

unicorn = Race [] Config {
    representationLength = 50,
    population = 200,
    seed = MWC.toSeed (V.singleton 42),
    best = []
}

ca = chromosomeToIndividual $ Chromosome [True, True, True, True, True, True]
cb = chromosomeToIndividual $ Chromosome [False, False, False, False, False, True]

print' :: Show a => a -> IO a
print' a = putStrLn (show a) >> return a

newline :: Show a => a -> IO a
newline a = putStrLn "" >> return a

iterateM :: Monad m => Int -> m a -> (a -> m a) -> m a
iterateM 0 c f = c
iterateM n c f = iterateM (n - 1) c f >>= f

fuck f = do
    u <- initialize unicorn >>= initializeGen
    Race _ config <- iterateM 100 (return u) (\s -> f s >>= crossover >>= evaluate >>= addRecord)
    print . reverse . best $ config
    --print . map (flip (-) 1000) . reverse . best $ config
    --print . intercalate ", " . map (\(i, a) -> "[" ++ show i ++ ", " ++ show a ++ "]") . zip [0 ..] . reverse . best $ config
    return ()


main = replicateM_ 10 (fuck rouletteWheel)

chromosomeToIndividual :: Chromosome -> Individual
chromosomeToIndividual chromosome = Individual chromosome $ evaluateC chromosome

initializeGen :: Race -> IO Race
initializeGen (Race pop config) = do
    a <- MWC.withSystemRandom . MWC.asGenST $ \gen -> MWC.uniform gen
    gen <- MWC.initialize (V.singleton a)
    config' <- save config gen
    return (Race pop config')

--
--  INITIALIZE
--

initializeI :: Gen -> Int -> IO Individual
initializeI gen len = do
    c <- MWC.uniformVector gen len :: IO (V.Vector Int)
    return (chromosomeToIndividual $ Chromosome (map even $ V.toList c))

initialize :: Race -> IO Race
initialize (Race _ config) = do
    gen <- restore config
    pop <- replicateM population' (initializeI gen representationLength')
    config' <- save config gen
    return (Race pop config')
    where   population'             = population config
            representationLength'   = representationLength config
--
--  EVALUATE
--

evaluateC :: Chromosome -> Fitness
evaluateC (Chromosome chromosome) = fuck
    where   fuck = fitness
            fitness = (sum $ map toBit chromosome)
            toBit True = 1
            toBit False = 0

evaluateI :: Individual -> Individual
evaluateI (Individual chromosome _) = Individual chromosome $ evaluateC chromosome

evaluate :: Race -> IO Race
evaluate (Race pop config) = return $ Race (map evaluateI pop) config

averageFitness :: Race -> IO Race
averageFitness (Race pop config) = do
    print $ fromIntegral sum / fromIntegral (population config)
    return (Race pop config)
    where   sum = gather pop
            gather [] = 0
            gather ((Individual _ f):xs) = f + gather xs

--
--  SELECTION
--



rouletteWheelI :: [Individual] -> Gen -> IO Individual
rouletteWheelI pop gen = do
    let total = totalFitness pop
    number <- MWC.uniformR (0, total) gen
    return (spinWheel number pop)
    where   
            totalFitness [] = 0
            totalFitness ((Individual _ fitness):xs) =  fitness + totalFitness xs
            spinWheel number [x] = x
            spinWheel number (x:xs)
                | number <= fitness = x
                | otherwise         = spinWheel (number - fitness) xs
                where (Individual _ fitness) = x

rouletteWheel :: Race -> IO MatingPool
rouletteWheel (Race pop config) = do
    gen <- restore config
    male <- replicateM halfPopulationSize (rouletteWheelI pop gen)
    female <- replicateM halfPopulationSize (rouletteWheelI pop gen)
    config' <- save config gen
    return (MatingPool (zip male female) config')
    where   halfPopulationSize = population config `div` 2


tournamentI :: [Individual] -> Gen -> IO Individual
tournamentI pop gen = do
    na <- MWC.uniformR (0, size - 1) gen
    nb <- MWC.uniformR (0, size - 1) gen
    let a = pop !! na
    let b = pop !! nb
    return (max a b)
    where   size = length pop


tournament :: Race -> IO MatingPool
tournament (Race pop config) = do
    gen <- restore config
    male <- replicateM halfPopulationSize (tournamentI pop gen)
    female <- replicateM halfPopulationSize (tournamentI pop gen)
    config' <- save config gen
    return (MatingPool (zip male female) config')
    where   halfPopulationSize = population config `div` 2






--
--  XOVER
--

crossoverI :: Gen -> (Individual, Individual) -> IO (Individual, Individual)
crossoverI gen (Individual (Chromosome chromosome0) _, Individual (Chromosome chromosome1) _) = do
    point <- MWC.uniformR (0, chromosomeLength) gen
    let (head0, tail0) = splitAt point chromosome0
    let (head1, tail1) = splitAt point chromosome1
    let chromosome0' = Chromosome $ head0 ++ tail1
    let chromosome1' = Chromosome $ head1 ++ tail0
    return (Individual chromosome0' 0, Individual chromosome1' 0)
    where   chromosomeLength = length chromosome0

crossover :: MatingPool -> IO Race
crossover (MatingPool mates config) = do
    gen <- restore config
    children <- mapM (crossoverI gen) mates
    let newPopulation = concat $ map pair2list children
    config' <- save config gen
    return (Race newPopulation config')
    where   halfPopulationSize = population config `div` 2
            pair2list (a, b) = [a, b] 

