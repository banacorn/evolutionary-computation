
module ES where

--import Control.Monad (forever, replicateM)
--import Data.List (intercalate)
import qualified Control.Monad.Primitive as Prim
import qualified System.Random.MWC as MWC
import qualified Data.Vector as V

type Variable = Double
type Variance = Double
type Covariance = Double
data Genotype   = Fixed [Variable] Variance
                | Uncorrelated [Variable] [Variance]
                | Correlated [Variable] [Variance] [Covariance] deriving (Show)
data Pheonotype = Sphere [Double] deriving (Show)


type Seed = MWC.Seed
type Gen = MWC.Gen (Prim.PrimState IO)


decode :: Genotype -> Pheonotype
decode (Fixed variables _) = Sphere variables
decode (Uncorrelated variables _) = Sphere variables
decode (Correlated variables _ _) = Sphere variables

fitness :: Pheonotype -> Double
fitness (Sphere sphere) = sum . map square $ sphere
    where   square x = x * x 

--mutate :: Genotype -> IO Genotype
--mutate (Fixed variables variance) = 

test = Fixed (replicate 30 1) 1

--go = do
--    gen <- MWC.initialize (singleton 40)
--    n <- MWC.normal gen
--    print n




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
