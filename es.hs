
module ES where

--import Control.Monad (forever, replicateM)
--import Data.List (intercalate)
import qualified System.Random.MWC as MWC

type Variable = Double
type Variance = Double
type Covariance = Double
data Genotype   = Fixed [Variable] Variance
                | Uncorrelated [Variable] [Variance]
                | Correlated [Variable] [Variance] [Covariance] deriving (Show)
data Pheonotype = Sphere [Double] deriving (Show)

decode :: Genotype -> Pheonotype
decode (Fixed variables _) = Sphere variables
decode (Uncorrelated variables _) = Sphere variables
decode (Correlated variables _ _) = Sphere variables

fitness (Sphere sphere) = sum . map square $ sphere
    where   square x = x * x 


test = Fixed (replicate 30 1) 1