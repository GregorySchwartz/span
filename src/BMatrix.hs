{- BMatrix
Gregory W. Schwartz

Collections the functions pertaining to the creation of the B Matrix
-}

module BMatrix
    ( getB
    , getB2
    ) where

-- Standard
import Data.Maybe
import qualified Data.IntMap.Strict as IMap
import qualified Data.Vector as V

-- Cabal

-- Local
import Types
import Utility

-- | Get the number of rows containing a column
getNumRowsWithCol :: Int -> B1 -> Double
getNumRowsWithCol x =
    fromIntegral . IMap.size . IMap.filter (IMap.member x) . unB1

-- | Get the B2 matrix from the B1 matrix, using the qGram length to figure
-- out a mapping of the column ints
getB2:: Int -> B1 -> B2
getB2 numQGrams (B1 b1) = B2 . IMap.map (IMap.mapWithKey norm) $ b1
  where
    norm k v   = normFactor k
               * v
    normFactor k = logBase 2 ( fromIntegral (IMap.size b1)
                             / vLookup "getB2 vLookup" colVec (k - 1)
                             )
    colVec     = V.imap (\k _ -> getNumRowsWithCol (k + 1) (B1 b1))
               $ V.replicate numQGrams 0

-- | Get the euclidean norm of a row
euclideanNorm :: Int -> B2 -> Double
euclideanNorm row = maybe (error "Row not found in B calculation") norm
                  . IMap.lookup row
                  . unB2
  where
    norm = sqrt . mapSum . IMap.map (** 2)

-- | Get the Euclidean norm of each row
euclideanNormVec :: B2 -> V.Vector Double
euclideanNormVec (B2 b2) = V.imap (\k _ -> euclideanNorm (k + 1) (B2 b2))
                         $ V.replicate (IMap.size b2) 0

-- | Get the B matrix from B2
getB :: B2 -> B
getB (B2 b2) =
    B
        . IMap.mapWithKey (\k -> IMap.map ( 1 / (vLookup "getB vLookup" rowNorms (k - 1)) *))
        $ b2
  where
    rowNorms = euclideanNormVec (B2 b2)

