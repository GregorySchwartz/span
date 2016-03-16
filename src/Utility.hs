{- Utility
Gregory W. Schwartz

Collections the functions pertaining to general helpful functions
-}

{-# LANGUAGE QuasiQuotes #-}

module Utility
    ( nub'
    , vLookup
    , mapSum
    , bToRB
    ) where

-- Standard
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IMap

-- Cabal
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ

-- Local
import Types

-- | Faster nub
nub' :: (Ord a) => [a] -> [a]
nub' = Set.toList . Set.fromList

-- | Helpful error for vector lookup
vLookup :: String -> V.Vector a -> Int -> a
vLookup s xs = fromMaybe (error s) . (V.!?) xs

-- | Strict sum over IntMap
mapSum :: (Num a) => IMap.IntMap a -> a
mapSum = IMap.foldl' (+) 0

-- | Convert a B matrix to its sparse R matrix representation
bToRB :: B -> R s (R.SomeSEXP s)
bToRB (B b) = [r| suppressMessages(require("Matrix"))
                  x = sparseMatrix(i=rows_hs, j=cols_hs, x=vals_hs)
              |]
  where
    rows :: [Double]
    rows  = concatMap (fmap fromIntegral . snd)
          . IMap.toAscList
          . IMap.mapWithKey (\k v -> replicate (IMap.size v) k)
          $ b
    cols :: [Double]
    cols  = concatMap (map (fromIntegral . fst) . snd) flatB
    vals :: [Double]
    vals  = concatMap (map snd . snd) flatB
    flatB = IMap.toAscList . IMap.map IMap.toAscList $ b
