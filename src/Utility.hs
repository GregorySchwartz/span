{- Utility
Gregory W. Schwartz

Collections the functions pertaining to general helpful functions
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Utility
    ( nub'
    , vLookup
    , mLookup
    , mapSum
    , getAlphabet
    , mToRM
    , transpose
    ) where

-- Standard
import Data.Int
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IMap
import Data.Monoid
import Debug.Trace

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

-- | Helpful error for map lookup
mLookup :: String -> IMap.IntMap a -> Int -> a
mLookup s xs = fromMaybe (error s) . flip IMap.lookup xs

-- | Strict sum over IntMap
mapSum :: (Num a) => IMap.IntMap a -> a
mapSum = IMap.foldl' (+) 0

-- | Get the alphabet map, ids for each character
getAlphabet :: String -> Alphabet
getAlphabet = Alphabet . Map.fromList . flip zip [0..] . ("#$" <>)

-- | Transpose a intmap matrix
transpose :: IMap.IntMap (IMap.IntMap a) -> IMap.IntMap (IMap.IntMap a)
transpose = IMap.foldlWithKey'
            (\acc k -> joinAcc acc . joinNewMaps . fmap (mapMake k) . IMap.toAscList)
            IMap.empty
  where
    joinAcc             = IMap.unionWith IMap.union
    joinNewMaps         = IMap.unionsWith IMap.union
    mapMake !x (!y, !z) = IMap.singleton y (IMap.singleton x z)

-- | Convert an intmap matrix to its sparse R matrix representation
mToRM :: IMap.IntMap (IMap.IntMap Double) -> R s (R.SomeSEXP s)
mToRM mat = [r| suppressMessages(library("Matrix"))
                x = sparseMatrix(i=rows_hs, j=cols_hs, x=vals_hs)
            |]
  where
    rows    :: [Int32]
    rows     = concatMap (fmap fromIntegral . snd)
             . IMap.toAscList
             . IMap.mapWithKey (\k v -> replicate (IMap.size v) k)
             $ mat
    cols    :: [Int32]
    cols     = concatMap (map (fromIntegral . fst) . snd) $ flatMat
    vals    :: [Double]
    vals     = concatMap (map snd . snd) flatMat
    flatMat  = IMap.toAscList . IMap.map IMap.toAscList $ mat
