{- HierarchicalClustering
Gregory W. Schwartz

Collections the functions pertaining to the hierarchical clustering of the
records
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module HierarchicalClustering
    ( cluster
    ) where

-- Standard
import qualified Data.Vector as V
import Data.Tree
import Control.Monad

-- Cabal

-- Local
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ
import qualified H.Prelude as H
import qualified H.Prelude.Interactive as H

-- Local
import Types
import CMatrix
import NewmanGirvan

clusterEmpty :: Height
             -> ID
             -> V.Vector (RowID, Record)
             -> R s (Tree TreeData)
clusterEmpty !height !(ID label) !records = do
    [r| print("HOWDY") |]
    let treeData = TreeData { treeLabel    = ID label
                            , treeSize     = Size . V.length $ records
                            , treeHeight   = height
                            , treeLocation = 0
                            , treeNGValue  = 0
                            , treeRecords  = records
                            }

    return $ Node { rootLabel = treeData, subForest = [] }

clusterFull :: Height
            -> ID
            -> V.Vector (RowID, Record)
            -> R.SomeSEXP s
            -> R s (Tree TreeData)
clusterFull !height !(ID label) !records !b = do
    c          <- getC b
    part       <- [r| as.integer(sign(svd(c_hs, nu=2, nv=0)$u[,2]) < 0) |]

    leftIndex  <- [r| as.numeric(which(part_hs == 0)) |]
    rightIndex <- [r| as.numeric(which(part_hs == 1)) |]

    leftB      <- [r| b_hs[leftIndex_hs,] |]
    rightB     <- [r| b_hs[rightIndex_hs,] |]

    let getRecords index = V.map (\x -> records V.! (truncate x - 1))
                         . V.fromList
                         $ (H.fromSomeSEXP index :: [Double])
        leftRecords      = getRecords leftIndex
        rightRecords     = getRecords rightIndex

    ngResultR    <- ngModularity b part
    let ngResult =  (H.fromSomeSEXP ngResultR) :: Double

    let treeData = TreeData { treeLabel    = ID label
                            , treeSize     = Size . V.length $ records
                            , treeHeight   = height
                            , treeLocation = 0
                            , treeNGValue  = ngResult
                            , treeRecords  = records
                            }

        resultM
            | ngResult > 0 = do
                left  <- cluster (height + 1) (ID (label * 2)) leftRecords leftB
                right <- cluster (height + 1) (ID (label * 2) + 1) rightRecords rightB
                return $
                    Node { rootLabel = treeData { treeRecords = V.empty }
                         , subForest = [left, right]
                         }
            | otherwise = return $ Node { rootLabel = treeData, subForest = [] }

    result <- resultM
    return result

cluster :: Height
        -> ID
        -> V.Vector (RowID, Record)
        -> R.SomeSEXP s
        -> R s (Tree TreeData)
cluster !height !(ID label) !records !b
    | V.length records < 2 = clusterEmpty height (ID label) records
    | otherwise            = clusterFull height (ID label) records b
