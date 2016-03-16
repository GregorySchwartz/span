{- Print
Gregory W. Schwartz

Collections the functions pertaining to the printing of the clusters
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Print
    ( printClusters
    ) where

-- Standard
import qualified Data.Vector as V
import Data.Monoid

-- Cabal
import qualified Data.Text as T
import TextShow
import Math.TreeFun.Tree
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ

-- Local
import Types

-- | Print the tree data cluster
printCluster :: TreeData -> T.Text
printCluster (TreeData { treeLabel = (ID label), treeRecords = records }) =
    T.intercalate "\n"
        . V.toList
        . V.map ( \(!x, !(Record y)) ->
                    T.intercalate "," [showt x, y, showt label]
                )
        $ records

-- | Print the clusters of a tree
printClusters :: ClusterTree -> T.Text
printClusters (ClusterTree clusterTree) = header <> body
  where
    header =  "fragment_id,fragment,cluster_id\n"
    body   =  T.unlines . fmap printCluster . leaves $ clusterTree
