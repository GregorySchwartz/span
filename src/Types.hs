{- Types
Gregory W. Schwartz

Collections the types used in the program
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

-- Standard
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IMap
import qualified Data.Vector as V
import Data.Tree

-- Cabal
import qualified Data.Text as T
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ

-- Local


-- Algebraic
data TreeData = TreeData { treeLabel    :: !ID
                         , treeSize     :: !Size
                         , treeHeight   :: !Height
                         , treeLocation :: !Double
                         , treeNGValue  :: !Double
                         , treeRecords  :: !(V.Vector (RowID, Record))
                         }
                deriving (Eq, Show)

newtype Q           = Q { unQ :: Int } deriving (Eq, Num, Read, Show)
newtype ID          = ID { unID :: Int } deriving (Eq, Num, Show)
newtype Size        = Size Int deriving (Eq, Num, Show)
newtype Height      = Height Int deriving (Eq, Num, Show)
newtype Record      = Record { unRecord :: T.Text } deriving (Eq, Ord, Show)
newtype QGram       = QGram { unQGram :: T.Text } deriving (Eq, Ord, Show)
newtype Row         = Row Int deriving (Show)
newtype Column      = Column Int deriving (Show)
newtype QGramMap    = QGramMap { unQGramMap :: Map.Map QGram QGramID }
newtype PreB1Row    = PreB1Row { unPreB1Row :: [(QGram, Double)] }
                      deriving (Eq, Ord, Show)
newtype B1Row       = B1Row { unB1Row :: [(QGramID, Double)] }
newtype B1          = B1 { unB1 :: IMap.IntMap (IMap.IntMap Double) }
newtype B2          = B2 { unB2 :: IMap.IntMap (IMap.IntMap Double) }
newtype B           = B  { unB  :: IMap.IntMap (IMap.IntMap Double) }
                      deriving (Eq, Show)
newtype ClusterTree = ClusterTree { unClusterTree :: (Tree TreeData) }
                      deriving (Show)
newtype Alphabet    = Alphabet { unAlphabet :: (Map.Map Char Int) }
                      deriving (Show)

-- Basic
type RowID   = Int
type QGramID = Int

-- Advanced
