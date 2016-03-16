{- B1Matrix
Gregory W. Schwartz

Collections the functions pertaining to the creation of the tf-idf B1 matrix
from the records
-}

{-# LANGUAGE OverloadedStrings #-}

module B1Matrix
    ( recordToQGrams
    , getB1
    , getQGramMapFromRows
    ) where

-- Standard
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IMap
import qualified Data.Set as Set
import Data.Monoid

-- Cabal
import qualified Data.Text as T
import Control.Lens

-- Local
import Types
import Utility

-- | Differentiate the beginning from the end of the record by adding "###"
-- to the beginning and "$$$" to the end.
differentiateStartEndRecord :: Q -> Record -> Record
differentiateStartEndRecord (Q q) = Record
                                  . flip T.append (T.replicate (q - 1) "$")
                                  . T.append (T.replicate (q - 1) "#")
                                  . unRecord

-- | Convert a record to qgrams.
toQGrams :: Q -> Record -> [QGram]
toQGrams (Q q) (Record record)
    | T.length record < q = []
    | otherwise           =
        QGram (T.take q record) : toQGrams (Q q) (Record . T.tail $ record)

-- | Count the number of qgrams in a list.
countQGrams :: [QGram] -> [(QGram, Double)]
countQGrams = Map.toAscList . Map.fromListWith (+) . flip zip [1,1..]

-- | Convert a qgram to the respective integer representation. Unused until
-- a better representation comes along.
projectQGramToInt :: Q -> QGram -> Int
projectQGramToInt (Q q) =
    read . T.foldl' (\acc x -> acc <> charInt x) "" . unQGram
  where
    charInt = show . (+) maxChar . fromEnum
    maxChar = fromEnum (maxBound :: Char)

-- | Convert a record to a tf-idf representation, with no index yet
recordToQGrams :: Q -> Record -> PreB1Row
recordToQGrams q =
    PreB1Row . countQGrams . toQGrams q . differentiateStartEndRecord q

-- | Make a qgram map from a bunch of record, making sure there is only one
-- qgram at a time
getQGramMapFromRows :: [PreB1Row] -> QGramMap
getQGramMapFromRows = QGramMap
                    . Map.fromList
                    . flip zip [1..]
                    . nub'
                    . concatMap (map fst . unPreB1Row)

-- | Convert the QGrams to an int id based off of an id map
qGramToInt :: QGram -> QGramMap -> QGramID
qGramToInt qgram =
    fromMaybe (error (T.unpack (unQGram qgram) <> ": no id found"))
        . Map.lookup qgram
        . unQGramMap

-- | Make a B1 row from a PreB1Row by converting qgrams to ints
getB1Row :: QGramMap -> PreB1Row -> B1Row
getB1Row qGramMap =
    B1Row . map (over _1 (flip qGramToInt qGramMap)) . unPreB1Row

-- | Get the IntMap representation of the B1 from a list of PreB1Rows
getB1 :: QGramMap -> [PreB1Row] -> B1
getB1 qGramMap = B1
               . IMap.fromList
               . zip [1..]
               . map (IMap.fromList . unB1Row . getB1Row qGramMap)
