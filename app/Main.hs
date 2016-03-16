{- scan
Gregory W. Schwartz

Executes the SCAN algorithm to cluster a list of records
-}

{-# LANGUAGE QuasiQuotes #-}

module Main where

-- Standard
import qualified Data.Map as Map
import Data.Vector as V

-- Cabal
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ
import qualified H.Prelude as H
import Options.Applicative

-- Local
import Types
import Utility
import B1Matrix
import BMatrix
import HierarchicalClustering
import Print

-- | Command line arguments
data Options = Options { input  :: Maybe String
                       , output :: Maybe String
                       , q      :: Q
                       }

-- | Command line options
options :: Parser Options
options = Options
      <$> optional ( strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> help "The input file, each line is a record"
          )
        )
      <*> optional ( strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "The output file"
          )
        )
      <*> option auto
          ( long "q"
         <> short 'q'
         <> metavar "INT"
         <> value 3
         <> help "The length of the qgrams"
          )

mainFunc :: Options -> IO ()
mainFunc opts = do
    contents <- T.getContents
    let recordsOnly   = V.map Record . V.fromList . T.lines $ contents
        recordIDs     = V.enumFromN 1 . V.length $ recordsOnly
        records       = V.zip recordIDs recordsOnly
        preB1Rows     =
            fmap (recordToQGrams (q opts)) . V.toList $ recordsOnly
        qGramMap      = getQGramMapFromRows preB1Rows
        bMat          = getB
                      . getB2 (Map.size $ unQGramMap qGramMap)
                      . getB1 qGramMap
                      $ preB1Rows

    R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        rB          <- bToRB bMat
        clusterTree <- cluster 0 (ID 1) records rB

        case output opts of
            Nothing  ->
                H.io . T.putStrLn . printClusters . ClusterTree $ clusterTree
            (Just x) ->
                H.io . T.writeFile x . printClusters . ClusterTree $ clusterTree

main :: IO ()
main = execParser opts >>= mainFunc
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Executes the SCAN algorithm to cluster a list of records"
     <> header "scan, Gregory W. Schwartz" )
