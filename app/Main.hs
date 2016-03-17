{- scan
Gregory W. Schwartz

Executes the SCAN algorithm to cluster a list of records
http://research.google.com/pubs/pub36940.html
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

-- Standard
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Vector as V
import Control.Monad

-- Cabal
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Text as PT
import qualified Pipes.Text.IO as PT
import qualified Pipes.Prelude.Text as PT
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
data Options = Options { input          :: Maybe String
                       , output         :: Maybe String
                       , outputTree     :: Maybe String
                       , q              :: Int
                       , alphabetString :: Maybe String
                       }

-- | Command line options
options :: Parser Options
options = Options
      <$> optional ( strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> help "The input file, each line is a record."
          )
        )
      <*> optional ( strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> help "The output file."
          )
        )
      <*> optional ( strOption
          ( long "output-tree"
         <> short 'o'
         <> metavar "FILE"
         <> help "The output file for the structure of the tree."
          )
        )
      <*> option auto
          ( long "q"
         <> short 'q'
         <> metavar "[3] | INT"
         <> value 3
         <> help "The length of the qgrams."
          )
      <*> optional ( strOption
          ( long "alphabet"
         <> short 'a'
         <> metavar "STRING"
         <> help "The alphabet (all characters) used in the records.\
                 \ For instance, for a records of just capital characters with\
                 \ periods and commas,\
                 \ the alphabet would be \"ABCDEFGHIJKLMNOPQRSTUVWXYZ.,\".\
                 \ If empty, uses the (very large and slow)\
                 \ maxBound for characters."
          )
        )

mainFunc :: Options -> IO ()
mainFunc opts = do
    let readPipes = case input opts of
                        Nothing  -> PT.stdinLn
                        (Just x) -> PT.readFileLn x

    contents <- fmap nub' . PT.runSafeT . runEffect $ P.toListM $ readPipes
                    >-> P.filter (not . T.null)
                    >-> P.map (\ !x -> ( Record x
                                       , recordToQGrams (Q $ q opts) (Record x)
                                       )
                              )
    let recordsOnly   = V.fromList . fmap fst $ contents
        recordIDs     = V.enumFromN 1 . V.length $ recordsOnly
        records       = V.zip recordIDs recordsOnly
        alphabet      = fmap getAlphabet . alphabetString $ opts
        preB1Rows     = fmap snd contents
        numQGrams     = case alphabet of
                            Nothing -> (fromEnum (maxBound :: Char)) ^ (q opts)
                            (Just (Alphabet x)) -> (Map.size x) ^ (q opts)
        bMat          = getB
                      . getB2 numQGrams
                      . getB1 alphabet (Q $ q opts)
                      $ preB1Rows

    R.withEmbeddedR R.defaultConfig $ R.runRegion $ do
        rB          <- bToRB bMat
        clusterTree <- cluster 0 (ID 1) records rB

        -- Output clusters
        case output opts of
            Nothing  ->
                H.io . T.putStrLn . printClusters . ClusterTree $ clusterTree
            (Just x) ->
                H.io . T.writeFile x . printClusters . ClusterTree $ clusterTree

        -- Output tree
        case (outputTree opts) of
            Nothing  -> return ()
            (Just x) -> H.io
                      . T.writeFile x
                      . T.pack
                      . show
                      . printTree
                      . ClusterTree
                      $ clusterTree

main :: IO ()
main = execParser opts >>= mainFunc
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Executes the SCAN algorithm to cluster a list of records"
     <> header "scan, Gregory W. Schwartz" )
