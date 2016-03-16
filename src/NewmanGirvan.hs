{- NewmanGirvan
Gregory W. Schwartz

Collections the functions pertaining to calculating the Newman-Girvan modularity
-}

{-# LANGUAGE QuasiQuotes #-}

module NewmanGirvan
    ( ngModularity
    ) where

-- Standard

-- Cabal
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ

-- Local
import Types

-- | Finds the Newman Girvan modularity from the B matrix and its partitioning
ngModularity :: R.SomeSEXP s -> R.SomeSEXP s -> R s (R.SomeSEXP s)
ngModularity mat part =
    [r| partt = matrix(rep(1, times=nrow(mat_hs)))
        part1 = part_hs
        part2 = abs(part_hs - 1)
        l   = (t(t(mat_hs) %*% partt) %*% (t(mat_hs) %*% partt)) - nrow(mat_hs)
        l1  = (t(t(mat_hs) %*% part1) %*% (t(mat_hs) %*% partt)) - sum(part1)
        l2  = (t(t(mat_hs) %*% part2) %*% (t(mat_hs) %*% partt)) - sum(part2)
        o11 = (t(t(mat_hs) %*% part1) %*% (t(mat_hs) %*% part1)) - sum(part1)
        o22 = (t(t(mat_hs) %*% part2) %*% (t(mat_hs) %*% part2)) - sum(part2)
        modularity = ((o11 / l) - ((l1 / l) ^ 2)) + ((o22 / l) - ((l2 / l) ^ 2))
        modularity[1,1]
    |]
