{- CMatrix
Gregory W. Schwartz

Collections the functions pertaining to the creation of the C Matrix
-}

{-# LANGUAGE QuasiQuotes #-}

module CMatrix
    ( getC
    ) where

-- Standard

-- Cabal

-- Local
import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ

-- Local
import Types

-- | Get the D matrix from the B Matrix
getD12 :: R.SomeSEXP s -> R s (R.SomeSEXP s)
getD12 b =
    [r| Diagonal(x=as.vector((b_hs %*% (t(b_hs) %*% matrix(rep(1, times=nrow(b_hs)))))^(-1/2))) |]

-- | Get the C matrix from the B matrix
getC :: R.SomeSEXP s -> R s (R.SomeSEXP s)
getC b = do
    d <- getD12 b
    c <- [r| d_hs %*% b_hs |]
    return c
