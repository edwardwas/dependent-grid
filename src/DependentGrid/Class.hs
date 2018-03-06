{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module DependentGrid.Class where

import           Control.Lens
import           Data.Kind                    (Type)
import qualified Data.ListLike                as L
import           Data.Singletons              (Sing, sing)
import           Data.Singletons.Prelude.Enum (sSucc)
import qualified Data.Type.Natural            as Peano
import           Data.Type.Natural.Builtin    (FromPeano, ToPeano, sToPeano)
import qualified GHC.TypeLits                 as GHC

class IsTypeNum (k :: Type) where
  type AsNat (a :: k) :: GHC.Nat
  type AsPeano (a :: k) :: Peano.Nat
  asNat :: Sing (a :: k)  -> Sing (AsNat a)
  asPeano :: Sing (a :: k) -> Sing (AsPeano a)

instance IsTypeNum GHC.Nat where
  type AsNat a = a
  type AsPeano a = ToPeano a
  asNat = id
  asPeano = sToPeano

instance IsTypeNum Peano.Nat where
  type AsNat a = FromPeano a
  type AsPeano a = a
  asNat Peano.SZ     = sing :: Sing 0
  asNat (Peano.SS n) = sSucc $ asNat n
  asPeano = id

class IsCoord x where
  type AmountPossible x :: GHC.Nat
  allPossible :: L.ListLike (f x) x => f x
