{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module DependentGrid.Class where

import           Control.Lens
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Kind                    (Type)
import qualified Data.ListLike                as L
import           Data.Maybe                   (fromJust)
import           Data.Singletons              (Sing, sing)
import           Data.Singletons.Prelude.Enum (sSucc)
import qualified Data.Type.Natural            as Peano
import           Data.Type.Natural.Builtin    (FromPeano, ToPeano, sToPeano)
import           Data.Unfoldable
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
  allPossible :: Unfoldable f => f x
  default allPossible :: (Unfoldable f, Eq x, Bounded x, Enum x) => f x
  allPossible =  makeAllBounded
  coordAsInt :: x -> Int
  default coordAsInt :: (AffineSpace x, Integral (Diff x), AdditiveGroup x) => x -> Int
  coordAsInt a = fromIntegral (a .-. zeroV)

makeSized :: (Unfoldable f, Ord a, Num a) => a -> x -> Maybe (f x)
makeSized x a
  | x < 0 = Nothing
  | otherwise = unfoldr (\n -> if n > 0 then Just (a,n - 1) else Nothing) x

makeAllBounded :: (Eq a, Enum a, Bounded a, Unfoldable f) => f a
makeAllBounded =
  fromJust $
  unfoldr
    (>>= (\a ->
            if a == maxBound
              then Just (a, Nothing)
              else Just (a, Just $ succ a)))
    (Just minBound)
