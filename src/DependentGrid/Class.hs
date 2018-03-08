{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module DependentGrid.Class where

import           Control.Lens
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Kind                    (Type)
import qualified Data.List.NonEmpty           as NE
import           Data.Maybe                   (fromJust)
import           Data.Proxy                   (Proxy (..))
import           Data.Singletons              (Sing, sing)
import           Data.Singletons.Prelude.Enum (sSucc)
import qualified Data.Type.Natural            as Peano
import           Data.Type.Natural.Builtin    (FromPeano, ToPeano, sToPeano)
import qualified Data.Vector                  as V
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
  allPossible :: MakeSized f => f x
  default allPossible :: (Eq x, Bounded x, Enum x) => MakeSized f => f x
  allPossible =  makeAllBounded
  coordAsInt :: x -> Int
  default coordAsInt :: (AffineSpace x, Integral (Diff x), AdditiveGroup x) => x -> Int
  coordAsInt a = fromIntegral (a .-. zeroV)

class MakeSized f where
  makeSized :: Int -> x -> f x
  makeSized n x = makeSizedFunc n (const x)
  makeSizedFunc :: Int -> (Int -> x) -> f x

instance MakeSized [] where
  makeSized = replicate
  makeSizedFunc n f = take n $ map f [0..]

instance MakeSized V.Vector where
  makeSizedFunc = V.generate
  makeSized = V.replicate

instance MakeSized NE.NonEmpty where
  makeSizedFunc n = NE.fromList . makeSizedFunc n
  makeSized n = NE.fromList . makeSized n

makeAllBounded ::
       forall f a. (Eq a, Enum a, Bounded a, MakeSized f)
    => f a
makeAllBounded =
    makeSizedFunc (fromEnum (maxBound :: a) - fromEnum (minBound :: a)) $ \n ->
        toEnum $ n - fromEnum (minBound :: a) + fromEnum (maxBound :: a)

class Cyclable f where
  moveForeward :: f a -> f a
  moveBackwards :: f a -> f a

instance Cyclable NE.NonEmpty where
  moveForeward (a NE.:| as) = NE.fromList (as ++ [a])
  moveBackwards (a NE.:| as) = NE.fromList (last as : a : init as)
