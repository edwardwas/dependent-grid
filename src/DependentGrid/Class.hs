{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE TypeInType#-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module DependentGrid.Class where

import           Control.Lens
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Kind                    (Type)
import qualified Data.List.NonEmpty           as NE
import           Data.Maybe                   (fromJust)
import           Data.Proxy                   (Proxy (..))
import           Data.Singletons              
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

class GHC.KnownNat (AmountPossible x) => IsCoord (x :: Type) where
  type AmountPossible x :: GHC.Nat
  type ModifyAmountPossible x (f :: GHC.Nat ~> GHC.Nat) :: Type
  allPossible :: MakeSized f => f x
  default allPossible :: (Eq x, Bounded x, Enum x) => MakeSized f => f x
  allPossible =  makeAllBounded
  coordAsInt :: x -> Int
  default coordAsInt :: (AffineSpace x, Integral (Diff x), AdditiveGroup x) => x -> Int
  coordAsInt a = fromIntegral (a .-. zeroV)
  coordFromInt :: Int -> Maybe x
  default coordFromInt ::
         (AffineSpace x, Monoid x, Integral (Diff x), AdditiveGroup (Diff x))
      => Int
      -> Maybe x
  coordFromInt n = Just (mempty .+^ (fromIntegral n :: Diff x))

class MakeSized f where
  makeSized :: Int -> x -> f x
  makeSized n x = makeSizedFunc n (const x)
  makeSizedFunc :: Int -> (Int -> x) -> f x
  takeSized :: Int -> f x -> Maybe (f x)

instance MakeSized [] where
  makeSized = replicate
  makeSizedFunc n f = take n $ map f [0..]
  takeSized n xs 
    | length xs < n = Nothing
    | otherwise = Just $ take n xs

instance MakeSized V.Vector where
  makeSizedFunc = V.generate
  makeSized = V.replicate
  takeSized n xs
    | length xs < n = Nothing
    | otherwise = Just $ V.take n xs

makeAllBounded ::
       forall f a. (Eq a, Enum a, Bounded a, MakeSized f)
    => f a
makeAllBounded =
    makeSizedFunc (fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1) $ toEnum

class Cyclable f where
  moveForeward :: f a -> f a
  moveBackwards :: f a -> f a

instance Cyclable NE.NonEmpty where
  moveForeward (a NE.:| as) = NE.fromList (as ++ [a])
  moveBackwards (a NE.:| as) = NE.fromList (last as : a : init as)

class GetByIndex f i | f -> i where
  getByIndex :: i -> Lens' (f a) a

instance GetByIndex [] Int where
  getByIndex i = singular (ix i)

instance GetByIndex V.Vector Int where
  getByIndex i = singular (ix i)

instance GetByIndex NE.NonEmpty Int where
  getByIndex i = singular (ix i)
