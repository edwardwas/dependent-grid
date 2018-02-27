{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module DependentGrid.Coord where

import           DependentGrid.Coord.Periodic
import           DependentGrid.Orphans

import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Constraint
import           Data.Group
import           Data.Kind                          (Type)
import           Data.List                          (intercalate)
import           Data.Semigroup
import           Data.Singletons
import           Data.Singletons.Prelude.List       (Sing (..))
import           Data.Singletons.Prelude.Num
import qualified Data.Sized                         as S
import           Data.Type.Natural.Class.Arithmetic

import qualified GHC.TypeLits                       as GHC

type family AllConstraint c f xs :: Constraint where
  AllConstraint c f '[] = ()
  AllConstraint c f (x ': xs) = (c (f x), AllConstraint c f xs)

type family Length (xs :: [k]) :: GHC.Nat where
  Length '[] = 0
  Length (_ ': xs) = S (Length  xs)

data Coord (c :: nat -> Type) (xs :: [nat]) where
  NoCoord :: Coord c '[]
  AddCoord :: c x -> Coord c xs -> Coord c (x : xs)

instance AllConstraint Eq c xs => Eq (Coord c xs) where
  NoCoord == NoCoord = True
  AddCoord (a :: c x) as == AddCoord b bs = (a == b && as == bs)

instance (SingI xs, AllConstraint Show c xs) => Show (Coord c xs) where
  show coord =
    let helper :: forall c xs . (SingI xs, AllConstraint Show c xs) => Coord c xs -> [String]
        helper NoCoord                  = []
        helper (AddCoord a as) = case (sing :: Sing xs) of
          SCons (_ :: Sing y) sTail -> show a : withSingI sTail (helper as)
    in "Coord (" ++ intercalate ", " (helper coord) ++ ")"

instance AllConstraint Semigroup c xs => Semigroup (Coord c xs) where
  NoCoord <> NoCoord = NoCoord
  AddCoord a as <> AddCoord b bs = AddCoord (a <> b) (as <> bs)

instance (SingI xs, AllConstraint Monoid c xs, AllConstraint Semigroup c xs) => Monoid (Coord c xs) where
  mappend = (<>)
  mempty = case (sing :: Sing xs) of
    SNil          -> NoCoord
    SCons _ stail -> AddCoord mempty $ withSingI stail mempty

instance ( AllConstraint Monoid c xs
         , AllConstraint Semigroup c xs
         , AllConstraint Group c xs
         , SingI xs
         ) =>
         Group (Coord c xs) where
  invert NoCoord         = NoCoord
  invert (AddCoord a as) = AddCoord (invert a) as

instance (SingI xs, AllConstraint AdditiveGroup c xs) => AdditiveGroup (Coord c xs) where
  zeroV = case (sing :: Sing xs) of
      SNil          -> NoCoord
      SCons _ stail -> AddCoord zeroV $ withSingI stail zeroV
  a ^+^ b =
    let helper :: AllConstraint AdditiveGroup c ys => Coord c ys -> Coord c ys -> Coord c ys
        helper NoCoord NoCoord = NoCoord
        helper (AddCoord a as) (AddCoord b bs) = AddCoord (a ^+^ b) (helper as bs)
    in helper a b
  negateV c =
    let helper :: AllConstraint AdditiveGroup c ys => Coord c ys -> Coord c ys
        helper NoCoord         = NoCoord
        helper (AddCoord a as) = AddCoord (negateV a) (helper as)
    in helper c
