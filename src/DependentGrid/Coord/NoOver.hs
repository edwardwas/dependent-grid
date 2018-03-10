{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module DependentGrid.Coord.NoOver where

import           DependentGrid.Class

import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Kind                     (Type)
import           Data.Semigroup
import           Data.Semigroup
import           Data.Singletons
import           Data.Singletons.Prelude.Ord
import           Data.Type.Monomorphic
import           Data.Type.Natural.Class.Order
import           Data.Type.Ordinal

data NoOver n
  = InsideGrid (Ordinal n)
  | OutsideGrid

deriving instance HasOrdinal nat => Eq (NoOver (n :: nat))
deriving instance (SingI n, HasOrdinal nat) => Show (NoOver (n :: nat))

instance (SingI n, HasOrdinal nat) => Semigroup (NoOver (n :: nat)) where
  OutsideGrid <> _ = OutsideGrid
  _ <> OutsideGrid = OutsideGrid
  InsideGrid a <> InsideGrid b
    | c >= 0 && c < demote' (sing :: Sing n) = InsideGrid $ unsafeFromInt c
    | otherwise = OutsideGrid
    where
      c = ordToInt a + ordToInt b

instance (SingI n, HasOrdinal nat) => Monoid (NoOver (n :: nat)) where
  mappend = (<>)
  mempty = InsideGrid 0

instance ( int ~ MonomorphicRep (Sing :: nat -> Type)
         , AdditiveGroup int
         , Ord int
         , SingI n
         , HasOrdinal nat
         ) =>
         AffineSpace (NoOver (n :: nat)) where
  type Diff (NoOver (n :: nat)) = Maybe (MonomorphicRep (Sing :: nat -> Type))
  OutsideGrid .-. _ = Nothing
  _ .-. OutsideGrid = Nothing
  InsideGrid a .-. InsideGrid b
    | c >= 0 && c < demote' (sing :: Sing n) = Just c
    | otherwise = Nothing
    where
      c = ordToInt a - ordToInt b
  OutsideGrid .+^ _ = OutsideGrid
  _ .+^ Nothing = OutsideGrid
  InsideGrid a .+^ Just b
    | c >= 0 && c < demote' (sing :: Sing n) = InsideGrid $ unsafeFromInt c
    | otherwise = OutsideGrid
      where c = ordToInt a + b

instance (SingI n, HasOrdinal nat) => IsCoord (NoOver (n :: nat)) where
  type AmountPossible (NoOver n) = AsNat n
  type ModifyAmountPossible (NoOver n) f = NoOver (Apply f (AsNat n))
  allPossible =
    makeSizedFunc (fromIntegral $ demote' (sing :: Sing n)) $
    InsideGrid . unsafeFromInt . fromIntegral
  coordAsInt (InsideGrid n) = fromIntegral $ ordToInt n
  coordAsInt OutsideGrid    = error "Tried to acces outside grid"
