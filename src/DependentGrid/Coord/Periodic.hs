{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module DependentGrid.Coord.Periodic where

import           DependentGrid.Class

import           Control.Monad                 (unless)
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Group
import           Data.Kind
import           Data.Semigroup
import           Data.Singletons.TH
import           Data.Type.Monomorphic
import           Data.Type.Natural.Class.Order
import           Data.Type.Ordinal
import           GHC.TypeLits

newtype Periodic (n :: nat) = Periodic {unPeriodic :: Ordinal n}

deriving instance HasOrdinal nat => Eq (Periodic (n :: nat))
deriving instance (SingI n, HasOrdinal nat) => Show (Periodic (n :: nat))
deriving instance (HasOrdinal nat) => Ord (Periodic (n :: nat))
deriving instance (SingI n, HasOrdinal nat) => Enum (Periodic (n :: nat))

instance (SingI n, HasOrdinal nat) => Semigroup (Periodic (n :: nat)) where
  Periodic a <> Periodic b =
    Periodic $
    unsafeFromInt ((ordToInt a + ordToInt b) `mod` (demote' (sing :: Sing n)))

instance (SingI n, HasOrdinal nat) => Monoid (Periodic (n :: nat)) where
  mappend = (<>)
  mempty = Periodic 0

instance (SingI n, HasOrdinal nat) => Group (Periodic (n :: nat)) where
    invert (Periodic a) =
        Periodic $
        unsafeFromInt ((negate $ ordToInt a) `mod` (demote' (sing :: Sing n)))

instance (SingI n, HasOrdinal nat) => AdditiveGroup (Periodic (n :: nat)) where
  zeroV = mempty
  (^+^) = (<>)
  negateV = invert

instance (SingI n, HasOrdinal nat) => Bounded (Periodic (n :: nat)) where
  minBound = Periodic 0
  maxBound = Periodic (fromIntegral $ demote (Monomorphic (sing :: Sing n)) - 1)

instance ( SingI n
         , HasOrdinal nat
         , MonomorphicRep (Sing :: nat -> Type) ~ int
         , AdditiveGroup int
         ) =>
         AffineSpace (Periodic (n :: nat)) where
  type Diff (Periodic (n :: nat)) = MonomorphicRep (Sing :: nat -> Type)
  Periodic a .-. Periodic b = ordToInt a - ordToInt b
  Periodic a .+^ b =
    Periodic $ unsafeFromInt ((ordToInt a + b) `mod` (demote' (sing :: Sing n)))

instance ( PeanoOrder nat
         , mr ~ (MonomorphicRep (Sing :: nat -> Type))
         , Show mr
         , ToJSON mr
         , Integral mr
         , Monomorphicable (Sing :: nat -> Type)
         , SingI n
         ) =>
         ToJSON (Periodic (n :: nat)) where
  toJSON (Periodic n) = object ["n" .= ordToInt n, "maxN" .= demote' (sing :: Sing n)]

instance ( PeanoOrder nat
         , mr ~ (MonomorphicRep (Sing :: nat -> Type))
         , AdditiveGroup mr
         , FromJSON mr
         , Eq mr
         , Show mr
         , Integral mr
         , SingI n
         , Monomorphicable (Sing :: nat -> Type)
         ) =>
         FromJSON (Periodic (n :: nat)) where
  parseJSON (Object v) = do
    recordedMax <- v .: "maxN"
    let hereMax = demote' (sing :: Sing n)
    unless (recordedMax == hereMax) $ fail "Sized mismatch"
    (mempty .+^) <$> (v .: "n")
  parseJSON _ = fail "Not object"

data instance Sing (Periodic n) = SPeriodic (Sing n)

instance SingI n => SingI (Periodic n) where
  sing = SPeriodic (sing :: Sing n)

instance ( MonomorphicRep (Sing :: nat -> Type) ~ int
         , SingI (n :: nat)
         , HasOrdinal nat
         , AdditiveGroup int
         ) =>
         IsCoord (Periodic n) where
  type AmountPossible (Periodic n) = AsNat n
