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

module DependentGrid.Coord.HardWrap where

import           DependentGrid.Class

import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Kind                     (Type)
import           Data.Semigroup
import           Data.Singletons
import           Data.Type.Monomorphic
import           Data.Type.Natural.Class.Order
import           Data.Type.Ordinal
import qualified GHC.TypeLits                  as GHC

newtype HardWrap (n :: nat) = HardWrap {unHardWrap :: Ordinal n}

deriving instance HasOrdinal nat => Eq (HardWrap (n :: nat))
deriving instance (HasOrdinal nat) => Ord (HardWrap (n :: nat))
deriving instance (SingI n, HasOrdinal nat) => Enum (HardWrap (n :: nat))
deriving instance (SingI n, HasOrdinal nat) => Show (HardWrap (n :: nat))

instance (SingI n, HasOrdinal nat) => Semigroup (HardWrap (n :: nat)) where
    HardWrap a <> HardWrap b =
        HardWrap $
        unsafeFromInt $
        max 0 $ min (demote' (sing :: Sing n) - 1) $ ordToInt a + ordToInt b

instance (SingI n, HasOrdinal nat) => Monoid (HardWrap (n :: nat)) where
    mappend = (<>)
    mempty = HardWrap 0

instance (SingI n, HasOrdinal nat) => Bounded (HardWrap (n :: nat)) where
  minBound = HardWrap 0
  maxBound = HardWrap (fromIntegral $ demote' (sing :: Sing n) - 1)

instance ( SingI n
         , HasOrdinal nat
         , MonomorphicRep (Sing :: nat -> Type) ~ int
         , AdditiveGroup int
         ) =>
         AffineSpace (HardWrap (n :: nat)) where
  type Diff (HardWrap (n :: nat)) = MonomorphicRep (Sing :: nat -> Type)
  HardWrap a .-. HardWrap b = ordToInt a - ordToInt b
  HardWrap a .+^ b =
        HardWrap $
        unsafeFromInt $
        max 0 $ min (demote' (sing :: Sing n) - 1) $ ordToInt a + b

data instance Sing (HardWrap n) = SHardWrap (Sing n)

instance SingI n => SingI (HardWrap n) where
  sing = SHardWrap (sing :: Sing n)

instance ( MonomorphicRep (Sing :: nat -> Type) ~ int
         , SingI (n :: nat)
         , HasOrdinal nat
         , Integral int
         , GHC.KnownNat (AsNat n)
         ) =>
         IsCoord (HardWrap n) where
    type AmountPossible (HardWrap n) = AsNat n
    type ModifyAmountPossible (HardWrap n) f = HardWrap (Apply f (AsNat n))
    coordAsInt (HardWrap a) =
        fromIntegral $ max 0 $ min (demote' (sing :: Sing n) - 1) $ ordToInt a
