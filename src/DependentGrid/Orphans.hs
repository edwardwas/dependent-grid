{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE UndecidableInstances       #-}

module DependentGrid.Orphans where

import           Control.Monad.Identity
import           Data.AdditiveGroup
import           Data.Kind
import           Data.Proxy
import           Data.Singletons
import           Data.Sized                as S
import qualified Data.Type.Natural         as Peano
import           Data.Type.Natural.Builtin
import           Data.Type.Ordinal
import           Generics.SOP              (Generic)
import qualified GHC.Generics              as GHC (Generic)
import qualified GHC.TypeLits              as GHC

instance (S.ListLikeF f, SingI n, HasOrdinal nat, AdditiveGroup a) =>
         AdditiveGroup (S.Sized f (n :: nat) a) where
  zeroV = S.withListLikeF (Proxy :: Proxy (f a)) $ S.replicate' zeroV
  (^+^) = S.withListLikeF (Proxy :: Proxy (f a)) $ S.zipWithSame (^+^)
  negateV = S.fmap negateV

class IsTypeNum (k :: Type) where
  type AsNat (a :: k) :: GHC.Nat
  type FromNat (a :: GHC.Nat) :: k

instance IsTypeNum GHC.Nat where
  type AsNat a = a
  type FromNat a = a

instance IsTypeNum Peano.Nat where
  type AsNat a = FromPeano a
  type FromNat a = ToPeano a

instance Generic (Identity a)

deriving instance AdditiveGroup a => AdditiveGroup (Identity a)
