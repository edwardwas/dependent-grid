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
import qualified Data.List.NonEmpty           as NE
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.Prelude.Enum
import qualified Data.Type.Natural            as Peano
import           Data.Type.Natural.Builtin
import           Data.Type.Ordinal
import qualified Data.Vector                  as V
import           Generics.SOP                 (Generic)
import qualified GHC.Generics                 as GHC (Generic, Generic1)
import qualified GHC.TypeLits                 as GHC

--instance (S.ListLikeF f, SingI n, HasOrdinal nat, AdditiveGroup a) =>
--         AdditiveGroup (S.Sized f (n :: nat) a) where
--  zeroV = S.withListLikeF (Proxy :: Proxy (f a)) $ S.replicate' zeroV
--  (^+^) = S.withListLikeF (Proxy :: Proxy (f a)) $ S.zipWithSame (^+^)
--  negateV = S.fmap negateV

instance Generic (Identity a)

deriving instance AdditiveGroup a => AdditiveGroup (Identity a)
