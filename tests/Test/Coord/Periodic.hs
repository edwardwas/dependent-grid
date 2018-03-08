{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Coord.Periodic where

import           DependentGrid.Coord.Periodic
import           Test.Util

import           Data.Proxy
import           Data.Type.Ordinal
import           GHC.TypeLits
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range
import           Test.Tasty

genPeriodic :: forall n . KnownNat n => Gen (Periodic (n :: Nat))
genPeriodic =
    Periodic . unsafeFromInt <$>
    Gen.integral (Range.linear 0 (natVal (Proxy :: Proxy n) - 1))

tests =
    let g :: Gen (Periodic 10) = genPeriodic
    in testGroup "Periodic 10" [semigroupTest g, monoidTest g, groupTest g]
