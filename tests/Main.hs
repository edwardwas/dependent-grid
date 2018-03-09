{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           DependentGrid.Coord.NoOver

import qualified Test.Coord
import qualified Test.Coord.Periodic        as P
import qualified Test.Grid
import           Test.Util

import           Data.Coerce
import           Data.Proxy
import           Data.Type.Ordinal
import           GHC.TypeLits
import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import           Test.Tasty

genNoOver :: forall n . KnownNat (n :: Nat) => Gen (NoOver n)
genNoOver =
    InsideGrid . unsafeFromInt <$>
    Gen.integral (Range.linear 0 (natVal (Proxy :: Proxy n) - 1))

noOverTest =
  let g :: Gen (NoOver 10) = genNoOver
  in testGroup "NoOver 10" [semigroupTest g, monoidTest g]

main =
  defaultMain $
  testGroup "Tests" [P.tests, Test.Coord.tests, Test.Grid.tests, noOverTest]
