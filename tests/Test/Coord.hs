{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Coord where

import           DependentGrid.Coord
import           DependentGrid.Coord.Periodic
import           Test.Coord.Periodic          (genPeriodic)
import           Test.Util

import           Generics.SOP
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range
import           Test.Tasty

genCoord :: NP Gen cs -> Gen (Coord cs)
genCoord Nil       = pure EmptyCoord
genCoord (a :* as) = AddCoord <$> a <*> genCoord as

tests =
    let g :: Gen (Coord '[Periodic 10, Periodic 4]) = genCoord $ genPeriodic @ 10 :* genPeriodic @ 4 :* Nil
    in testGroup "Coord [Periodic 10, Periodic 4]" [semigroupTest g, monoidTest g, groupTest g, additiveGroupTest g]
