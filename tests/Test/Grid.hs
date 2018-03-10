{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Test.Grid where

import           DependentGrid.Class
import           DependentGrid.Coord
import           DependentGrid.Coord.Periodic
import           DependentGrid.Grid
import           Test.Coord                   (genCoord)
import           Test.Coord.Periodic          (genPeriodic)
import           Test.Util

import           Control.Monad.Zip
import           Data.Singletons
import           Generics.SOP                 hiding (SingI)
import           Hedgehog
import qualified Hedgehog.Gen                 as Gen
import qualified Hedgehog.Range               as Range
import           Test.Tasty

genGrid ::
     (SingI cs, All IsCoord cs, Traversable f, MonadZip f, MakeSized f)
  => Gen a
  -> Gen (Grid cs f a)
genGrid g = sequenceA $ pure g

tests =
  let g :: Gen (Grid '[ Periodic 3, Periodic 4] [] Int) =
        genGrid $ Gen.integral $ Range.linear 0 100
  in testGroup "Grid '[Periodic 3, Periodic 4] [] Int" [comonadTest $ makeFocusGrid <$> g]
