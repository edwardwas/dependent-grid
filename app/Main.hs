{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Main where

import           DependentGrid.Class
import           DependentGrid.Coord
import           DependentGrid.Coord.HardWrap
import           DependentGrid.Coord.NoOver
import           DependentGrid.Coord.Periodic
import           DependentGrid.Grid

import           Codec.Picture                (PixelRGBA8 (..), writePng)
import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad.Random
import           Control.Monad.Zip
import           Data.AffineSpace
import           Data.Foldable                (toList)
import           Data.Singletons
import qualified Data.Vector                  as V
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture
import           System.Random

data CellState
    = Alive
    | Dead
    deriving (Eq, Show, Ord)

cellStateFromBool True  = Alive
cellStateFromBool False = Dead

gameOfLife ::
     ( IsCoord a
     , IsCoord b
     , MonadZip f
     , GetByIndex f Int
     , MakeSized f
     , AllDiffEq '[ a, b] Int
     , AffineSpace a
     , AffineSpace b
     )
  => FocusedGrid '[ a, b] f CellState
  -> FocusedGrid '[ a, b] f CellState
gameOfLife =
  let helper g =
        let alive = Alive == extract g
            neigh =
              filter (== Alive) $
              map (`peek` g) $ mooreNeighborhood (1 :: Int) (pos g)
        in if alive
             then if length neigh < 2 || length neigh > 3
                    then Dead
                    else Alive
             else if length neigh == 3
                    then Alive
                    else Dead
  in extend helper

showGrid :: (Foldable f, Functor f) => Grid '[a,b] f CellState ->  String
showGrid = unlines . toList . fmap (toList . fmap helper) . collapseGrid
  where helper Alive = '#'
        helper Dead  = '.'

randomState ::
     (MakeSized f, MonadZip f, Traversable f, AllAmountPossibleKnowNat cs, MonadRandom m, SingI cs)
  => m (Grid cs f CellState)
randomState = sequenceA $ pure (cellStateFromBool <$> getRandom)

main = undefined
