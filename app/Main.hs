{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Main where

import           DependentGrid.Class
import           DependentGrid.Coord
import           DependentGrid.Coord.HardWrap
import           DependentGrid.Coord.Periodic
import           DependentGrid.Grid

import           Codec.Picture                (PixelRGBA8 (..), writePng)
import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad.Random
import           Data.AffineSpace
import           Data.Singletons
import qualified Data.Vector                  as V
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture
import           System.Random

data CellState
    = On
    | Off
    deriving (Eq, Show, Ord)

cellStateFromBool True  = On
cellStateFromBool False = Off

rule30 :: [CellState] -> CellState
rule30 [On, On, On]    = Off
rule30 [On, On, Off]   = Off
rule30 [On, Off, On]   = Off
rule30 [Off, Off, Off] = Off
rule30 _               = On

type World = Grid '[Periodic 300] []
type FocusedWorld a = FocusedGrid '[Periodic 300] [] a

randomWorld :: MonadRandom m => m (World CellState)
randomWorld = sequenceA $ pure (cellStateFromBool <$> getRandom)

startCenter :: World CellState
startCenter = pure Off & gridIndexed (AddCoord (Periodic 150) EmptyCoord) .~ On

stepRule :: ([CellState] -> CellState) -> FocusedWorld CellState -> CellState
stepRule rule g =
    rule $ map (\c -> g ^. focusedGrid . gridIndexed c) $ mooreNeighborhood 1 $
    pos g

displayWorld :: FocusedWorld CellState -> String
displayWorld =
    let helper On  = '#'
        helper Off = '.'
    in collapseGrid . view focusedGrid . fmap helper

run :: Int -> World CellState -> [World CellState]
run n =
    map (view focusedGrid) .
    take n . iterate (extend $ stepRule rule30). makeFocusGrid

data DrawInfo = DrawInfo {squareSize :: Float} deriving (Eq,Show)

drawRow :: DrawInfo -> Int -> World CellState -> Drawing px ()
drawRow DrawInfo {..} rn w =
    let helper [x] On =
            fill $
            rectangle
                (V2 (x * squareSize) (fromIntegral rn * squareSize))
                squareSize
                squareSize
        helper _ _ = mempty
    in ifoldMap (helper . fmap fromIntegral . coordAsList) w

makeImage =
    writePng "test.png" . renderDrawing 600 600 (PixelRGBA8 255 255 255 255) .
    withTexture (uniformTexture $ PixelRGBA8 255 0 0 255) .
    mconcat . zipWith (drawRow (DrawInfo 2)) [0 ..]

main = makeImage $ run 300 startCenter
