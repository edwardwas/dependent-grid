{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Handler
import           Parser
import           Types

import           DependentGrid.Class
import           DependentGrid.Coord
import           DependentGrid.Coord.HardWrap
import           DependentGrid.Coord.NoOver
import           DependentGrid.Coord.Periodic
import           DependentGrid.Grid

import           Brick
import           Brick.BChan
import           Brick.Widgets.Border
import           Control.Comonad
import           Control.Comonad.Store
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.Random
import           Control.Monad.State
import           Control.Monad.Zip
import           Data.AffineSpace
import           Data.Foldable                (toList)
import           Data.Singletons
import qualified Data.Vector                  as V
import           Generics.SOP                 (All)
import qualified Graphics.Vty                 as V
import           System.Random


randomState ::
       ( GridLike cs f
       , MonadRandom m
       , Traversable f
       )
    => m (Grid cs f CellState)
randomState = sequenceA $ pure (cellStateFromBool <$> getRandom)

gridWidget :: (Foldable f, GridLike '[a,b] f) => Grid '[ a, b] f CellState -> Widget n
gridWidget g =
    let helper Alive = str "#"
        helper Dead  = str "."
    in hBox $ toList $ vBox . toList <$> collapseGrid (helper <$> g)

makeGlider center =
    execState $
    forM_ [-1 .. 1] $ \x ->
        forM_ [-1 .. 1] $ \y ->
            gridIndexed (center .+^ (x, y)) .=
            if (x, y) `elem` [(0,-1), (1,0), (-1,1), (0,1), (1,1)]
                then Alive
                else Dead

data RenderInfo = RenderInfo
    { squareSize :: Double
    } deriving (Eq, Show)

stepWorld ::
       ( Diff a ~ Diff b
       , Enum (Diff a)
       , Enum (Diff b)
       , Num (Diff a)
       , Num (Diff b)
       , GridLike '[a,b] f
       , Monoid a
       , Monoid b
       , AffineSpace a
       , AffineSpace b
       )
    => Grid '[ a, b] f CellState
    -> Grid '[ a, b] f CellState
stepWorld = view focusedGrid . gameOfLife . makeFocusGrid

makePrisms ''BrickEvent

golApp :: App AppState AppEvent ()
golApp =
    let
        helper s (VtyEvent (V.EvKey V.KEsc _)) = halt s
        helper s (VtyEvent (V.EvKey (V.KChar 'p') _)) =
            continue $ s & isRunning %~ not
        helper s (VtyEvent (V.EvKey V.KEnter _)) =
            continue $ over grid stepWorld s
        helper s (AppEvent (Tick _)) =
            continue $
            if s ^. isRunning
                then over grid stepWorld s
                else s
        helper s _ = continue s
    in App
       { appDraw =
             \(Running g) ->
                 [ borderWithLabel
                       (str $
                        "Game of Life" ++
                        if (g ^. isRunning)
                            then ""
                            else " - Paused") $
                   gridWidget $ g ^. grid
                 ]
       , appChooseCursor = \_ _ -> Nothing
       , appHandleEvent = \(Running g) -> fmap (fmap Running) . helper g
       , appStartEvent = pure
       , appAttrMap = \_ -> attrMap V.defAttr []
       }

data AppEvent = Tick Double

run = do
    chan <- newBChan 2
    timerSync <-
        async $ forever $ writeBChan chan (Tick 0.1) >> threadDelay (10 ^ 5)
    Just spaceShip <- loadLifeFile "patterns/lwss_106.lif"
    customMain (V.mkVty V.defaultConfig) (Just chan) golApp $
        Running $ RunningState True $ spaceShip
    cancel timerSync
    return ()

main = run
