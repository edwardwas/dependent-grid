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

data CellState
    = Alive
    | Dead
    deriving (Eq, Show, Ord)

flipCellState :: CellState -> CellState
flipCellState Alive = Dead
flipCellState Dead  = Alive

cellStateFromBool True  = Alive
cellStateFromBool False = Dead

gameOfLife ::
       forall a b f i.
       ( IsCoord a
       , IsCoord b
       , MonadZip f
       , GetByIndex f Int
       , MakeSized f
       , AllDiffEq '[ a, b] i
       , AffineSpace a
       , AffineSpace b
       , Enum i
       , Num i
       )
    => FocusedGrid '[ a, b] f CellState
    -> FocusedGrid '[ a, b] f CellState
gameOfLife =
  let helper g =
        let alive = Alive == extract g
            l = fromIntegral (length neigh) - if alive then 1 else 0
            neigh =
              filter (== Alive) $
              map (`peek` g) $ mooreNeighborhood (1 :: i) (pos g)
        in if alive
             then if l < 2 || l > 3
                    then Dead
                    else Alive
             else if l == 3
                    then Alive
                    else Dead
  in extend helper

randomState ::
       ( MakeSized f
       , MonadZip f
       , Traversable f
       , All IsCoord cs
       , MonadRandom m
       , SingI cs
       )
    => m (Grid cs f CellState)
randomState = sequenceA $ pure (cellStateFromBool <$> getRandom)

gridWidget :: (Foldable f, Functor f) => Grid '[ a, b] f CellState -> Widget n
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

data AppState = AppState
    { _isRunning :: Bool
    , _grid      :: Grid '[ Periodic 80, Periodic 25] V.Vector CellState
    }
makeLenses ''AppState

stepWorld ::
       ( Diff a ~ Diff b
       , Enum (Diff a)
       , Enum (Diff b)
       , Num (Diff a)
       , Num (Diff b)
       , SingI a
       , SingI b
       , MakeSized f
       , Monoid a
       , Monoid b
       , GetByIndex f Int
       , MonadZip f
       , IsCoord a
       , IsCoord b
       , AffineSpace a
       , AffineSpace b
       )
    => Grid '[ a, b] f CellState
    -> Grid '[ a, b] f CellState
stepWorld = view focusedGrid . gameOfLife . makeFocusGrid

golApp :: App AppState AppEvent ()
golApp =
    let helper s (VtyEvent (V.EvKey V.KEsc _)) = halt s
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
             \g ->
                 [ borderWithLabel
                       (str $
                        "Game of Life" ++
                        if (g ^. isRunning)
                            then ""
                            else " - Paused") $
                   gridWidget $ g ^. grid
                 ]
       , appChooseCursor = \_ _ -> Nothing
       , appHandleEvent = helper
       , appStartEvent = pure
       , appAttrMap = \_ -> attrMap V.defAttr []
       }

data AppEvent = Tick Double

run = do
    chan <- newBChan 2
    timerSync <-
        async $ forever $ writeBChan chan (Tick 0.1) >> threadDelay (10 ^ 5)
    customMain (V.mkVty V.defaultConfig) (Just chan) golApp $
        AppState True $
        makeGlider (AddCoord (Periodic 2) $ AddCoord (Periodic 3) EmptyCoord) $
        pure Dead
    cancel timerSync
    return ()

main = run
