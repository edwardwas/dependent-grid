{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Types where

import           DependentGrid.Coord
import           DependentGrid.Coord.Periodic
import           DependentGrid.Grid

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Data.AffineSpace
import qualified Data.Vector                  as V

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
       ( GridLike '[a,b] f
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

data RunningState = RunningState
    { _isRunning :: Bool
    , _grid      :: Grid '[ Periodic 80, Periodic 25] V.Vector CellState
    } deriving (Eq,Show)
makeLenses ''RunningState

data AppState = Running RunningState
  deriving (Eq,Show)
makePrisms ''AppState

data AppEvent = Tick Double
  deriving (Eq,Show)
makePrisms ''AppEvent
