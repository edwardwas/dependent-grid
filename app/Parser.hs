{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Parser where

import           Types

import           DependentGrid.Class
import           DependentGrid.Coord
import           DependentGrid.Grid

import           Control.Lens
import           Control.Monad
import           Data.Singletons
import qualified GHC.TypeLits        as GHC
import           Text.Parsec
import qualified Text.Parsec.Text    as T

decimal :: T.Parser Int
decimal = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

parseLife106 :: T.Parser [(Int,Int)]
parseLife106 = do
  string "#Life 1.06"
  newline
  ((,) <$> decimal <*> (string " " *> decimal)) `sepBy1` newline

createPattern ::
       forall f a b. GridLike '[ a, b] f
    => [(Int, Int)]
    -> Maybe (Grid '[ a, b] f CellState)
createPattern xs = do
  minX <- minimumOf (traverse . _1) xs
  minY <- minimumOf (traverse . _2) xs
  let newXs = map (\a -> a & _1 +~ minX & _2 +~ minY) xs
  maxX <- maximumOf (traverse . _1) xs
  maxY <- maximumOf (traverse . _2) xs
  guard $ maxX <= fromIntegral (GHC.natVal (Proxy :: Proxy (AmountPossible a)))
  guard $ maxY <= fromIntegral (GHC.natVal (Proxy :: Proxy (AmountPossible b)))
  return $ pure Dead
