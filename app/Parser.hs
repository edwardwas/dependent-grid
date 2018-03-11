{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Parser where

import           Types

import           DependentGrid.Class
import           DependentGrid.Coord
import           DependentGrid.Grid

import           Control.Lens
import           Control.Monad
import           Data.Singletons
import           Data.Singletons
import qualified Data.Text.IO        as T
import           Debug.Trace
import           Generics.SOP        hiding (SingI)
import qualified GHC.TypeLits        as GHC
import           Text.Parsec
import qualified Text.Parsec.Text    as T

decimal :: T.Parser Int
decimal = read <$> ((++) <$> option "" (string "-") <*> many1 digit)

parseLife106 :: T.Parser [(Int,Int)]
parseLife106 = do
  string "#Life 1.06"
  endOfLine
  many $ do
      x <- decimal
      spaces
      y <- decimal
      spaces
      return (x,y)

createPattern ::
       forall f a b xs.
       ( GridLike '[ a, b] f
       , FunctorWithIndex Int f
       )
    => [(Int, Int)]
    -> Maybe (Grid '[ a, b] f CellState)
createPattern xs = do
    minX <- minimumOf (traverse . _1) xs
    minY <- minimumOf (traverse . _2) xs
    let newXs = map (\a -> a & _1 -~ minX & _2 -~ minY) xs
        sizeX = fromIntegral $ GHC.natVal (Proxy :: Proxy (AmountPossible a))
        sizeY = fromIntegral $ GHC.natVal (Proxy :: Proxy (AmountPossible b))
    maxX <- maximumOf (traverse . _1) xs
    maxY <- maximumOf (traverse . _2) xs
    let f :: f (f CellState) =
            imap
                (\x ->
                     imap
                         (\y a ->
                              if (x, y) `elem` newXs
                                  then Alive
                                  else Dead)) $
            makeSized sizeX $ makeSized sizeY Dead
    rebuildGrid f

loadLifeFile ::
       (FunctorWithIndex Int f, GridLike '[ a, b] f, SingI a, SingI b)
    => FilePath
    -> IO (Maybe (Grid '[ a, b] f CellState))
loadLifeFile fp =
    let helper x =
            either (const Nothing) Just (parse parseLife106 "" x) >>=
            createPattern
    in helper <$> T.readFile fp
