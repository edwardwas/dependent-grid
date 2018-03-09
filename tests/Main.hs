module Main where

import qualified Test.Coord
import qualified Test.Coord.Periodic as P
import qualified Test.Grid
import           Test.Util

import           Test.Tasty

main = defaultMain $ testGroup "Tests" [P.tests, Test.Coord.tests, Test.Grid.tests]
