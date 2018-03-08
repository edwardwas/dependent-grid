module Main where

import qualified Test.Coord.Periodic as P
import           Test.Util

import           Test.Tasty

main = defaultMain $ testGroup "Tests" [P.tests]
