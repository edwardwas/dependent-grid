module Test.Util where

import           Hedgehog            hiding (Group)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           Control.Monad
import           Data.Group
import           Data.Semigroup
import           Test.Tasty
import           Test.Tasty.Hedgehog

semigroupTest :: (Show a, Eq a, Semigroup a) => Gen a -> TestTree
semigroupTest genItem =
    let associativity =
            property $ do
                a <- forAll $ genItem
                b <- forAll $ genItem
                c <- forAll $ genItem
                (a <> b) <> c === a <> (b <> c)
    in testGroup "Semigroup Laws" [testProperty "Associatvety" associativity]

monoidTest :: (Show a, Eq a, Monoid a) => Gen a -> TestTree
monoidTest genItem =
    let apLeftEmpty = property $ forAll genItem >>= \x -> x === mappend mempty x
        apRightEmpty = property $ forAll genItem >>= \x -> x === mappend x mempty
        assoc = property $ do
            a <- forAll genItem
            b <- forAll genItem
            c <- forAll genItem
            mappend a (mappend b c) === mappend (mappend a b) c
        foldLaw = property $ do
            n <- forAll $ Gen.integral $ Range.linear 1 10
            as <- replicateM n $ forAll genItem
            mconcat as === foldr mappend mempty as
    in testGroup
           "Monoid Laws"
           [ testProperty "Left Append Empty" apLeftEmpty
           , testProperty "Right Append Empty" apRightEmpty
           , testProperty "Assoc" assoc
           , testProperty "Fold Law" foldLaw
           ]

groupTest :: (Show a, Eq a, Group a) => Gen a -> TestTree
groupTest genItem =
    testGroup
        "Group Laws"
        [ testProperty "InvertLeft" $
          property $ forAll genItem >>= \x -> mappend x (invert x) === mempty
        , testProperty "InvertRight" $
          property $ forAll genItem >>= \x -> mappend (invert x) x === mempty
        ]
