{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module DependentGrid.Grid where

import           DependentGrid.Class
import           DependentGrid.Coord
import           DependentGrid.Coord.Periodic

import           Control.Applicative
import           Control.Comonad
import           Control.Lens                 hiding (index)
import           Control.Monad.Zip
import           Data.AffineSpace
import           Data.Constraint
import           Data.Constraint.Forall
import           Data.Distributive
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Functor.Rep
import           Data.Kind                    (Type)
import           Data.List                    (sort)
import qualified Data.List.NonEmpty           as NE
import qualified Data.ListLike                as L
import           Data.Maybe                   (fromJust)
import           Data.Proxy                   (Proxy (..))
import           Data.Singletons
import qualified Data.Singletons.Prelude.List as S
import           Data.Type.Monomorphic
import qualified Data.Type.Natural            as Peano
import           Data.Type.Ordinal
import           Data.Unfoldable
import qualified Data.Vector                  as V
import           Debug.Trace
import           Generics.SOP                 (All)
import qualified GHC.TypeLits                 as GHC
import           Unsafe.Coerce                (unsafeCoerce)

class (L.ListLike (f a) a) => LLF f a
instance (L.ListLike (f a) a) => LLF f a

type ListLikeF f = Forall (LLF f)

withListLikeF ::
       forall proxy f a b. ListLikeF f
    => proxy (f a)
    -> (L.ListLike (f a) a =>
            b)
    -> b
withListLikeF _ b = b \\ entail
  where
    entail = inst :: ListLikeF f :- LLF f a

type family ItemsRequired xs where
  ItemsRequired '[] = 1
  ItemsRequired (x ': xs) = AmountPossible x GHC.* ItemsRequired xs

type family AllAmountPossibleKnowNat xs :: Constraint where
    AllAmountPossibleKnowNat '[] = ()
    AllAmountPossibleKnowNat (x ': xs) = ( GHC.KnownNat (AmountPossible x)
                                         , AllAmountPossibleKnowNat xs)

data Grid (cs :: [Type]) f a where
  EmptyGrid :: a -> Grid '[] f a
  AddGridLayer :: f (Grid cs f a) -> Grid (c ': cs) f a

addCoordToGrid ::
     (All IsCoord cs, MonadZip f, Unfoldable f)
  => Grid cs f a
  -> Grid cs f (Coord cs, a)
addCoordToGrid (EmptyGrid a) = EmptyGrid (EmptyCoord, a)
addCoordToGrid (AddGridLayer gl) =
  AddGridLayer $
  mzipWith (\c g -> over _1 (AddCoord c) <$> addCoordToGrid g) allPossible gl

instance (Foldable f, MonadZip f) => Eq1 (Grid cs f) where
  liftEq func (EmptyGrid a) (EmptyGrid b) = func a b
  liftEq func (AddGridLayer as) (AddGridLayer bs) =
    and $ mzipWith (liftEq func) as bs

instance (Foldable f, MonadZip f, Eq a) => Eq (Grid cs f a) where
  (==) = liftEq (==)

instance (Foldable f, MonadZip f) => Ord1 (Grid cs f) where
  liftCompare func (EmptyGrid a) (EmptyGrid b) = func a b
  liftCompare func (AddGridLayer as) (AddGridLayer bs) =
    foldMap id $ mzipWith (liftCompare func) as bs

instance (Foldable f, MonadZip f, Ord a) => Ord (Grid cs f a) where
  compare = liftCompare compare

instance (Functor f, Foldable f) => Show1 (Grid cs f) where
  liftShowsPrec sFunc mFunc n (EmptyGrid a) = sFunc n a
  liftShowsPrec sFunc mFunc n (AddGridLayer as) =
    showString "Grid {" .
    foldl
      (\f1 f2 -> f1 . showString "," . f2)
      id
      (fmap (liftShowsPrec sFunc mFunc (n + 1)) as) .
    showString "}"

instance (Functor f, Foldable f, Show a) => Show (Grid cs f a) where
  show x = liftShowsPrec showsPrec (foldl (.) id . fmap (showsPrec 0)) 0 x ""

instance Functor f => Functor (Grid cs f) where
  fmap f (EmptyGrid x)     = EmptyGrid (f x)
  fmap f (AddGridLayer gs) = AddGridLayer (fmap f <$> gs)

instance (MonadZip f, Functor f, All IsCoord cs, Unfoldable f) =>
         FunctorWithIndex (Coord cs) (Grid cs f) where
  imap f (EmptyGrid x) = EmptyGrid $ f EmptyCoord x
  imap f (AddGridLayer gs) =
    AddGridLayer $
    mzipWith
      (\outer g -> imap (\inner -> f (AddCoord outer inner)) g)
      allPossible
      gs

instance Foldable f => Foldable (Grid cs f) where
  foldMap f (EmptyGrid x)     = f x
  foldMap f (AddGridLayer gs) = foldMap (foldMap f) gs

instance (Unfoldable f, MonadZip f, Foldable f, All IsCoord cs) =>
         FoldableWithIndex (Coord cs) (Grid cs f) where
  ifoldMap f (EmptyGrid x) = f EmptyCoord x
  ifoldMap f (AddGridLayer gs) =
    foldMap id $
    mzipWith
      (\outer -> ifoldMap (\inner -> f (AddCoord outer inner)))
      allPossible
      gs

instance Traversable f => Traversable (Grid cs f) where
  traverse f (EmptyGrid x)     = EmptyGrid <$> f x
  traverse f (AddGridLayer gs) = AddGridLayer <$> traverse (traverse f) gs

instance (All IsCoord cs, Traversable f, MonadZip f, Unfoldable f) =>
         TraversableWithIndex (Coord cs) (Grid cs f) where
  itraverse f (EmptyGrid x) = EmptyGrid <$> f EmptyCoord x
  itraverse f (AddGridLayer gs) =
    AddGridLayer <$>
    sequenceA
      (mzipWith
         (\outer -> itraverse (\inner -> f (AddCoord outer inner)))
         allPossible
         gs)

instance (AllAmountPossibleKnowNat cs, SingI cs, Applicative f, MonadZip f, Unfoldable f) =>
         Applicative (Grid cs f) where
  pure a =
    case (sing :: Sing cs) of
      S.SNil -> EmptyGrid a
      S.SCons (shead :: Sing n) (stail :: Sing ns) ->
        AddGridLayer $
        fromJust $ makeSized
          (fromIntegral $ GHC.natVal (Proxy :: Proxy (AmountPossible n))) $
        withSingI stail $ pure a
  EmptyGrid f <*> EmptyGrid a = EmptyGrid (f a)
  AddGridLayer fs <*> AddGridLayer as = case (sing :: Sing cs) of
      S.SCons _ stail -> withSingI stail $ AddGridLayer (mzipWith (<*>) fs as)

instance (All IsCoord cs, SingI cs, Unfoldable f, Foldable f, Functor f) =>
         Distributive (Grid cs f) where
  distribute = distributeRep

instance (All IsCoord cs, SingI cs, Functor f, Unfoldable f, Foldable f) =>
         Representable (Grid cs f) where
  type Rep (Grid cs f) = Coord cs
  tabulate f =
    case (sing :: Sing cs) of
      S.SNil -> EmptyGrid $ f EmptyCoord
      S.SCons shead stail ->
        withSingI stail $
        AddGridLayer $
        (\c -> tabulate (\c' -> f (AddCoord c c'))) <$> allPossible
  index (EmptyGrid a) EmptyCoord = a
  index (AddGridLayer gl) (AddCoord c cs) =
    case (sing :: Sing cs) of
      S.SCons _ stail ->
        withSingI stail $ index (head $ drop (coordAsInt c) $ toList gl) cs

instance ( AllAmountPossibleKnowNat cs
         , Comonad f
         , Traversable f
         , SingI cs
         , MonadZip f
         , Unfoldable f
         ) =>
         Comonad (Grid cs f) where
  extract (EmptyGrid a) = a
  extract (AddGridLayer gl) =
    case (sing :: Sing cs) of
      S.SCons _ stail -> withSingI stail $ extract $ extract gl
  duplicate (EmptyGrid a) = EmptyGrid (EmptyGrid a)
  duplicate (AddGridLayer gl) =
    case (sing :: Sing cs) of
      S.SCons shead stail ->
        withSingI stail $
        AddGridLayer <$>
        AddGridLayer (sequenceA <$> duplicate (duplicate <$> gl))

overGridLayer ::
     forall n cs f a. (SingI (AsPeano n), Functor f, IsTypeNum nat)
  => Proxy (n :: nat)
  -> (forall x. f x -> f x)
  -> Grid cs f a
  -> Grid cs f a
overGridLayer _ f gs =
  case (sing :: Sing (AsPeano n), gs) of
    (Peano.SZ, AddGridLayer as) -> AddGridLayer (f as)
    (Peano.SS (n :: Sing m), AddGridLayer as) ->
      withSingI n $ AddGridLayer (overGridLayer (Proxy :: Proxy m) f <$> as)

overAllLayers :: Traversable f => (forall x . f x -> f x) -> Grid cs f a -> [ Grid cs f a ]
overAllLayers _ (EmptyGrid a)     = pure (EmptyGrid a)
overAllLayers f (AddGridLayer gl) =
  map
    AddGridLayer
    (traverse (overAllLayers id) (f gl) ++ traverse (overAllLayers f) gl)

