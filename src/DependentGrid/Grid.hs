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
import           Control.Lens
import           Control.Monad.Zip
import           Data.Constraint
import           Data.Constraint.Forall
import           Data.Functor.Classes
import           Data.Kind                    (Type)
import qualified Data.ListLike                as L
import           Data.Proxy                   (Proxy (..))
import           Data.Singletons
import qualified Data.Singletons.Prelude.List as S
import           Data.Type.Monomorphic
import           Data.Type.Ordinal
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

instance (Foldable f, MonadZip f) => Eq1 (Grid cs f) where
  liftEq func (EmptyGrid a) (EmptyGrid b) = func a b
  liftEq func (AddGridLayer as) (AddGridLayer bs) =
    and $ mzipWith (liftEq func) as bs

instance (Foldable f, MonadZip f, Eq a) => Eq (Grid cs f a) where
  (==) = liftEq (==)

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

instance Foldable f => Foldable (Grid cs f) where
  foldMap f (EmptyGrid x)     = f x
  foldMap f (AddGridLayer gs) = foldMap (foldMap f) gs

instance Traversable f => Traversable (Grid cs f) where
  traverse f (EmptyGrid x)     = EmptyGrid <$> f x
  traverse f (AddGridLayer gs) = AddGridLayer <$> traverse (traverse f) gs

instance (AllAmountPossibleKnowNat cs, SingI cs, Applicative f, GenerateSized f, MonadZip f) =>
         Applicative (Grid cs f) where
  pure a =
    case (sing :: Sing cs) of
      S.SNil -> EmptyGrid a
      S.SCons (shead :: Sing n) (stail :: Sing ns) ->
        AddGridLayer $
        makeSized
          (fromIntegral $ GHC.natVal (Proxy :: Proxy (AmountPossible n))) $
        withSingI stail $ pure a
  EmptyGrid f <*> EmptyGrid a = EmptyGrid (f a)
  AddGridLayer fs <*> AddGridLayer as = case (sing :: Sing cs) of
      S.SCons _ stail -> withSingI stail $ AddGridLayer (mzipWith (<*>) fs as)
