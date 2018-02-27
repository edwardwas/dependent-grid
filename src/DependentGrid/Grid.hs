{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module DependentGrid.Grid where

import           Data.Functor.Classes
import           Data.Kind
import           Data.ListLike                 (ListLike)
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.Prelude.Ord   (Min)
import qualified Data.Sized                    as S
import           Data.Type.Monomorphic
import           Data.Type.Natural.Class.Order
import           Data.Type.Ordinal             (HasOrdinal)
import           Text.Show

data Grid (c :: nat -> Type) (is :: [nat]) (f :: Type -> Type) (a :: Type) where
  EmptyGrid :: a -> Grid c '[] f a
  GridLayer :: S.Sized f i (Grid c is f a)  -> Grid c (i ': is) f a

instance (S.ListLikeF f, Foldable f) => Eq1 (Grid cord is f) where
  liftEq f (EmptyGrid a) (EmptyGrid b) = f a b
  liftEq (f :: a -> b -> Bool) (GridLayer a) (GridLayer b) =
    S.withListLikeF' (S.unsized a) $
    S.withListLikeF' (S.unsized b) $
    S.withListLikeF (Proxy :: Proxy (f Bool)) $ and $ S.zipWith (liftEq f) a b

instance (S.ListLikeF f, Foldable f, Eq a) => Eq (Grid coord is f a) where
  (==) = liftEq (==)

instance (S.ListLikeF f) => Show1 (Grid coord is f) where
  liftShowsPrec sFunc mFunc n (EmptyGrid a) =
    showString "EmptyGrid " . sFunc n a
  liftShowsPrec sFunc mFunc n (GridLayer gl) = S.withListLikeF' (S.unsized gl) $
    showString "GridLayer " .
    showListWith (liftShowsPrec sFunc mFunc (n+1)) (S.toList gl)

instance (S.ListLikeF f, Show a) => Show (Grid coord is f a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Functor f => Functor (Grid c is f) where
  fmap f (EmptyGrid a) = EmptyGrid (f a)
  fmap f (GridLayer l) = GridLayer (fmap f <$> l)

instance (Functor f, Foldable f) => Foldable (Grid c is f) where
  foldMap f (EmptyGrid a) = f a
  foldMap f (GridLayer l) = foldMap id (foldMap f <$> l)

instance (Functor f, Traversable f) => Traversable (Grid c is f) where
  traverse f (EmptyGrid a) = EmptyGrid <$> f a
  traverse f (GridLayer l) = GridLayer <$> traverse (traverse f) l


instance (SingI is, S.ListLikeF f, HasOrdinal nat) =>
         Applicative (Grid c (is :: [nat]) f) where
  pure a =
    let pureHelper ::
             forall c f ns a. (S.ListLikeF f, SingI ns, HasOrdinal nat)
          => Sing (ns :: [nat])
          -> a
          -> Grid c ns f a
        pureHelper SNil a = EmptyGrid a
        pureHelper (SCons sHead (sTail :: Sing xs)) a =
          S.withListLikeF (Proxy :: Proxy (f (Grid c xs f a))) $
          GridLayer $
          S.replicate sHead $ withSingI sTail $ withSing $ \s -> pureHelper s a
    in withSing $ \s -> pureHelper s a
  (<*>) :: Grid c is f (a -> b) -> Grid c is f a -> Grid c is f b
  fl <*> al =
    let helper ::
             forall a b xs. SingI xs
          => Grid c xs f (a -> b)
          -> Grid c xs f a
          -> Grid c xs f b
        helper (EmptyGrid f) (EmptyGrid a) = EmptyGrid $ f a
        helper (GridLayer fl) (GridLayer al) =
          case (sing :: Sing xs) of
            SCons (sHead :: Sing y) (sTail :: Sing ys) ->
              S.withListLikeF (Proxy :: Proxy (f (Grid c ys f a))) $
              S.withListLikeF (Proxy :: Proxy (f (Grid c ys f (a -> b)))) $
              S.withListLikeF (Proxy :: Proxy (f (Grid c ys f b))) $
              withSingI sTail $ GridLayer $ S.zipWithSame (<*>) fl al
    in helper fl al
