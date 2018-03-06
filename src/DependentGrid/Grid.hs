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

import           Control.Lens
import           Data.Constraint
import           Data.Constraint.Forall
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

newtype Grid (cs :: [Type]) f a = Grid {unGrid :: f a}
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance (GHC.KnownNat (ItemsRequired cs), Applicative f, ListLikeF f) =>
         Applicative (Grid cs f) where
    pure (a :: a) =
        withListLikeF (Proxy :: Proxy (f a)) $
        Grid $
        L.genericReplicate (GHC.natVal (Proxy :: Proxy (ItemsRequired cs))) a
    Grid ff <*> Grid fa = Grid (ff <*> fa)

splitToGroups ::
       (L.ListLike (f a) a, L.ListLike (f (f a)) (f a)) => Int -> f a -> f (f a)
splitToGroups n xs
    | L.length xs > n = L.take n xs `L.cons` splitToGroups n (L.drop n xs)
    | otherwise = L.singleton xs

addCoord ::
       forall cs f a.
       ( Functor f
       , SingI cs
       , AllAmountPossibleKnowNat cs
       , ListLikeF f
       , All IsCoord cs
       )
    => Grid cs f a
    -> Grid cs f (Coord cs, a)
addCoord (Grid grid) =
    case (sing :: Sing cs) of
        S.SNil -> (EmptyCoord, ) <$> Grid grid
        S.SCons (shead :: Sing n) (stail :: Sing ns) ->
            let nVal =
                    fromIntegral $
                    demote $ Monomorphic (sing :: Sing (AmountPossible n))
                a :: f (f (Coord ns, a)) =
                    (\g ->
                         unGrid $
                         withSingI stail $ addCoord (Grid g :: Grid ns f a)) <$>
                    (withListLikeF (Proxy :: Proxy (f (f a))) $
                     withListLikeF (Proxy :: Proxy (f a)) $
                     splitToGroups nVal grid)
                b :: f (f (Coord cs, a)) =
                    withListLikeF (Proxy :: Proxy (f (f (Coord ns, a)))) $
                    withListLikeF (Proxy :: Proxy (f (f (Coord cs, a)))) $
                    withListLikeF (Proxy :: Proxy (f n)) $
                    L.zipWith
                        (\(c :: n) (g :: f (Coord ns, a)) ->
                             (over _1 (AddCoord c)) <$> g)
                        (allPossible :: f n)
                        a
                c :: f (Coord cs, a) =
                    withListLikeF (Proxy :: Proxy (f (Coord cs, a))) $
                    withListLikeF (Proxy :: Proxy (f (f (Coord cs, a)))) $
                    L.concat b
            in Grid $ c
