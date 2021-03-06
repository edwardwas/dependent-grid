{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           Control.Comonad.Hoist.Class
import           Control.Comonad.Representable.Store
import           Control.Comonad.Trans.Class
import           Control.Lens                        hiding (index)
import           Control.Monad
import           Control.Monad.Zip
import           Data.AffineSpace
import           Data.Distributive
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Functor.Rep
import           Data.Kind                           (Type)
import           Data.List                           (sort)
import qualified Data.List.NonEmpty                  as NE
import           Data.Maybe                          (fromJust)
import           Data.Proxy                          (Proxy (..))
import           Data.Singletons
import qualified Data.Singletons.Prelude.List        as S
import qualified Data.Singletons.Prelude.Num         as S
import           Data.Type.Monomorphic
import qualified Data.Type.Natural                   as Peano
import           Data.Type.Ordinal
import qualified Data.Vector                         as V
import           Debug.Trace
import           Generics.SOP                        (All)
import           GHC.Exts                            (Constraint)
import qualified GHC.TypeLits                        as GHC
import           Unsafe.Coerce                       (unsafeCoerce)

data Grid (cs :: [Type]) f a where
    EmptyGrid :: a -> Grid '[] f a
    AddGridLayer :: f (Grid cs f a) -> Grid (c ': cs) f a

type GridLike cs f
     = ( MakeSized f
       , MonadZip f
       , All IsCoord cs
       , SingI cs
       , GetByIndex f Int
       , Traversable f)

addCoordToGrid ::
       forall cs f a. GridLike cs f
    => Grid cs f a
    -> Grid cs f (Coord cs, a)
addCoordToGrid (EmptyGrid a) = EmptyGrid (EmptyCoord, a)
addCoordToGrid (AddGridLayer gl) =
    case (sing :: Sing cs) of
        S.SCons _ stail ->
            withSingI stail $
            AddGridLayer $
            mzipWith
                (\c g -> over _1 (AddCoord c) <$> addCoordToGrid g)
                allPossible
                gl

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

instance (MonadZip f, Functor f, All IsCoord cs, MakeSized f) =>
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

instance (MonadZip f, Foldable f, All IsCoord cs, MakeSized f) =>
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

instance (All IsCoord cs, Traversable f, MonadZip f, MakeSized f) =>
         TraversableWithIndex (Coord cs) (Grid cs f) where
    itraverse f (EmptyGrid x) = EmptyGrid <$> f EmptyCoord x
    itraverse f (AddGridLayer gs) =
        AddGridLayer <$>
        sequenceA
            (mzipWith
                 (\outer -> itraverse (\inner -> f (AddCoord outer inner)))
                 allPossible
                 gs)

instance GridLike cs f =>
         Applicative (Grid cs f) where
    pure a =
        case (sing :: Sing cs) of
            S.SNil -> EmptyGrid a
            S.SCons (shead :: Sing n) (stail :: Sing ns) ->
                AddGridLayer $
                makeSized
                    (fromIntegral $
                     GHC.natVal (Proxy :: Proxy (AmountPossible n))) $
                withSingI stail $ pure a
    EmptyGrid f <*> EmptyGrid a = EmptyGrid (f a)
    AddGridLayer fs <*> AddGridLayer as =
        case (sing :: Sing cs) of
            S.SCons _ stail ->
                withSingI stail $ AddGridLayer (mzipWith (<*>) fs as)

instance (All IsCoord cs, SingI cs, Foldable f, Functor f, MakeSized f) =>
         Distributive (Grid cs f) where
    distribute = distributeRep

instance (All IsCoord cs, SingI cs, Functor f, Foldable f, MakeSized f) =>
         Representable (Grid cs f) where
    type Rep (Grid cs f) = Coord cs
    tabulate f =
        case (sing :: Sing cs) of
            S.SNil -> EmptyGrid $ f EmptyCoord
            S.SCons shead stail ->
                withSingI stail $ AddGridLayer $
                (\c -> tabulate (\c' -> f (AddCoord c c'))) <$>
                allPossible
    index (EmptyGrid a) EmptyCoord = a
    index (AddGridLayer gl) (AddCoord c cs) =
        case (sing :: Sing cs) of
            S.SCons _ stail ->
                withSingI stail $
                index (head $ drop (coordAsInt c) $ toList gl) cs

type family AllGridLayersIndex cs f a i where
    AllGridLayersIndex '[] f a i = Index (f a) ~ i
    AllGridLayersIndex (c ': cs) f a i = ( Index (f (Grid cs f a)) ~ i
                                         , IxValue (f (Grid cs f a)) ~ Grid cs f a
                                         , Ixed (f (Grid cs f a))
                                         , AllGridLayersIndex cs f a i)

gridIndexed ::
       (All IsCoord cs, GetByIndex f Int) => Coord cs -> Lens' (Grid cs f a) a
gridIndexed EmptyCoord f (EmptyGrid a) = EmptyGrid <$> f a
gridIndexed (AddCoord c cs) f (AddGridLayer gl) =
    AddGridLayer <$>
    singular (getByIndex (coordAsInt c)) (\g -> gridIndexed cs f g) gl

data FocusedGrid cs f a = FocusedGrid
    { _focusedGrid  :: Grid cs f a
    , _focusedCoord :: Coord cs
    } deriving (Eq, Show, Functor, Foldable, Traversable)

makeLenses ''FocusedGrid

instance (GridLike cs f) =>
         Comonad (FocusedGrid cs f) where
    extract fg = fg ^. focusedGrid . gridIndexed (fg ^. focusedCoord)
    duplicate fg =
        over
            focusedGrid
            (imap (\pos a -> FocusedGrid (fg ^. focusedGrid) pos))
            fg

instance GridLike cs f =>
         ComonadStore (Coord cs) (FocusedGrid cs f) where
    pos = view focusedCoord
    peek pos fg = fg ^. focusedGrid . gridIndexed pos

makeFocusGrid :: (All Monoid cs, SingI cs) => Grid cs f a -> FocusedGrid cs f a
makeFocusGrid g = FocusedGrid g mempty

type family CollapsedGrid cs f a where
    CollapsedGrid '[] _ a = a
    CollapsedGrid (c ': cs) f a = f (CollapsedGrid cs f a)

collapseGrid :: Functor f => Grid cs f a -> CollapsedGrid cs f a
collapseGrid (EmptyGrid a)     = a
collapseGrid (AddGridLayer gs) = collapseGrid <$> gs

rebuildGrid ::
       forall cs f a. (Traversable f, SingI cs, MakeSized f, All IsCoord cs)
    => CollapsedGrid cs f a
    -> Maybe (Grid cs f a)
rebuildGrid a =
    case (sing :: Sing cs) of
        S.SNil -> Just (EmptyGrid a)
        S.SCons (_ :: Sing n) stail -> do
            a' <-
                takeSized
                    (fromIntegral $
                     GHC.natVal (Proxy :: Proxy (AmountPossible n)))
                    a
            withSingI stail (AddGridLayer <$> traverse rebuildGrid a')

gridSized :: GridLike cs f => Prism' (CollapsedGrid cs f a) (Grid cs f a)
gridSized = prism' collapseGrid rebuildGrid

type family SameShape f g :: Constraint where
  SameShape (_ ': as) (_ ': bs) = SameShape as bs
  SameShape '[] '[] = ()

padGridBehind ::
       forall newCs cs f a.
       (SameShape newCs cs, Alternative f, GridLike newCs f, GridLike cs f)
    => a
    -> Grid cs f a
    -> Grid newCs f a
padGridBehind d (EmptyGrid a) =
    case (sing :: Sing cs, sing :: Sing newCs) of
        (S.SNil, S.SNil) -> (EmptyGrid a)
        _                -> error "Pad grid unaccessbile"
padGridBehind d (AddGridLayer a) =
    case (sing :: Sing cs, sing :: Sing newCs) of
        (S.SCons (chead :: Sing c) ctail, S.SCons (nhead :: Sing n) ntail) ->
            let oldLength = GHC.natVal (Proxy :: Proxy (AmountPossible c))
                newLength = GHC.natVal (Proxy :: Proxy (AmountPossible n))
            in withSingI ctail $
               withSingI ntail $
               AddGridLayer
                   (fmap (padGridBehind d) a <|>
                    makeSized (fromIntegral $ newLength - oldLength) (pure d))

padGridInFront ::
       forall newCs cs f a.
       (SameShape newCs cs, Alternative f, GridLike newCs f, GridLike cs f)
    => a
    -> Grid cs f a
    -> Grid newCs f a
padGridInFront d (EmptyGrid a) =
    case (sing :: Sing cs, sing :: Sing newCs) of
        (S.SNil, S.SNil) -> (EmptyGrid a)
        _                -> error "Pad grid unaccessbile"
padGridInFront d (AddGridLayer a) =
    case (sing :: Sing cs, sing :: Sing newCs) of
        (S.SCons (chead :: Sing c) ctail, S.SCons (nhead :: Sing n) ntail) ->
            let oldLength = GHC.natVal (Proxy :: Proxy (AmountPossible c))
                newLength = GHC.natVal (Proxy :: Proxy (AmountPossible n))
            in withSingI ctail $
               withSingI ntail $
               AddGridLayer
                   (makeSized (fromIntegral $ newLength - oldLength) (pure d) <|>
                    fmap (padGridInFront d) a)

type family ZipCoordHelper cs ns where
    ZipCoordHelper '[] '[] = '[]
    ZipCoordHelper (c ': cs) (n ': ns) = ModifyAmountPossible c (Apply (S.:+$) n) ': ZipCoordHelper cs ns


padGrid ::
       forall a ns cs f newCs.
       ( SameShape newCs cs
       , SameShape newCs (ZipCoordHelper cs ns)
       , SameShape (ZipCoordHelper cs ns) cs
       , GridLike cs f
       , GridLike newCs f
       , GridLike (ZipCoordHelper cs ns) f
       , Alternative f
       )
    => a
    -> Proxy (ns :: [GHC.Nat])
    -> Grid cs f a
    -> Grid newCs f a
padGrid a _ g =
    padGridBehind
        a
        (padGridInFront a g :: Grid (ZipCoordHelper cs ns) f a)
