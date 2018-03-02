{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module DependentGrid.Coord where

import           DependentGrid.Coord.Periodic
import           DependentGrid.Orphans

import           Control.Monad.Identity
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Coerce
import           Data.Constraint
import           Data.Group
import           Data.Kind                          (Type)
import           Data.List                          (intercalate)
import           Data.Semigroup                     (Semigroup (..))
import           Data.Singletons
import           Data.Singletons.Prelude.List       (Sing (..))
import           Data.Singletons.Prelude.Num
import qualified Data.Sized                         as S
import           Data.Type.Natural.Class.Arithmetic
import           Generics.SOP                       hiding (SCons, SNil, Sing,
                                                     SingI, sing)

import qualified GHC.TypeLits                       as GHC

type family AllConstraint c f xs :: Constraint where
    AllConstraint c f '[] = ()
    AllConstraint c f (x ': xs) = (c (f x), AllConstraint c f xs)

data Coord (c :: nat -> Type) (xs :: [nat]) where
    NoCoord :: Coord c '[]
    AddCoord :: c x -> Coord c xs -> Coord c (x : xs)

instance AllConstraint Eq c xs => Eq (Coord c xs) where
    NoCoord == NoCoord = True
    AddCoord (a :: c x) as == AddCoord b bs = (a == b && as == bs)

instance (SingI xs, AllConstraint Show c xs) => Show (Coord c xs) where
    show coord =
        let helper ::
                   forall c xs. (SingI xs, AllConstraint Show c xs)
                => Coord c xs
                -> [String]
            helper NoCoord = []
            helper (AddCoord a as) =
                case (sing :: Sing xs) of
                    SCons (_ :: Sing y) sTail ->
                        show a : withSingI sTail (helper as)
        in "Coord (" ++ intercalate ", " (helper coord) ++ ")"

instance AllConstraint Semigroup c xs => Semigroup (Coord c xs) where
    NoCoord <> NoCoord = NoCoord
    AddCoord a as <> AddCoord b bs = AddCoord (a <> b) (as <> bs)

instance (SingI xs, AllConstraint Monoid c xs, AllConstraint Semigroup c xs) =>
         Monoid (Coord c xs) where
    mappend = (<>)
    mempty =
        case (sing :: Sing xs) of
            SNil          -> NoCoord
            SCons _ stail -> AddCoord mempty $ withSingI stail mempty

instance ( AllConstraint Monoid c xs
         , AllConstraint Semigroup c xs
         , AllConstraint Group c xs
         , SingI xs
         ) =>
         Group (Coord c xs) where
    invert NoCoord         = NoCoord
    invert (AddCoord a as) = AddCoord (invert a) as

instance (SingI xs, AllConstraint AdditiveGroup c xs) =>
         AdditiveGroup (Coord c xs) where
    zeroV =
        case (sing :: Sing xs) of
            SNil          -> NoCoord
            SCons _ stail -> AddCoord zeroV $ withSingI stail zeroV
    a ^+^ b =
        let helper ::
                   AllConstraint AdditiveGroup c ys
                => Coord c ys
                -> Coord c ys
                -> Coord c ys
            helper NoCoord NoCoord = NoCoord
            helper (AddCoord a as) (AddCoord b bs) =
                AddCoord (a ^+^ b) (helper as bs)
        in helper a b
    negateV c =
        let helper ::
                   AllConstraint AdditiveGroup c ys => Coord c ys -> Coord c ys
            helper NoCoord         = NoCoord
            helper (AddCoord a as) = AddCoord (negateV a) (helper as)
        in helper c

type family DiffCoord (c :: k -> Type) (xs :: [k]) :: Type where
  DiffCoord c '[] = ()
  DiffCoord c '[a] = Identity (Diff (c a))
  DiffCoord c '[a,b] = (Diff (c a), Diff (c b))
  DiffCoord c '[a,b,d] = (Diff (c a), Diff (c b), Diff (c d))

type family Head xs where
  Head (x ': xs) = x

type family MapDiff c xs where
  MapDiff _ '[] = '[]
  MapDiff c (x ': xs) = Diff (c x) ': MapDiff c xs

newtype DiffC c x = DiffC { unDiffC ::  Diff (c x) }

deriving instance Show (Diff (c x)) => Show (DiffC c x)

negHelper :: AllConstraint AffineSpace c xs => Coord c xs -> Coord c xs -> NP (DiffC c) xs
negHelper NoCoord NoCoord = Nil
negHelper (AddCoord a as) (AddCoord b bs) = DiffC (a .-. b)  :* negHelper as bs

diffCoordToTuple ::
       ( Code (DiffCoord c xs) ~ '[ MapDiff c xs]
       , Generic (DiffCoord c xs)
       , SameShapeAs xs (MapDiff c xs)
       , SameShapeAs (MapDiff c xs) xs
       , AllZipN NP (LiftedCoercible (DiffC c) I) xs (MapDiff c xs)
       )
    => NP (DiffC c) xs
    -> DiffCoord c xs
diffCoordToTuple = to . SOP . Z . htoI

tupleToDiffCoord ::
       ( IsProductType (DiffCoord c xs) (MapDiff c xs)
       , SameShapeAs xs (MapDiff c xs)
       , SameShapeAs (MapDiff c xs) xs
       , AllZipN NP (LiftedCoercible I (DiffC c)) (MapDiff c xs) xs
       )
    => DiffCoord c xs
    -> NP (DiffC c) xs
tupleToDiffCoord x =
    case from x of
        SOP (Z n) -> hfromI n

addHelper :: AllConstraint AffineSpace c xs => Coord c xs -> NP (DiffC c) xs -> Coord c xs
addHelper NoCoord Nil = NoCoord
addHelper (AddCoord a as) (DiffC b :* bs) = AddCoord (a .+^ b) $ addHelper as bs

instance ( AdditiveGroup (DiffCoord c xs)
         , Code (DiffCoord c xs) ~ '[ MapDiff c xs]
         , Generic (DiffCoord c xs)
         , SameShapeAs xs (MapDiff c xs)
         , SameShapeAs (MapDiff c xs) xs
         , AllZipN NP (LiftedCoercible (DiffC c) I) xs (MapDiff c xs)
         , AllZipN NP (LiftedCoercible I (DiffC c))  (MapDiff c xs) xs
         , AllConstraint AffineSpace c xs
         ) =>
         AffineSpace (Coord c xs) where
    type Diff (Coord c xs) = DiffCoord c xs
    a .-. b =
        diffCoordToTuple $ negHelper a b
    a .+^ b = addHelper a $ tupleToDiffCoord b
