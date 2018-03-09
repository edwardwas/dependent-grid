{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ParallelListComp       #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}

module DependentGrid.Coord where

import           DependentGrid.Class
import           DependentGrid.Coord.Periodic
import           DependentGrid.Orphans

import           Control.Lens                       hiding (from, to, (%~))
import           Control.Monad.Identity             (Identity (..))
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.Aeson.Types                   (Parser)
import           Data.AffineSpace
import           Data.Group
import           Data.Kind                          (Type)
import           Data.List
import           Data.List                          (intercalate)
import           Data.Promotion.Prelude.List        ((:!!))
import           Data.Semigroup                     (Semigroup (..))
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.Prelude.List       hiding (All, Group, Zip)
import qualified Data.Singletons.Prelude.List       as S
import           Data.Singletons.Prelude.Num
import           Data.Singletons.Prelude.Ord        (POrd (..))
import qualified Data.Singletons.TypeLits           as S
import qualified Data.Type.Natural                  as Nat
import           Data.Type.Natural.Class.Arithmetic
import           Data.Type.Ordinal
import qualified Data.Vector                        as V
import           Generics.SOP                       hiding (SCons, SNil,
                                                     Sing (..), SingI, sing)
import           GHC.Exts                           (Constraint)
import qualified GHC.TypeLits                       as GHC

type family IndexList n xs where
    IndexList Nat.Z (x ': _) = x
    IndexList Nat.Z '[] = GHC.TypeError (GHC.Text "This list is too short for use with IndexList")
    IndexList (Nat.S n) (_ ': xs) = IndexList n xs

certIxHelper :: Sing n -> Lens' (Coord xs) (IndexList n xs)
certIxHelper Zero f (AddCoord x xs)     = (\x' -> AddCoord x' xs) <$> f x
certIxHelper (Succ n) f (AddCoord x xs) = AddCoord x <$> certIxHelper n f xs

certIxCoord ::
       forall n xs. (IsTypeNum nat, SingI n)
    => Proxy (n :: nat)
    -> Lens' (Coord xs) (IndexList (AsPeano n) xs)
certIxCoord _ = certIxHelper (asPeano $ (sing :: Sing n))

data Coord (xs :: [Type]) :: Type where
  EmptyCoord :: Coord '[]
  AddCoord :: x -> Coord xs -> Coord (x ': xs)

instance (x ~ IndexList (AsPeano 0) xs) => Field1 (Coord xs) (Coord xs) x x where
    _1 = certIxCoord (Proxy @0)

instance (x ~ IndexList (AsPeano 1) xs) => Field2 (Coord xs) (Coord xs) x x where
    _2 = certIxCoord (Proxy @1)

instance (x ~ IndexList (AsPeano 2) xs) => Field3 (Coord xs) (Coord xs) x x where
    _3 = certIxCoord (Proxy @2)

instance (x ~ IndexList (AsPeano 3) xs) => Field4 (Coord xs) (Coord xs) x x where
    _4 = certIxCoord (Proxy @3)

instance All ToJSON cs => ToJSON (Coord cs) where
  toJSON gl =
    let helper :: All ToJSON xs => Coord xs -> [Value]
        helper EmptyCoord      = []
        helper (AddCoord c cs) = toJSON c : helper cs
    in Array $ V.fromList $ helper gl

instance (SingI cs, All FromJSON cs) => FromJSON (Coord cs) where
  parseJSON (Array a) = do
      let helper :: forall xs . (All FromJSON xs, SingI xs) => [Value] -> Parser (Coord xs)
          helper vs = case (sing :: Sing xs, vs) of
              (S.SNil, [])                    -> return EmptyCoord
              (S.SCons shead stail, (y : ys)) -> AddCoord <$> parseJSON y <*> withSingI stail (helper ys)
              _ -> fail "Incorrect number of elements"
      helper $ V.toList a
  parseJSON _ = fail "Not array"

instance All Eq xs => Eq (Coord xs) where
  EmptyCoord == EmptyCoord = True
  AddCoord a as == AddCoord b bs = a == b && as == bs

instance (All Eq cs, All Ord cs) => Ord (Coord cs) where
  compare EmptyCoord EmptyCoord           = EQ
  compare (AddCoord a as) (AddCoord b bs) = compare a b <> compare as bs

instance All Show xs => Show (Coord xs) where
    show c =
        let helper :: All Show ys => Coord ys -> [String]
            helper EmptyCoord      = []
            helper (AddCoord x xs) = show x : helper xs
        in "Coord (" ++ intercalate ", " (helper c) ++ ")"

instance All Semigroup xs => Semigroup (Coord xs) where
  EmptyCoord <> EmptyCoord = EmptyCoord
  AddCoord a as <> AddCoord b bs = AddCoord (a <> b) (as <> bs)

instance (SingI xs, All Monoid xs) => Monoid (Coord xs) where
    mappend EmptyCoord EmptyCoord = EmptyCoord
    mappend (AddCoord a as) (AddCoord b bs) =
        case (sing :: Sing xs) of
            (SCons _ stail) ->
                AddCoord (mappend a b) (withSingI stail $ mappend as bs)
    mempty =
        case (sing :: Sing xs) of
            SNil -> EmptyCoord
            (SCons _ xtail) ->
                AddCoord mempty $ withSingI xtail mempty

instance (All Monoid xs, All Group xs, SingI xs) => Group (Coord xs) where
    invert EmptyCoord = EmptyCoord
    invert (AddCoord a as) =
        case (sing :: Sing xs) of
            (SCons _ xtail) -> withSingI xtail $ AddCoord (invert a) (invert as)

instance (All AdditiveGroup xs, SingI xs) => AdditiveGroup (Coord xs) where
    zeroV =
        case (sing :: Sing xs) of
            SNil            -> EmptyCoord
            (SCons _ xtail) -> AddCoord zeroV $ withSingI xtail zeroV
    negateV EmptyCoord = EmptyCoord
    negateV (AddCoord a as) =
        case (sing :: Sing xs) of
            (SCons _ xtail) -> AddCoord (negateV a) $ withSingI xtail negateV as
    EmptyCoord ^+^ EmptyCoord = EmptyCoord
    AddCoord a as ^+^ AddCoord b bs =
        case (sing :: Sing xs) of
            (SCons _ xtail) -> AddCoord (a ^+^ b) $ withSingI xtail (as ^+^ bs)

type family DiffCoord (xs :: [k]) :: Type where
  DiffCoord '[] = ()
  DiffCoord '[a] = Identity (Diff a)
  DiffCoord '[a,b] = (Diff a, Diff b)
  DiffCoord '[a,b,c] = (Diff a, Diff b, Diff c)
  DiffCoord '[a,b,c,d] = (Diff a, Diff b, Diff c, Diff d)

type family MapDiff xs where
  MapDiff '[] = '[]
  MapDiff (x ': xs) = Diff x ': MapDiff xs

negHelper ::
       forall cs xs. All AffineSpace xs
    => Coord xs
    -> Coord xs
    -> NP I (MapDiff xs)
negHelper EmptyCoord EmptyCoord           = Nil
negHelper (AddCoord a as) (AddCoord b bs) = I (a .-. b) :* negHelper as bs

addHelper :: All AffineSpace xs => Coord xs -> NP I (MapDiff xs) -> Coord xs
addHelper EmptyCoord Nil              = EmptyCoord
addHelper (AddCoord a as) (I b :* bs) = AddCoord (a .+^ b) $ addHelper as bs

instance ( IsProductType (DiffCoord xs) (MapDiff xs)
         , AdditiveGroup (DiffCoord xs)
         , All AffineSpace xs
         ) =>
         AffineSpace (Coord xs) where
    type Diff (Coord xs) = DiffCoord xs
    a .-. b = to $ SOP $ Z $ negHelper a b
    a .+^ b =
        let helper :: IsProductType x codeX => x -> NP I codeX
            helper x =
                case from x of
                    SOP (Z n) -> n
        in addHelper a (helper b)

type family AllCoord c xs :: Constraint where
  AllCoord _ '[] = ()
  AllCoord c (x ': xs) = (c (Coord (x ': xs)), AllCoord c xs)

mooreNeighborhood ::
       (AllDiffEq cs i, All AffineSpace cs, Num i, Enum i)
    => i
    -> Coord cs
    -> [Coord cs]
mooreNeighborhood _ EmptyCoord = [EmptyCoord]
mooreNeighborhood i (AddCoord c cs) = do
    dcs <- mooreNeighborhood i cs
    dc <- [-i .. i]
    return $ AddCoord (c .+^ dc) dcs

type family AllDiffEq cs i :: Constraint where
  AllDiffEq '[] i = ()
  AllDiffEq (c ': cs) i = (Diff c ~ i, AllDiffEq cs i)

manhattenDistance ::
       (Num i, AllDiffEq cs i, All AffineSpace cs) => Coord cs -> Coord cs -> i
manhattenDistance EmptyCoord EmptyCoord = 0
manhattenDistance (AddCoord a as) (AddCoord b bs) =
    abs (a .-. b) + manhattenDistance as bs

vonNeumanNeighbourhood ::
       (AllDiffEq cs i, All AffineSpace cs, Num i, Enum i, Ord i)
    => i
    -> Coord cs
    -> [Coord cs]
vonNeumanNeighbourhood r c =
    filter (\c' -> manhattenDistance c c' <= r) $ mooreNeighborhood r c

coordAsList :: All IsCoord cs => Coord cs -> [Int]
coordAsList  EmptyCoord     = []
coordAsList (AddCoord a as) = coordAsInt a : coordAsList as
