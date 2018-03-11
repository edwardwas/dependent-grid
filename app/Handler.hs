{-# LANGUAGE RankNTypes #-}

module Handler where

import           Brick
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Maybe
import           Data.Semigroup
import qualified Graphics.Vty                         as V

newtype Handler n s e = Handler {unHandler :: e -> s -> MaybeT (EventM n) s}

runHandler ::
       Handler n s (BrickEvent n e) -> s -> BrickEvent n e -> EventM n (Next s)
runHandler (Handler func) s e =
    runMaybeT (func e s) >>= \x ->
        case x of
            Nothing -> halt s
            Just s  -> continue s

instance Semigroup (Handler n s e) where
  Handler a <> Handler b = Handler $ \e s -> a e s >>= b e

instance Monoid (Handler n s e) where
  mappend = (<>)
  mempty = Handler $ \_ -> pure

instance Contravariant (Handler n s) where
  contramap f (Handler h) = Handler $ \e -> h (f e)

instance Divisible (Handler n s) where
    divide f (Handler hb) (Handler hc) =
        Handler $ \a s ->
            let (b, c) = f a
            in hb b s >>= hc c
    conquer = Handler $ \_ -> pure

instance Decidable (Handler n s) where
    lose _ = conquer
    choose f (Handler hb) (Handler hc) =
        Handler $ \a s ->
            case f a of
                Left b  -> hb b s
                Right c -> hc c s

zoomHandler :: Traversal' a b -> Handler n b e -> Handler n a e
zoomHandler l (Handler f) = Handler $ \e -> traverseOf l (f e)
