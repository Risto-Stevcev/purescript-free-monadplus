module Control.MonadPlus.Free where

import Control.Alt (class Alt, (<|>), alt)
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus, empty)
import Data.Foldable (class Foldable, foldr, foldMap, foldlDefault, foldrDefault)
import Data.List (List(..), (:))
import Data.Monoid (class Monoid)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Prelude (type (~>), (==), (<$>), ($), (<>), (>>=), (<<<), class Eq, class Ord, class Functor, class Apply, class Applicative, class Bind, class Monad, class Semigroup, Ordering(..), compare, map, apply, bind, pure, id)

data FreeMonadPlus f a
  = Pure a
  | Bind (f (FreeMonadPlus f a))
  | Plus (List (FreeMonadPlus f a))

instance eqFreeMonadPlus ∷ (Eq (f (FreeMonadPlus f a)), Eq a) => Eq (FreeMonadPlus f a) where
  eq (Pure a) (Pure b) = a == b
  eq (Bind fa) (Bind fb) = fa == fb
  eq (Plus as) (Plus bs) = as == bs
  eq _ _ = false

instance ordFreeMonadPlus ∷ (Ord (f (FreeMonadPlus f a)), Ord a) => Ord (FreeMonadPlus f a) where
  compare (Pure a)  (Pure b) = a `compare` b
  compare (Pure _)  (Bind _) = LT
  compare (Pure _)  (Plus _) = LT
  compare (Bind _)  (Pure _) = GT
  compare (Bind fa) (Bind fb) = fa `compare` fb
  compare (Bind _)  (Plus _) = LT
  compare (Plus _)  (Pure _) = GT
  compare (Plus _)  (Bind _) = GT
  compare (Plus as) (Plus bs) = as `compare` bs

instance functorFreeMonadPlus ∷ Functor f => Functor (FreeMonadPlus f) where
  map f = go where
    go (Pure a)  = Pure (f a)
    go (Bind fa) = Bind (go <$> fa)
    go (Plus as) = Plus (map go as)

instance applyFreeMonadPlus ∷ Functor f => Apply (FreeMonadPlus f) where
  apply (Pure f)  (Pure b) = Pure (f b)
  apply (Pure f)  (Plus bs) = Plus $ map f <$> bs
  apply (Pure f)  (Bind fb) = Bind $ map f <$> fb
  apply (Bind ff) b = Bind $ (\a → apply a b) <$> ff
  apply (Plus fs) b = Plus $ (\a → apply a b) <$> fs

instance applicativeFreeMonadPlus ∷ Functor f => Applicative (FreeMonadPlus f) where
  pure = Pure

instance bindFreeMonadPlus ∷ Functor f => Bind (FreeMonadPlus f) where
  bind (Pure a) f = f a
  bind (Bind m) f = Bind ((\a → bind a f) <$> m)
  bind (Plus m) f = Plus ((\a → bind a f) <$> m)

instance monadFreeMonadPlus ∷ Functor f => Monad (FreeMonadPlus f)

instance altFreeMonadPlus ∷ Functor f => Alt (FreeMonadPlus f) where
  alt (Plus Nil) r = r
  alt l (Plus Nil) = l
  alt (Plus as) (Plus bs) = Plus (as <> bs)
  alt a b = Plus (a : b : Nil)

instance plusFreeMonadPlus ∷ Functor f => Plus (FreeMonadPlus f) where
  empty = Plus Nil

instance alternativeFreeMonadPlus ∷ Functor f => Alternative (FreeMonadPlus f)

instance monadZeroFreeMonadPlus ∷ Functor f => MonadZero (FreeMonadPlus f)

instance monadPlusFreeMonadPlus ∷ Functor f => MonadPlus (FreeMonadPlus f)

instance semigroupFreeMonadPlus ∷ Functor f => Semigroup (FreeMonadPlus f a) where
  append = alt

instance monoidFreeMonadPlus ∷ Functor f => Monoid (FreeMonadPlus f a) where
  mempty = empty

instance foldableFreeMonadPlus ∷ Foldable f => Foldable (FreeMonadPlus f) where
  foldr = foldrDefault
  foldl = foldlDefault
  foldMap f = go where
    go (Pure a) = f a
    go (Bind fa) = foldMap go fa
    go (Plus as) = foldMap go as

instance traversableFreeMonadPlus ∷ Traversable f => Traversable (FreeMonadPlus f) where
  sequence = sequenceDefault
  traverse f = go where
    go (Pure a) = Pure <$> f a
    go (Bind fa) = Bind <$> traverse go fa
    go (Plus as) = Plus <$> traverse go as

iter ∷ ∀ f a. Functor f => (f a → a) → (List a → a) → FreeMonadPlus f a → a
iter phi psi = go where
  go (Pure a) = a
  go (Bind as) = phi (go <$> as)
  go (Plus as) = psi (go <$> as)

iterM ∷ ∀ f m a. Monad m => Functor f => (f (m a) → m a) → (List (m a) → m a) → FreeMonadPlus f a → m a
iterM phi psi = go where
  go (Pure a) = pure a
  go (Bind as) = phi (go <$> as)
  go (Plus as) = psi (go <$> as)

hoistFree ∷ ∀ f g. Functor g => (f ~> g) → FreeMonadPlus f ~> FreeMonadPlus g
hoistFree f = go where
  go (Pure a)  = Pure a
  go (Bind as) = Bind (go <$> f as)
  go (Plus as) = Plus (map go as)

liftFree ∷ ∀ f. Functor f => f ~> FreeMonadPlus f
liftFree = Bind <<< map Pure

lowerFree ∷ ∀ f. MonadPlus f => FreeMonadPlus f ~> f
lowerFree = foldFree id

foldFree ∷ ∀ f m. MonadPlus m => Functor f => (f ~> m) -> FreeMonadPlus f ~> m
foldFree f (Pure x)  = pure x
foldFree f (Bind x)  = f x >>= foldFree f
foldFree f (Plus xs) = foldr (\elem acc → foldFree f elem <|> acc) empty xs
