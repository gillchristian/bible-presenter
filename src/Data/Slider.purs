module Data.Slider where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.FoldableWithIndex as FoldableWithIndex
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.ZipperArray (ZipperArray)
import Data.ZipperArray as ZipperArray

data Slider a
  = Active (ZipperArray a)
  | Paused (ZipperArray a)
  | Inactive (Array a)

--------------------------------------------------------------------------------
-- Mutation --------------------------------------------------------------------
--------------------------------------------------------------------------------

cons :: forall a. a -> Slider a -> Slider a
cons a (Active as) = Active $ ZipperArray.cons a as
cons a (Paused as) = Paused $ ZipperArray.cons a as
cons a (Inactive as) = Inactive $ Array.cons a as

snoc :: forall a. Slider a -> a -> Slider a
snoc (Active as) a = Active $ ZipperArray.snoc as a
snoc (Paused as) a = Paused $ ZipperArray.snoc as a
snoc (Inactive as) a = Inactive $ Array.snoc as a

--------------------------------------------------------------------------------
-- Deconstruction --------------------------------------------------------------
--------------------------------------------------------------------------------

toArray :: forall a. Slider a -> Array a
toArray (Active as) = ZipperArray.toArray as
toArray (Paused as) = ZipperArray.toArray as
toArray (Inactive as) = as

--------------------------------------------------------------------------------
-- Traversals ------------------------------------------------------------------
--------------------------------------------------------------------------------

mapCurrent :: forall a b. { cur :: a -> b, rest :: a -> b } -> Slider a -> Slider b
mapCurrent fs (Active as) = Active $ ZipperArray.mapCurrent fs as
mapCurrent fs (Paused as) = Paused $ ZipperArray.mapCurrent fs as
mapCurrent { rest } (Inactive as) = Inactive $ map rest as

mapCurrentWithIndex ::
    forall a b.
    { cur :: Int -> a -> b, rest :: Int -> a -> b } ->
    Slider a ->
    Slider b
mapCurrentWithIndex fs (Active as) = Active $ ZipperArray.mapCurrentWithIndex fs as
mapCurrentWithIndex fs (Paused as) = Paused $ ZipperArray.mapCurrentWithIndex fs as
mapCurrentWithIndex { rest } (Inactive as) = Inactive $ mapWithIndex rest as

--------------------------------------------------------------------------------
-- Inspection ------------------------------------------------------------------
--------------------------------------------------------------------------------

null :: forall a. Slider a -> Boolean
null (Inactive as) = Array.null as
null _ = false

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------
--------------------------------------------------------------------------------

instance showSlider :: Show a => Show (Slider a) where
  show (Active as) = "(Slider.Active " <> show as <> ")"
  show (Paused as) = "(Paused.Active " <> show as <> ")"
  show (Inactive as) = "(Slider.Inactive " <> show as <> ")"

instance sliderFunctor :: Functor Slider where
  map f (Active a) = Active $ map f a
  map f (Paused a) = Paused $ map f a
  map f (Inactive a) = Inactive $ map f a

instance sliderFunctorWithIndex :: FunctorWithIndex Int Slider where
  mapWithIndex f (Active a) = Active $ mapWithIndex f a
  mapWithIndex f (Paused a) = Paused $ mapWithIndex f a
  mapWithIndex f (Inactive a) = Inactive $ mapWithIndex f a

instance sliderFoldable :: Foldable Slider where
  foldr f b (Active as) = Foldable.foldr f b as
  foldr f b (Paused as) = Foldable.foldr f b as
  foldr f b (Inactive as) = Foldable.foldr f b as

  foldl f b (Active as) = Foldable.foldl f b as
  foldl f b (Paused as) = Foldable.foldl f b as
  foldl f b (Inactive as) = Foldable.foldl f b as

  foldMap f (Active a) = Foldable.foldMap f a
  foldMap f (Paused a) = Foldable.foldMap f a
  foldMap f (Inactive a) = Foldable.foldMap f a

instance sliderFoldableWithIndex :: FoldableWithIndex Int Slider where
  foldrWithIndex f b (Active as) = FoldableWithIndex.foldrWithIndex f b as
  foldrWithIndex f b (Paused as) = FoldableWithIndex.foldrWithIndex f b as
  foldrWithIndex f b (Inactive as) = FoldableWithIndex.foldrWithIndex f b as

  foldlWithIndex f b (Active as) = FoldableWithIndex.foldlWithIndex f b as
  foldlWithIndex f b (Paused as) = FoldableWithIndex.foldlWithIndex f b as
  foldlWithIndex f b (Inactive as) = FoldableWithIndex.foldlWithIndex f b as

  foldMapWithIndex f (Active a) = FoldableWithIndex.foldMapWithIndex f a
  foldMapWithIndex f (Paused a) = FoldableWithIndex.foldMapWithIndex f a
  foldMapWithIndex f (Inactive a) = FoldableWithIndex.foldMapWithIndex f a
