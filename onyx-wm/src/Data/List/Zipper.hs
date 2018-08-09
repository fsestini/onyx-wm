{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.List.Zipper where

import Data.Foldable
import Data.List.NonEmptyZipper as NEZ hiding (toList, length)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad
import Data.List (uncons)
import Lens.Micro.Platform
-- import Control.Arrow ((>>>))

newtype Zipper a = Zipper
  { unZipper :: Maybe (NonEmptyZipper a)
  } deriving (Eq, Functor)

instance Foldable Zipper where
  foldr f z = maybe z (foldr f z) . unZipper

focusedL :: Traversal' (Zipper a) a
focusedL _ z@(Zipper Nothing) = pure z
focusedL f (Zipper (Just (NonEmptyZipper xs x ys))) =
  fmap (Zipper . Just) $ NonEmptyZipper <$> pure xs <*> f x <*> pure ys

focused :: Zipper a -> Maybe a
focused = fmap _current . unZipper

-- | Replaces the focused element with a new element. If the collection is
-- empty, it does nothing.
setFocus :: a -> Zipper a -> Zipper a
setFocus x = Zipper . fmap (set current x) . unZipper

fromList :: [a] -> Zipper a
fromList =
  foldr (\x xs -> Zipper (Just (fromNonEmpty (x :| (toList xs))))) (Zipper Nothing)

fromListNE :: [a] -> Maybe (NonEmptyZipper a)
fromListNE = fmap (uncurry (|:)) . uncons

filterNE :: Eq a => (a -> Bool) -> NonEmptyZipper a -> Maybe (NonEmptyZipper a)
filterNE p z = (setCurrent (_current z) <=< fromListNE . filter p . toList) z

filterZ :: Eq a => (a -> Bool) -> Zipper a -> Zipper a
filterZ p = Zipper . (>>= filterNE p) . unZipper

idxNE :: NonEmptyZipper a -> Int
idxNE (NonEmptyZipper xs _ _) = length xs

idx :: Zipper a -> Maybe Int
idx = fmap idxNE . unZipper

tryFocus :: Eq a => (a -> Bool) -> Zipper a -> Maybe (Zipper a)
tryFocus p z = do
  x <- find p z
  unZipper z >>= fmap (Zipper . Just) . setCurrent x

focusAt :: Int -> Zipper a -> Maybe (Zipper a)
focusAt i = fmap (Zipper . Just) . (>>= NEZ.setCurrentIndex i) . unZipper

swapAt' :: Int -> Int -> Zipper a -> Zipper a
swapAt' i j z = maybe z id $ do
  a <- focusAt i z >>= focused
  b <- focusAt j z >>= focused
  (focusAt i z <&> setFocus b) >>= (fmap (setFocus a) . focusAt j)
