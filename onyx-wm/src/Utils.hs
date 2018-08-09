module Utils where

import Onyx.Prelude

import Data.List.HT (partitionMaybe)
import Data.List.NonEmptyZipper (NonEmptyZipper(..))

nezFind :: (a -> Bool) -> [a] -> Maybe (NonEmptyZipper a)
nezFind p xs = do
  let l = takeWhile (not . p) xs
  (x, r) <- uncons (dropWhile (not . p) xs)
  pure (NonEmptyZipper l x r)

(&&&) :: (a -> b) -> (a -> c) -> a -> (b,c)
(f &&& g) x = (f x, g x)

takeF :: Foldable f => Int -> f a -> [a]
takeF i = take i . toList

dropF :: Foldable f => Int -> f a -> [a]
dropF i = drop i . toList

find' :: [(a, b)] -> (b -> Maybe c) -> Maybe (a, c)
find' xs f = msum $ fmap (\(x,y) -> (x,) <$> f y) xs

find'' :: (a -> Maybe b) -> [a] -> Maybe (a, b)
find'' p = msum . fmap (\x -> (x,) <$> p x)

-- boolM :: Applicative m => m () -> Bool -> m ()
-- boolM m = bool (pure ()) m

safeIdx :: Int -> [a] -> Maybe a
safeIdx _ [] = Nothing
safeIdx i (x:xs)
  | i == 0 = Just x
  | otherwise = safeIdx (i - 1) xs

const2 :: c -> a -> b -> c
const2 x _ _ = x

maybeM :: Applicative m => (a -> m ()) -> Maybe a -> m ()
maybeM f = maybe (pure ()) f

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs = let a = xs !! i ; b = xs !! j in
  flip fmap (zip xs [0..]) $ \(x,k) -> case k of
  _ | k == i -> b
  _ | k == j -> a
  _ -> x

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

overSubstring :: (a -> Bool) -> ([a] -> [a]) -> [a] -> [a]
overSubstring p f ss =
  let ((ixs,l2), l1) = bimap unzip id (partition (p . snd) (zip [0 :: Int ..] ss))
  in fmap snd . sortBy (on compare fst) . (l1 ++) . zip (ixs ++ [length ss..]) $ f l2

mayOverSubstring :: (a -> Maybe b) -> (b -> a) -> ([b] -> [b]) -> [a] -> [a]
mayOverSubstring p g f ss =
  let ((ixs,l2), l1) = bimap unzip id (partitionMaybe (sequence . second p) (zip [0 :: Int ..] ss))
  in fmap snd . sortBy (on compare fst) . (l1 ++) . zip (ixs ++ [length ss..]) $ fmap g (f l2)

mayOverSubstring' :: (a -> Maybe b) -> ([b] -> [a]) -> [a] -> [a]
mayOverSubstring' p f ss =
  let ((ixs,l2), l1) = bimap unzip id (partitionMaybe (sequence . second p) (zip [0 :: Int ..] ss))
  in fmap snd . sortBy (on compare fst) . (l1 ++) . zip (ixs ++ [length ss..]) $ f l2

filterOut :: (a -> Bool) -> [a] -> [a]
filterOut p = filter (not . p)
