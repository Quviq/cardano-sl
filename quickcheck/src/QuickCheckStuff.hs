module QuickCheckStuff where

import Test.QuickCheck

----------------------------------------------------------------------
-- | @InfiniteList xs _@: guarantees that xs is an infinite list.
-- When a counterexample is found, only prints the prefix of xs
-- that was used by the program.
--
-- Here is a contrived example property:
--
-- > prop_take_10 :: InfiniteList Char -> Bool
-- > prop_take_10 (InfiniteList xs _) =
-- >   or [ x == 'a' | x <- take 10 xs ]
--
-- In the following counterexample, the list must start with @"bbbbbbbbbb"@ but
-- the remaining (infinite) part can contain anything:
--
-- >>> quickCheck prop_take_10
-- *** Failed! Falsifiable (after 1 test and 14 shrinks):
-- "bbbbbbbbbb" ++ ...
data InfiniteList a =
  InfiniteList {
    getInfiniteList :: [a],
    infiniteListInternalData :: InfiniteListInternalData a }

-- Uses a similar trick to Test.QuickCheck.Function:
-- the Arbitrary instance generates an infinite list, which is
-- reduced to a finite prefix by shrinking. We use discard to
-- check that nothing coming after the finite prefix is used
-- (see makeInfiniteList).
data InfiniteListInternalData a = Infinite [a] | FinitePrefix [a]

infiniteListFromData :: InfiniteListInternalData a -> InfiniteList a
infiniteListFromData info@(Infinite xs) = InfiniteList xs info
infiniteListFromData info@(FinitePrefix xs) =
  InfiniteList (xs ++ discard) info

instance Show a => Show (InfiniteList a) where
  showsPrec _ (InfiniteList _ (Infinite _)) =
    ("<infinite list>" ++)
  showsPrec n (InfiniteList _ (FinitePrefix xs)) =
    (if n > 10 then ('(':) else id) .
    showsPrec 0 xs .
    (" ++ ..." ++) .
    (if n > 10 then (')':) else id)

instance Arbitrary a => Arbitrary (InfiniteList a) where
  arbitrary = fmap infiniteListFromData arbitrary
  shrink (InfiniteList _ info) =
    map infiniteListFromData (shrink info)

instance Arbitrary a => Arbitrary (InfiniteListInternalData a) where
  arbitrary = fmap Infinite infiniteList
  shrink (Infinite xs) =
    [FinitePrefix (take n xs) | n <- map (2^) [0..]]
  shrink (FinitePrefix xs) =
    map FinitePrefix (shrink xs)
