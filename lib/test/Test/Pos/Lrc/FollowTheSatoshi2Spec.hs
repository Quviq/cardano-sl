{-# OPTIONS_GHC -Wno-name-shadowing -Wno-type-defaults #-}
module Test.Pos.Lrc.FollowTheSatoshi2Spec where

import Prelude

import Test.QuickCheck
import Pos.Util.QuickCheck.Arbitrary
import Pos.Core
import Pos.Util.QuickCheck.Backported
import Pos.Lrc (followTheSatoshi)
import qualified Data.List.NonEmpty as NonEmpty
import Text.Printf
import Statistics.Distribution
import Statistics.Distribution.Binomial
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Pos.Lrc.Pure" $
    describe "followTheSatoshi" $
        modifyMaxSuccess (*10) $
          prop "All stakeholders get a fair amount" prop_satoshi

newtype Stakes = Stakes { getStakes :: [(StakeholderId, Coin)] }

totalStakes :: Stakes -> Integer
totalStakes (Stakes xs) =
  sum (map (coinToInteger . snd) xs)

stakesValid :: Stakes -> Bool
stakesValid stakes = totalStakes stakes > 0

instance Show Stakes where
  show (Stakes xs) =
    unlines
      [ printf "Stakeholder %s has %d coins" (show sh) (coinToInteger c)
      | (sh, c) <- xs ]

instance Arbitrary Stakes where
  arbitrary = (Stakes <$> listOf stake) `suchThat` stakesValid
    where
      stake = sized $ \n -> do
        stakeholder <- arbitraryUnsafe
        coin <- oneof [choose (0, 3 `min` n), choose (0, 10 `min` n), choose (0, 30 `min` n), choose (0, 100 `min` n)]
        return (stakeholder, mkCoin (fromIntegral coin))

  shrink (Stakes stakes) =
    filter stakesValid $ map Stakes $ genericShrink stakes ++ merge stakes
    where
      -- Try merging adjacent stakeholders, adding their stakes
      -- (this is more likely to work than generic shrinking because it does not
      -- alter the coin indices assigned to other stakeholders)
      merge [] = []
      merge [_] = []
      merge ((x,m):(y,n):xs) =
        [(x, unsafeAddCoin m n):xs] ++
        map ((x,m):) (merge ((y,n):xs))

prop_satoshi :: ProtocolConstants -> InfiniteList SharedSeed -> Stakes -> Property
prop_satoshi pc (InfiniteList seeds _) stakes =
  conjoin [prop x p (totalsFor x) | (x, p) <- expectedProbabilities]
  where
    expectedProbabilities =
      [ (x, fromIntegral (coinToInteger k) / fromIntegral (totalStakes stakes))
      | (x, k) <- getStakes stakes]

    leaders =
      [ NonEmpty.toList (withProtocolConstants pc (followTheSatoshi seed (getStakes stakes)))
      | seed <- seeds ]

    totalSlots = scanl1 (+) (map length leaders)
    totalSlotsFor x = scanl1 (+) (map (length . filter (== x)) leaders)
    totalsFor x = zip totalSlots (totalSlotsFor x)

    pValue = 0.000000001 -- the target p-value

    prop :: StakeholderId -> Double -> [(Int, Int)] -> Property
    prop x p ~((n, k):xs)
      | acceptPValue n k p <= pValue = property True
      | rejectPValue n k p <= pValue =
        let expectedSlots = truncate (p*fromIntegral n) :: Integer
            actualProbability = fromIntegral k/fromIntegral n :: Double in
        counterexample (printf "Stakeholder %s has wrong number of slots" (show x)) $
        counterexample (printf "Expected: %d out of %d (%.3f%%)" expectedSlots n p) $
        counterexample (printf "Actual: %d out of %d (%.3f%%)" k n actualProbability) $
        counterexample (printf "Rejected with P-value: %.10f%%" (100*rejectPValue n k p)) $
        False
      | otherwise = prop x p xs

-- rejectPpValue n k p: the p-value for rejecting the hypothesis
-- that the probability is p, after n tests with k successes.
-- When this is low enough we reject the test case.
rejectPValue :: Int -> Int -> Double -> Double
rejectPValue n k p
  -- Multiplying by 2 gives us a two-tailed test.
  --
  | freq <= p = 2*cumulative distr (fromIntegral k)
  | otherwise = 2*complCumulative distr (fromIntegral (k-1))
  where
    freq = fromIntegral k / fromIntegral n
    distr = binomial n p

-- acceptPValue n k p: the p-value for rejecting the hypothesis
-- that the probability is p+1% or p-1% (whichever is most likely).
-- When this is low enough we accept the test case.
acceptPValue :: Int -> Int -> Double -> Double
acceptPValue n k p =
  maximum [rejectPValue n k p' | p' <- [p-0.01, p+0.01], p' >= 0, p' <= 1]
