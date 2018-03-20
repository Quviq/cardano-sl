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
  conjoin [prop 1 x p (totalsFor x) | (x, p) <- expectedProbabilities]
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

    prop :: Int -> StakeholderId -> Double -> [(Int, Int)] -> Property
    prop i x p ~((n, k):xs)
      | and [ confidence n k p' <= 0.000000001
            | p' <- [p-0.01, p+0.01], p' >= 0, p' <= 1 ] =
        collect i (property True)
      | confidence n k p <= 0.00000001 =
        let expectedSlots = truncate (p*fromIntegral n) :: Integer
            actualProbability = fromIntegral k/fromIntegral n :: Double in
        counterexample (printf "Wrong number of slots after round %d (%d slots total)" i n) $
        counterexample (printf "Stakeholder: %s" (show x)) $
        counterexample (printf "Expected slots: %d (%.3f%%)" expectedSlots p) $
        counterexample (printf "Actual slots: %d (%.3f%%)" k actualProbability) $
        counterexample (printf "P-value: %.3f%%" (100*confidence n k p)) $
        False
      | otherwise = prop (i+1) x p xs

confidence :: Int -> Int -> Double -> Double
confidence n k p
  | freq <= p = min 1 (2*cumulative distr (fromIntegral k))
  | otherwise = min 1 (2*complCumulative distr (fromIntegral (k-1)))
  where
    freq = fromIntegral k / fromIntegral n
    distr = binomial n p
