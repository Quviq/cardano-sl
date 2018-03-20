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

-- A type which records how many coins each stakeholder owns.
newtype StakeOwnership = StakeOwnership { stakes :: [(StakeholderId, Coin)] }

-- The total amount of coins owned by anyone.
totalCoins :: StakeOwnership -> Integer
totalCoins = sum . map (coinToInteger . snd) . stakes

-- The proportion of coins owned by each stakeholder.
stakeProportions :: StakeOwnership -> [(StakeholderId, Double)]
stakeProportions s =
  [ (x, fromIntegral (coinToInteger c) / fromIntegral n)
  | (x, c) <- stakes s ]
  where
    n = totalCoins s

instance Show StakeOwnership where
  show s =
    unlines
      [ printf "Stakeholder %s has %d coins" (show x) (coinToInteger c)
      | (x, c) <- stakes s ]

instance Arbitrary StakeOwnership where
  arbitrary = (StakeOwnership <$> listOf stake)
    where
      stake = do
        stakeholder <- arbitraryUnsafe
        coin <- arbitraryCoin
        return (stakeholder, coin)

  shrink (StakeOwnership stakes) =
    map StakeOwnership $ genericShrink stakes ++ merge stakes
    where
      -- Try merging adjacent stakeholders, adding their stakes
      -- (this is more likely to work than generic shrinking because it does not
      -- alter the coin indices assigned to other stakeholders)
      merge [] = []
      merge [_] = []
      merge ((x,m):(y,n):xs) =
        [(x, unsafeAddCoin m n):xs] ++
        map ((x,m):) (merge ((y,n):xs))

-- A generator for coins which generates a mixture of small and large coins,
-- rather than using a uniform distribution.
arbitraryCoin :: Gen Coin
arbitraryCoin = sized $ \n -> do
  i <- choose (0, (length bounds - 1) * n `div` 100)
  mkCoin . fromIntegral <$> choose (0, bounds !! i)
  where
    -- [maxBound, maxBound `div` 3, maxBound `div` 9, ...]
    bounds = reverse (takeWhile (> 0) (iterate (`div` 3) (getCoin maxBound)))

prop_satoshi :: ProtocolConstants -> InfiniteList SharedSeed -> StakeOwnership -> Property
prop_satoshi pc (InfiniteList seeds _) s =
  totalCoins s > 0 ==>
  -- Evaluate the property for each stakeholder and "and" the results
  conjoin $ do
    (x, p) <- stakeProportions s
    -- Count how many total slots there are and how many were assigned to x
    let total = countSlots (const True)
        mine = countSlots (== x)
    return (prop x p (zip total mine))
  where
    -- An infinite list of elected stakeholders (each element represents the
    -- results of one election)
    leaders :: [[StakeholderId]]
    leaders = map run seeds
      where
        run seed =
          NonEmpty.toList (withProtocolConstants pc (followTheSatoshi seed (stakes s)))

    -- How many of the elected slots satisfy 'p'? Computes a running sum over
    -- all elections.
    countSlots :: (StakeholderId -> Bool) -> [Int]
    countSlots p = scanl1 (+) (map (length . filter p) leaders)

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

-- rejectPValue n k p: the p-value for rejecting the hypothesis that the
-- probability of being elected is p, after being elected k times in n
-- elections. When this is low enough we reject the test case.
rejectPValue :: Int -> Int -> Double -> Double
rejectPValue n k p
  -- Multiplying by 2 gives us a two-tailed test.
  -- At least, it does for continuous distributions; for a discrete
  -- discrete distribution, it may give a higher answer. This is OK because:
  -- 1) it will only cause us to continue testing instead of stopping;
  -- 2) this error goes away as n becomes larger.
  | freq <= p = 2*cumulative distr (fromIntegral k)
  | otherwise = 2*complCumulative distr (fromIntegral (k-1))
  where
    freq = fromIntegral k / fromIntegral n
    distr = binomial n p

-- acceptPValue n k p: the p-value for rejecting the hypothesis
-- that the probability is p+0.01 or p-0.01 (whichever is most likely).
-- When this is low enough we accept the test case.
acceptPValue :: Int -> Int -> Double -> Double
acceptPValue n k p =
  maximum [rejectPValue n k p' | p' <- [p-0.01, p+0.01], p' >= 0, p' <= 1]
