{-# OPTIONS_GHC -Wno-name-shadowing -Wno-type-defaults #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Reflection
import System.Random
import Pos.Crypto.Random

spec :: Spec
spec =
  describe "Pos.Lrc.Pure" $ do
    describe "followTheSatoshi2" $ do
      prop "All stakeholders get a fair amount" (noFaults prop_satoshi)
      prop "The property detects unfair implementations" (expectFailure (faultRate 0.01 prop_satoshi))
    describe "randomNumber" $
      prop "Check distribution of random numbers" prop_randomNumber

-- What is the smallest error we want to catch?
-- The tolerance is represented as a proportion of the expected probability.
-- If the expected probability is p, the actual probability must lie between
-- p-tolerance p and p+tolerance p.
tolerance :: Double -> Double
tolerance p =
  -- If the probability is small, we check that it is at most 0.5%.
  -- Otherwise, the tolerance is 5% of the probability.
  clamp (0.005, 1) (p * 0.05)

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
  arbitrary =
    (StakeOwnership <$> listOf stake)
    `suchThat` (\s -> totalCoins s <= fromIntegral (getCoin maxBound))
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

prop_satoshi :: Faulty => ProtocolConstants -> InfiniteList SharedSeed -> StakeOwnership -> Property
prop_satoshi pc (InfiniteList seeds _) s =
  totalCoins s > 0 ==>
  -- Fault injection for testing if the property finds bugs
  perturbing s $ \sFault ->
  -- Evaluate the property for each stakeholder and "and" the results
  conjoin $ do
    (x, p) <- stakeProportions sFault
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

    -- The target p-value.
    -- Bear in mind that we are running (say) 1000 tests, with an
    -- average of (say) 50 stakeholders, so a p-value of 1% would be
    -- hit almost every time! This value means we expect to see a
    -- false positive every billion tests or so.
    pValue = 1/10^11
    -- XXX would it make sense to a have a lower p-value for
    -- acceptance, with a smaller tolerance?

    prop :: StakeholderId -> Double -> [(Int, Int)] -> Property
    prop x p ~((n, k):xs)
      | rejectPValue n k p <= pValue =
        let expectedSlots = truncate (p*fromIntegral n) :: Integer
            actualProbability = fromIntegral k/fromIntegral n :: Double in
        counterexample (printf "Stakeholder %s has wrong number of slots" (show x)) $
        counterexample (printf "Expected: %d out of %d (%.3f%%)" expectedSlots n p) $
        counterexample (printf "Actual: %d out of %d (%.3f%%)" k n actualProbability) $
        counterexample (printf "Rejected with P-value: %.10f%%" (100*rejectPValue n k p)) $
        False
      | acceptPValue n k p <= pValue = property True
      | otherwise = prop x p xs

prop_randomNumber :: InfiniteList SharedSeed -> Positive Integer -> Property
prop_randomNumber (InfiniteList seeds _) (Positive n) =
  conjoin [check 0 0 i vals | i <- [0..n-1]]
  where
    vals = map gen seeds
    gen (SharedSeed seed) =
      deterministic seed (randomNumber n)

    p = 1 / fromIntegral n
    check n k x ~(val:vals)
      | rejectPValue n k p <= pValue =
        let expectedSlots = truncate (p*fromIntegral n) :: Integer
            actualProbability = fromIntegral k/fromIntegral n :: Double in
        counterexample (printf "Stakeholder %s has wrong number of slots" (show x)) $
        counterexample (printf "Expected: %d out of %d (%.3f%%)" expectedSlots n p) $
        counterexample (printf "Actual: %d out of %d (%.3f%%)" k n actualProbability) $
        counterexample (printf "Rejected with P-value: %.10f%%" (100*rejectPValue n k p)) $
        False
      | acceptPValue n k p <= pValue = property True
      | otherwise =
        check (n+1) (k + if x == val then 1 else 0) x vals

    pValue = 1/10^6

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
-- that the probability is outside of the tolerance.
-- When this is low enough we accept the test case.
acceptPValue :: Int -> Int -> Double -> Double
acceptPValue n k p =
  maximum [rejectPValue n k p' | p' <- [p-tolerance p, p+tolerance p], p' >= 0, p' <= 1]

-- Experimental work on fault injection.
newtype FaultRate = FaultRate Double
type Faulty = Given FaultRate

faultRate :: Testable prop => Double -> (Faulty => prop) -> Property
faultRate x prop = property (give (FaultRate x) prop)

noFaults :: Testable prop => (Faulty => prop) -> Property
noFaults = faultRate 0

maybeFault :: Faulty => Gen a -> Gen (Maybe a)
maybeFault bad = do
  let FaultRate p = given
  x <- choose (0, 1)
  -- Should never generate faulty data when p=0
  if x <= p && p /= 0 then Just <$> bad else return Nothing

fault :: Faulty => Gen a -> Gen a -> Gen a
fault bad good = do
  mx <- maybeFault bad
  case mx of
    Just x -> return x
    Nothing -> good

clamp :: Ord a => (a, a) -> a -> a
clamp (lower, upper) x
  | x < lower = lower
  | x > upper = upper
  | otherwise = x

perturbBy :: (Num a, Random a) => a -> a -> Gen a
perturbBy bound x = do
  err <- choose (-bound, bound)
  return (x+err)

class Perturb a where
  perturb :: a -> Gen a

instance Perturb StakeOwnership where
  perturb s =
    StakeOwnership <$>
    sequence [do c' <- mkCoin <$> fromIntegral <$>
                       -- Coin is really a Word64;
                       -- do arithmetic on Integer so that clamping to 0 works
                       clamp (0, coins) <$> perturbBy (bound n) n
                 return (x, c')
             | (x, c) <- stakes s,
               let n = fromIntegral (getCoin c) ]
    where
      coins = totalCoins s
      bound n = truncate (fromIntegral coins * tolerance (fromIntegral n / fromIntegral coins))

perturbing :: (Faulty, Perturb a, Show a, Testable prop) =>
  a -> (a -> prop) -> Property
perturbing x prop = property $ again $ do
  my <- maybeFault (perturb x)
  return $
    case my of
      Nothing -> property (prop x)
      Just y ->
        counterexample ("Injected fault: " ++ show y) (prop y)
