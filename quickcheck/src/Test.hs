{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -w #-}
module Test where

import Prelude

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Pos.Arbitrary.Core
import Pos.Arbitrary.Crypto
import Pos.Util.QuickCheck.Arbitrary
import Pos.Core
import Test.Pos.Configuration
import Control.Monad.Except
import Text.JSON.Canonical
import Data.Reflection
import Pos.Crypto.Configuration
import QuickCheckStuff
import Pos.Lrc (followTheSatoshi)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map(Map)
import Data.List
import Test.Pos.Block.Logic.Mode
import GHC.Generics

deriving instance Eq SchemaError

type M = ExceptT SchemaError IO

roundTrip :: forall a. (ToJSON M a, FromJSON M a, Eq a, Show a) => a -> Property
roundTrip x =
  ioProperty $ do
    res <- runExceptT (toJSON x >>= fromJSON :: M a)
    return $ res === return x

prop :: ProtocolConstants -> ProtocolMagic -> Property
prop constants magic =
  give constants $ give magic $ 
  property $ \(x :: GenesisData) -> roundTrip x

main = quickCheckWith stdArgs { maxSuccess = 1000 } prop

prop_addrstake :: AddrStakeDistribution -> Bool
prop_addrstake (UnsafeMultiKeyDistr m) =
  sum (map getCoinPortion (Map.elems m)) == coinPortionDenominator &&
  all ((> 0) . getCoinPortion) (Map.elems m) &&
  Map.size m >= 2
prop_addrstake _ = discard

newtype Stakes = Stakes [(StakeholderId, Coin)] deriving (Show, Generic)
instance Arbitrary Stakes where
  arbitrary = Stakes <$> listOf (do
    stakeholder <- arbitraryUnsafe
    coin <- oneof [choose (0, 2), choose (0, 9), choose (0, 50)]
    return (stakeholder, mkCoin coin)) `suchThat` (not . null)
  shrink = genericShrink

prop_satoshi :: Property
prop_satoshi =
  withDefNodeConfiguration $ withDefDlgConfiguration $ withDefSscConfiguration $
  blockPropertyTestable $ stop prop_satoshi_inner

prop_satoshi_inner :: HasConfiguration => InfiniteList SharedSeed -> Stakes -> Property
prop_satoshi_inner (InfiniteList seeds _) (Stakes stakes) =
  n > 0 ==>
    check 0 (scanl1 (Map.unionWith (+)) (map round seeds))
  where
    n = sum (map (coinToInteger . snd) stakes)
    probs = [(x, fromIntegral (coinToInteger k) / fromIntegral n) | (x, k) <- stakes]

    round seed =
      Map.fromListWith (+)
        [(x, 1) | x <- NonEmpty.toList (followTheSatoshi seed stakes)]

    check n (map:maps) =
      counterexample (show map) $
      case allFairlyChosen probs map of
        Just x  -> collect n x
        Nothing -> check (n+1) maps

allFairlyChosen :: [(StakeholderId, Double)] -> Map StakeholderId Integer -> Maybe Bool
allFairlyChosen stakes slots
  | Just False `elem` results = Just False
  | all (== Just True) results = Just True
  | otherwise = Nothing
  where
    n = sum (Map.elems slots)
    results =
      [ fairlyChosen (Map.findWithDefault 0 x slots) n p | (x, p) <- stakes ]

fairlyChosen :: Integer -> Integer -> Double -> Maybe Bool
fairlyChosen k n p
  | cdf k n p <= 0.01 = Just False
    -- A very ad hoc test
  | mean - stddev <= fromIntegral k &&
    fromIntegral k <= mean + stddev &&
    -- N.B. stddev grows as sqrt n
    stddev <= fromIntegral n * 0.05 = Just True
  | otherwise = Nothing
  where
    mean = p * fromIntegral n
    stddev = sqrt (fromIntegral n * p * (1-p))

-- N.B. this is an approximation of the cdf for the binomial distribution.
-- It computes the probability that the value observed is at least this
-- extreme (far from the expected value), so e.g. cdf k n (k/n) = 1.
cdf :: Integer -> Integer -> Double -> Double
cdf k n p
  | k == 0 = (1-p)^^n
  | k == n = p^^n
  | otherwise = exp(-fromIntegral n * entropy (fromIntegral k / fromIntegral n) p)

entropy :: Double -> Double -> Double
entropy a p = a * log (a / p) + (1-a) * log ((1-a) / (1-p))
