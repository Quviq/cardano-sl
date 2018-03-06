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
import Text.Printf
import Statistics.ConfidenceInt
import Statistics.Distribution
import Statistics.Distribution.Binomial
import Statistics.Types
import Data.Maybe

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

newtype Stakes = Stakes [(StakeholderId, Integer)] deriving Generic

getStakes :: Stakes -> [(StakeholderId, Coin)]
getStakes (Stakes xs) =
  [(sh, mkCoin (fromInteger (n * base))) | (sh, n) <- xs]
  where
    base = 1000000

instance Show Stakes where
  show stakes =
    unlines
      [ printf "Stakeholder %s has %d coins" (show sh) (coinToInteger c)
      | (sh, c) <- getStakes stakes ]

instance Arbitrary Stakes where
  arbitrary = Stakes <$> nonEmptyListOf (do
    stakeholder <- arbitraryUnsafe
    coin <- oneof [choose (0, 2), choose (0, 9), choose (0, 50)]
    return (stakeholder, coin))
    where
      nonEmptyListOf gen = liftM2 (:) gen (listOf gen)

  shrink = genericShrink

prop_satoshi :: Property
prop_satoshi =
  withDefNodeConfiguration $ withDefDlgConfiguration $ withDefSscConfiguration $
  blockPropertyTestable $ stop prop_satoshi_inner

prop_satoshi_inner :: HasConfiguration => InfiniteList SharedSeed -> Stakes -> Property
prop_satoshi_inner (InfiniteList seeds _) stakes =
  n > 0 ==> check 1 slotss
  where
    n = sum (map (coinToInteger . snd) (getStakes stakes))
    probs = [(x, fromIntegral (coinToInteger k) / fromIntegral n) | (x, k) <- getStakes stakes]

    round seed =
      Map.fromListWith (+)
        [(x, 1) | x <- NonEmpty.toList (followTheSatoshi seed (getStakes stakes))]

    slotss = scanl1 (Map.unionWith (+)) (map round seeds)

    check n (slots:slotss)
      | all (fair slots) probs =
        collect n True
      | otherwise =
        case catMaybes (map (unfair slots) probs) of
          [] -> check (n+1) slotss
          xs -> foldr counterexample (property False) xs

fair :: Map StakeholderId Integer -> (StakeholderId, Double) -> Bool
fair slots (sh, p) =
  p `inInterval` (x, y) && (y-x <= 0.01 || (y-x)*5 <= p)
  where
    (x, y) = interval 0.1 n k
    n = sum (Map.elems slots)
    k = Map.findWithDefault 0 sh slots

unfair :: Map StakeholderId Integer -> (StakeholderId, Double) -> Maybe String
unfair slots (sh, p)
  | not (p `inInterval` (x, y)) = Just message
  | otherwise = Nothing
  where
    message =
      printf "After %d slots, stakeholder %s had %d slots but should have had %d (stake=%.3f%%, low=%.3f%%, high=%.3f%%)"
        n (show sh) k (truncate (p*fromIntegral n) :: Integer) (100*p) (100*x) (100*y)

    n = sum (Map.elems slots)
    k = Map.findWithDefault 0 sh slots
    (x, y) = interval 0.000001 n k

-- Confidence interval for binomial distribution
interval :: Double -> Integer -> Integer -> (Double, Double)
interval a n k
  -- Work around bug in statistics package
  | n == k = (x, fromIntegral 1)
  | otherwise = (x, y)
  where
    (x, y) = confidenceInterval (binomialCI (mkCLFromSignificance a) (fromIntegral n) (fromIntegral k))

inInterval :: Double -> (Double, Double) -> Bool
y `inInterval` (x, z) = x <= y && y <= z
