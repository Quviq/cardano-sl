{-# OPTIONS_GHC -Wno-name-shadowing -Wno-type-defaults #-}
module Test.Pos.Lrc.FollowTheSatoshi2Spec where

import Prelude

import Test.QuickCheck
import Pos.Util.QuickCheck.Arbitrary
import Pos.Core
import Control.Monad.Except
import Pos.Util.QuickCheck.Backported
import Pos.Lrc (followTheSatoshi)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Map(Map)
import Text.Printf
import Statistics.Distribution
import Statistics.Distribution.Binomial
import Data.Maybe
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec =
  describe "Pos.Lrc.Pure" $
    describe "followTheSatoshi" $
        modifyMaxSuccess (const 10000) $
          prop "All stakeholders get a fair amount" prop_satoshi

newtype Stakes = Stakes { getStakes :: [(StakeholderId, Coin)] }

instance Show Stakes where
  show (Stakes xs) =
    unlines
      [ printf "Stakeholder %s has %d coins" (show sh) (coinToInteger c)
      | (sh, c) <- xs ]

instance Arbitrary Stakes where
  arbitrary = Stakes <$> nonEmptyListOf (sized $ \n -> do
    stakeholder <- arbitraryUnsafe
    let n' = n `max` 1
    coin <- oneof [choose (1, 3 `min` n'), choose (1, 10 `min` n'), choose (1, 30 `min` n'), choose (1, 100 `min` n')]
    return (stakeholder, mkCoin (fromIntegral coin)))
    where
      nonEmptyListOf gen = liftM2 (:) gen (listOf gen)

  shrink (Stakes stakes) =
    map Stakes $
      genericShrink stakes ++
      [[(x, mkCoin (fromIntegral (coinToInteger n `div` 2))) | (x, n) <- stakes]] ++
      merge stakes
    where
      merge [] = []
      merge [_] = []
      merge ((x,m):(y,n):xs) =
        [(x, unsafeAddCoin m n):xs] ++
        map ((x,m):) (merge ((y,n):xs))

prop_satoshi :: ProtocolConstants -> InfiniteList SharedSeed -> Stakes -> Property
prop_satoshi pc (InfiniteList seeds _) stakes =
  n > 0 ==> check 0 (Map.fromList probs) slotss
  where
    n = sum (map (coinToInteger . snd) (getStakes stakes))
    probs = [(x, fromIntegral (coinToInteger k) / fromIntegral n) | (x, k) <- getStakes stakes]

    round seed =
      Map.fromListWith (+)
        [(x, 1) | x <- NonEmpty.toList (withProtocolConstants pc (followTheSatoshi seed (getStakes stakes)))]

    slotss = scanl1 (Map.unionWith (+)) (map round seeds)

    check n probs ~(slots:slotss)
      | Map.null probs = property True -- collect n True
      | otherwise =
        case catMaybes (map (unfair slots) (Map.toList probs)) of
          [] ->
            let probs' = Map.fromList (filter (not . fair slots) (Map.toList probs)) in
            check (n+1) (if n > 10 then probs' else probs) slotss
          xs -> foldr counterexample (property False) xs

fair :: Map StakeholderId Integer -> (StakeholderId, Double) -> Bool
fair slots (sh, p) =
  -- confidence only goes up to (roughly) 0.5
  confidence n p k >= 0.25
  where
    n = sum (Map.elems slots)
    k = Map.findWithDefault 0 sh slots

unfair :: Map StakeholderId Integer -> (StakeholderId, Double) -> Maybe String
unfair slots (sh, p)
  | confidence n p k <= 0.00000001 = Just message
  | otherwise = Nothing
  where
    message =
      printf "After %d slots, stakeholder %s had %d slots but should have had %d (stake=%.3f%%, confidence=%.5f%%)"
        n (show sh) k (truncate (p*fromIntegral n) :: Integer) (100*p) (100*confidence n p k)

    n = sum (Map.elems slots)
    k = Map.findWithDefault 0 sh slots

confidence :: Integer -> Double -> Integer -> Double
confidence n p k
  | freq <= p = cumulative (binomial (fromIntegral n) p) (fromIntegral k)
  | otherwise = complCumulative distr (fromIntegral k-1)
  where
    freq = fromIntegral k / fromIntegral n
    distr = binomial (fromIntegral n) p
