-- | Simpler specification of Pos.Core.Coin

{-# OPTIONS_GHC -w #-}
module Test.Pos.Core.Coin2Spec ( spec ) where

import Universum

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck

import Pos.Core.Common
--import Pos.Arbitrary.Core

import Control.Exception (ErrorCall)

-- A generator for Coin values that tries to select interesting cases

newtype C = C Coin deriving Show

instance Arbitrary C where
  arbitrary = (C . mkCoin) <$>
  	      (oneof $ oneof [choose (0,10),
  	      		      ((-) maxCoinVal) <$> choose (0,10)] :
		       [choose(0,n) | n <- takeWhile (>10) $
	          		      	     iterate (`div` 1000) maxCoinVal])

--  shrink (C coin) = map C (shrink coin)

spec = describe "Coin properties" $ do
    prop "unsafeAddCoin" ((+) `models` unsafeAddCoin)
    prop "unsafeSubCoin" ((-) `models` unsafeSubCoin)
--    prop "unsafeMulCoin" ((*) `models` unsafeMulCoin)

(op `models` impl) (C c1) (C c2) =
    cover ok       40 "successful case" .
    cover (not ok)  5 "exceptional case" $
    ioProperty (catch (return $! unsafeGetCoin (c1 `impl` c2) == ans)
                      (\(_ :: ErrorCall) -> return $ not ok))
    where ans = unsafeGetCoin c1 `op` unsafeGetCoin c2
	  ok  = ans >= 0 && ans <= maxCoinVal