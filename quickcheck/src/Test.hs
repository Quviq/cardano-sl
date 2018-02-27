{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS_GHC -w #-}
module Test where

import Prelude

import Test.QuickCheck
import Pos.Arbitrary.Core
import Pos.Arbitrary.Crypto
import Pos.Core
import Control.Monad.Except
import Text.JSON.Canonical
import Data.Reflection
import Pos.Crypto.Configuration
import qualified Data.Map as Map

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
