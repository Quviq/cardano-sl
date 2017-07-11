-- | Execution mode used by blockchain generator.

module Pos.Generator.Block.Mode
       ( MonadBlockGenBase
       , MonadBlockGen
       , BlockGenContext (..)
       , BlockGenMode
       , mkBlockGenContext
       ) where

import           Universum

import           Control.Exception           (Exception (..))
import           Control.Lens.TH             (makeLensesWith)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Text.Buildable
import           Mockable                    (MonadMockable)
import           System.Wlog                 (WithLogger)

import           Pos.Block.Core              (Block, BlockHeader)
import           Pos.Block.Slog              (HasSlogContext (..), SlogContext)
import           Pos.Block.Types             (Undo)
import           Pos.Context                 (GenesisUtxo (..))
import           Pos.Core                    (BlockCount, EpochOrSlot (..),
                                              HasPrimaryKey (..), IsHeader, SlotId,
                                              StakeholderId, Timestamp, getEpochOrSlot)
import           Pos.Crypto                  (SecretKey)
import           Pos.DB                      (DBPureVar, MonadBlockDBGeneric (..),
                                              MonadDB, MonadDBRead)
import qualified Pos.DB                      as DB
import qualified Pos.DB.Block                as BDB
import           Pos.Delegation              (DelegationVar)
import           Pos.Generator.Block.Param   (BlockGenParams, HasBlockGenParams (..))
import           Pos.Lrc                     (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting                (SlottingData)
import           Pos.Ssc.Class               (SscBlock)
import           Pos.Ssc.Extra               (SscMemTag, SscState)
import           Pos.Ssc.GodTossing          (SscGodTossing)
import           Pos.Txp                     (GenericTxpLocalData, TxPayload,
                                              TxpHolderTag, TxpMetrics, emptyTxPayload)
import           Pos.Update.Context          (UpdateContext)
import           Pos.Util                    (HasLens (..), Some, postfixLFields)
import           Pos.Util.Chrono             (OldestFirst (..))
import           Pos.WorkMode.Class          (TxpExtra_TMP)

----------------------------------------------------------------------------
-- Constraint
----------------------------------------------------------------------------

-- | A set of constraints imposed on the base monad used for
-- arbitrary blockchain generation.
type MonadBlockGenBase m
     = ( WithLogger m
       , MonadMask m
       , MonadIO m
       , MonadMockable m
       , MonadBaseControl IO m
       )

-- | A set of constraints necessary for blockchain generation. All
-- these constraints must be satistified by the generator's caller.
type MonadBlockGen ctx m
     = ( MonadBlockGenBase m
       , MonadReader ctx m
       , HasBlockGenParams ctx
       -- TODO: we don't really need to require pure db from the
       -- outside. We can clone real DB into a pure one. Or we can use
       -- 'DBProxyT' approach. Currently we require pure DB because
       -- it's easier.
       , HasLens DBPureVar ctx DBPureVar
       )

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

-- | Context used by blockchain generator.
data BlockGenContext = BlockGenContext
    { bgcPrimaryKey    :: SecretKey
    -- ^ This field is lazy on purpose. Primary key used for block
    -- generation changes frequently. We don't define it initially and
    -- modify it using 'local' when we need it. Alternative solution
    -- would be to define a type w/o primary key and another one with
    -- primary key, but it would lead to enormous amount of
    -- boilerplate. Also it could be put into mutable reference, but
    -- it's complicated too.
    , bgcDB            :: !DBPureVar
    -- ^ Pure DB used by block generation. Currently we always use
    -- pure DB and assume it always fit in memory. It allows us to
    -- simply clone existing DB.
    , bgcSlogContext   :: !SlogContext
    , bgcParams        :: !BlockGenParams
    , bgcDelegation    :: !DelegationVar
    , bgcLrcContext    :: !LrcContext
    , bgcTxpMem        :: !(GenericTxpLocalData TxpExtra_TMP, TxpMetrics)
    , bgcUpdateContext :: !UpdateContext
    , bgcSscState      :: !(SscState SscGodTossing)
    }

makeLensesWith postfixLFields ''BlockGenContext

-- | Execution mode for blockchain generation.
type BlockGenMode m = ReaderT BlockGenContext m

mkBlockGenContext :: MonadBlockGen ctx m => m BlockGenContext
mkBlockGenContext = undefined

----------------------------------------------------------------------------
-- Boilerplate instances
----------------------------------------------------------------------------

instance HasLens BlockGenContext BlockGenContext BlockGenContext where
    lensOf = identity

instance HasBlockGenParams BlockGenContext where
    blockGenParams = bgcParams_L

instance HasLens DBPureVar BlockGenContext DBPureVar where
    lensOf = bgcDB_L

instance HasLens UpdateContext BlockGenContext UpdateContext where
    lensOf = bgcUpdateContext_L

instance HasLens DelegationVar BlockGenContext DelegationVar where
    lensOf = bgcDelegation_L

instance HasLens LrcContext BlockGenContext LrcContext where
    lensOf = bgcLrcContext_L

instance HasPrimaryKey BlockGenContext where
    primaryKey = bgcPrimaryKey_L

instance HasSlogContext BlockGenContext where
    slogContextL = bgcSlogContext_L

instance HasLens TxpHolderTag BlockGenContext (GenericTxpLocalData TxpExtra_TMP, TxpMetrics) where
    lensOf = bgcTxpMem_L

instance HasLens SscMemTag BlockGenContext (SscState SscGodTossing) where
    lensOf = bgcSscState_L

instance MonadBlockGenBase m => MonadDBRead (BlockGenMode m) where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault

instance MonadBlockGenBase m => MonadDB (BlockGenMode m) where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault

instance MonadBlockGenBase m =>
    MonadBlockDBGeneric (BlockHeader SscGodTossing) (Block SscGodTossing) Undo (BlockGenMode m)
  where
    dbGetBlock = BDB.dbGetBlockPureDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoPureDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderPureDefault @SscGodTossing

instance MonadBlockGenBase m =>
    MonadBlockDBGeneric (Some IsHeader) (SscBlock SscGodTossing) () (BlockGenMode m)
  where
    dbGetBlock = BDB.dbGetBlockSscPureDefault @SscGodTossing
    dbGetUndo = BDB.dbGetUndoSscPureDefault @SscGodTossing
    dbGetHeader = BDB.dbGetHeaderSscPureDefault @SscGodTossing
