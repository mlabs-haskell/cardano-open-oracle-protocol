{-# LANGUAGE BlockArguments #-}

module InsertFs (insertFs, InsertFsOpts (InsertFsOpts)) where

import BeamConfig (FactStatementT (FactStatement), FsStore (fsTbl), fsStoreSettings)
import Cardano.Proto.Aux ()
import Control.Lens (makeLenses, (^.))
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Beam.Query (insert, insertValues, runInsert)
import Database.Beam.Sqlite (runBeamSqliteDebug)
import Database.SQLite.Simple (withConnection)

data InsertFsOpts = InsertFsOpts
  { _db :: FilePath
  , _fsId :: Text
  , _json :: Text
  }
  deriving stock (Show, Eq)

makeLenses ''InsertFsOpts

insertFs :: InsertFsOpts -> IO ()
insertFs opts = withConnection (opts ^. db) $ \dbConn ->
  runBeamSqliteDebug putStrLn dbConn $
    runInsert $
      insert (fsTbl fsStoreSettings) $
        insertValues
          [ FactStatement (encodeUtf8 (opts ^. fsId)) (encodeUtf8 (opts ^. json)) :: FactStatementT Identity
          ]
