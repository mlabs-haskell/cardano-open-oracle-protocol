{-# LANGUAGE BlockArguments #-}

module Genesis (genesis, GenesisOpts (GenesisOpts)) where

import BeamConfig (factStatementsCreateTable)
import Cardano.Proto.Aux ()
import Control.Lens (makeLenses, (^.))
import Data.String (IsString (fromString))
import Database.SQLite.Simple (execute_, withConnection)

newtype GenesisOpts = GenesisOpts
  { _db :: FilePath
  }
  deriving stock (Show, Eq)

makeLenses ''GenesisOpts

genesis :: GenesisOpts -> IO ()
genesis opts = withConnection (opts ^. db) $ \dbConn ->
  execute_ dbConn (fromString factStatementsCreateTable)
