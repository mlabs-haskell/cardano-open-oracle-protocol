{-# LANGUAGE BlockArguments #-}

module Genesis (genesis, GenesisOpts (GenesisOpts)) where

import BeamConfig (factStatementsCreateTable)
import Cardano.Proto.Aux ()
import Control.Lens (makeLenses, (^.))
import Data.String (IsString (fromString))
import Database.SQLite.Simple (execute_, open)

newtype GenesisOpts = GenesisOpts
  { _db :: FilePath
  }
  deriving stock (Show, Eq)

makeLenses ''GenesisOpts

genesis :: GenesisOpts -> IO ()
genesis opts = do
  conn <- open (opts ^. db)
  execute_ conn (fromString factStatementsCreateTable)
