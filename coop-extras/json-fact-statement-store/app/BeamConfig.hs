-- | Beam definitions and wiring
module BeamConfig (fsStoreSettings, FactStatementT (..), FsStore (fsTbl), factStatementsCreateTable) where

import Data.ByteString (ByteString)
import Database.Beam (Beamable, Columnar, Database, DatabaseSettings, Generic, Table (PrimaryKey, primaryKey), TableEntity, dbModification, defaultDbSettings, fieldNamed, modifyTableFields, setEntityName, tableModification, withDbModification)

data FactStatementT f = FactStatement
  { _factStatementId :: Columnar f ByteString
  , _json :: Columnar f ByteString
  }
  deriving stock (Generic)

instance Beamable FactStatementT

instance Table FactStatementT where
  data PrimaryKey FactStatementT f = FsId (Columnar f ByteString)
    deriving stock (Generic)
    deriving anyclass (Beamable)
  primaryKey = FsId . _factStatementId

newtype FsStore f = FsStore
  {fsTbl :: f (TableEntity FactStatementT)}
  deriving stock (Generic)
  deriving anyclass (Database be)

fsStoreSettings :: DatabaseSettings be FsStore
fsStoreSettings =
  defaultDbSettings
    `withDbModification` dbModification
      { fsTbl =
          setEntityName "fact_statements"
            <> modifyTableFields
              tableModification
                { _factStatementId = fieldNamed "fact_statement_id"
                , _json = fieldNamed "json"
                }
      }

factStatementsCreateTable :: String
factStatementsCreateTable = "CREATE TABLE fact_statements (fact_statement_id BLOB NOT NULL, json BLOB NOT NULL, PRIMARY KEY( fact_statement_id ))"
