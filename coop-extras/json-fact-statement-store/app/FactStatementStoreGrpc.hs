{-# LANGUAGE BlockArguments #-}

module FactStatementStoreGrpc (factStatementStoreService, FactStatementStoreGrpcOpts (FactStatementStoreGrpcOpts)) where

import BeamConfig (FactStatementT (_factStatementId, _json), FsStore (fsTbl), fsStoreSettings)
import Cardano.Proto.Aux ()
import Control.Lens (makeLenses, (&), (.~), (^.), (^..))
import Data.Aeson (json)
import Data.Aeson.Parser (decodeStrictWith)
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.ProtoLens (Message (defMessage))
import Data.String (IsString (fromString))
import Data.Traversable (for)
import Database.Beam (SqlValable (val_), runSelectReturningOne)
import Database.Beam.Query (SqlEq ((==.)), all_, filter_, select)
import Database.Beam.Sqlite (runBeamSqliteDebug)
import Database.SQLite.Simple (open)
import Network.GRPC.HTTP2.Encoding as Encoding (
  gzip,
  uncompressed,
 )
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server as Server (
  ServiceHandler,
  UnaryHandler,
  runGrpc,
  unary,
 )
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS (tlsSettings)
import PlutusJson (jsonToPlutusData)
import PlutusTx (fromData)
import Proto.Coop_Fields (value)
import Proto.FactStatementStoreService (FactStatementStore, GetFactStatementRequest, GetFactStatementResponse, GetFactStatementResponse'FsIdAndPlutus)
import Proto.FactStatementStoreService_Fields (fsId, fsIds, fsIdsWithPlutus, msg, plutusData)
import Proto.FactStatementStoreService_Fields qualified as Proto

data FactStatementStoreGrpcOpts = FactStatementStoreGrpcOpts
  { _db :: FilePath
  , _grpcAddress :: String
  , _grpcPort :: Int
  , _tlsCertFile :: FilePath
  , _tlsKeyFile :: FilePath
  }
  deriving stock (Show, Eq)

makeLenses ''FactStatementStoreGrpcOpts

factStatementStoreService :: FactStatementStoreGrpcOpts -> IO ()
factStatementStoreService opts = do
  let routes :: [ServiceHandler]
      routes =
        [Server.unary (RPC :: RPC FactStatementStore "getFactStatement") (handleReq $ opts ^. db)]

  runServer
    routes
    (fromString $ opts ^. grpcAddress, opts ^. grpcPort)
    (opts ^. tlsCertFile, opts ^. tlsKeyFile)

runServer :: [ServiceHandler] -> (Warp.HostPreference, Int) -> (FilePath, FilePath) -> IO ()
runServer routes (h, p) (certFile, keyFile) = do
  let warpSettings =
        Warp.defaultSettings
          & Warp.setPort p
          & Warp.setHost h
  Server.runGrpc
    (tlsSettings certFile keyFile)
    warpSettings
    routes
    [ Encoding.uncompressed
    , Encoding.gzip
    ]

type Fs = FactStatementT Identity

handleReq :: FilePath -> Server.UnaryHandler IO GetFactStatementRequest GetFactStatementResponse
handleReq dbPath _ req = do
  putStrLn $ "Establishing the database connection to: " <> dbPath
  fsDb <- open dbPath
  let fsTbl' = fsTbl fsStoreSettings
      ids = nub $ req ^. fsIds ^.. traverse . value

  mayIdsWithPs <-
    sequence
      <$> for
        ids
        ( \i -> do
            (mayFsWithId :: Maybe Fs) <- runBeamSqliteDebug Prelude.putStrLn fsDb $ runSelectReturningOne (select $ filter_ (\fs -> _factStatementId fs ==. val_ i) (all_ fsTbl'))
            maybe
              (putStrLn ("Not found requested fact statement with id " <> show i) >> return Nothing)
              (return . Just)
              mayFsWithId
        )

  idsWithPs <-
    maybe
      (putStrLn "There were errors processing the requests" >> return [])
      return
      mayIdsWithPs

  mayIdsWithPs' <-
    sequence
      <$> for
        idsWithPs
        ( \idWithP -> do
            let fsIdAndP :: Maybe GetFactStatementResponse'FsIdAndPlutus = do
                  decoded <- decodeStrictWith json return (_json @Identity $ idWithP)
                  let plData = jsonToPlutusData decoded
                  prData <- fromData plData
                  return $
                    defMessage
                      & fsId .~ _factStatementId @Identity idWithP
                      & plutusData .~ prData
            maybe
              ( do
                  putStrLn $ "Failed formatting a fact statement with id" <> show (_factStatementId @Identity idWithP)
                  return Nothing
              )
              (return . Just)
              fsIdAndP
        )
  maybe
    ( return $
        defMessage
          & Proto.error . msg .~ "Failed processing request"
    )
    ( \idsWithPs' ->
        return $
          defMessage
            & Proto.success . fsIdsWithPlutus .~ idsWithPs'
    )
    mayIdsWithPs'
