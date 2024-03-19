{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module FactStatementStoreGrpc (factStatementStoreService, FactStatementStoreGrpcOpts (FactStatementStoreGrpcOpts)) where

import BeamConfig (FactStatementT (_factStatementId, _json), FsStore (fsTbl), fsStoreSettings)
import Cardano.Proto.Aux ()
import Control.Lens (makeLenses, (&), (.~), (^.))
import Data.Aeson (json)
import Data.Aeson.Parser (decodeStrictWith)
import Data.Functor.Identity (Identity)
import Data.List (nub)
import Data.ProtoLens (Message (defMessage))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Traversable (for)
import Database.Beam (SqlValable (val_), runSelectReturningOne)
import Database.Beam.Query (SqlEq ((==.)), all_, filter_, select)
import Database.Beam.Sqlite (runBeamSqliteDebug)
import Database.SQLite.Simple (Connection, withConnection)
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
import Proto.FactStatementStoreService (FactStatementStore, GetFactStatementRequest, GetFactStatementResponse, Success'FsIdAndPlutus)
import Proto.FactStatementStoreService_Fields (error, fsId, fsIds, fsIdsWithPlutus, msg, otherErr, plutusData, success)
import Prelude hiding (error, succ)

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
factStatementStoreService opts =
  withConnection (opts ^. db) $ \dbConn ->
    let routes :: [ServiceHandler]
        routes =
          [Server.unary (RPC :: RPC FactStatementStore "getFactStatement") (handleReq dbConn)]
     in runServer
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

type FsT = FactStatementT Identity

handleReq :: Connection -> Server.UnaryHandler IO GetFactStatementRequest GetFactStatementResponse
handleReq dbConn _ req = do
  let fsTbl' = fsTbl fsStoreSettings
      ids = nub $ req ^. fsIds

  idsWithRes :: [Either Text Success'FsIdAndPlutus] <-
    for
      ids
      ( \i -> do
          (mayFsT :: Maybe FsT) <- runBeamSqliteDebug Prelude.putStrLn dbConn $ runSelectReturningOne (select $ filter_ (\fs -> _factStatementId fs ==. val_ i) (all_ fsTbl'))
          maybe
            (return (Left $ Text.pack "Not found requested Fact Statement with ID " <> (Text.pack . show $ i)))
            ( \fs -> do
                let maySucc :: Maybe Success'FsIdAndPlutus = do
                      decoded <- decodeStrictWith json return (_json @Identity $ fs)
                      let plData = jsonToPlutusData decoded
                      prData <- fromData plData
                      return $
                        defMessage
                          & fsId .~ _factStatementId @Identity fs
                          & plutusData .~ prData

                maybe (return (Left $ Text.pack "Failed formatting to PlutusData for Fact Statement with ID: " <> (Text.pack . show $ i))) (return . Right) maySucc
            )
            mayFsT
      )

  -- If any contains an error report that
  let allSuccOrErr = sequence idsWithRes
  return $
    either
      (\err -> defMessage & error . otherErr . msg .~ err)
      (\succ -> defMessage & success . fsIdsWithPlutus .~ succ)
      allSuccOrErr
