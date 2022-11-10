module FactStatementStoreGrpc (factStatementStoreService, FactStatementStoreGrpcOpts (FactStatementStoreGrpcOpts)) where

import Control.Lens (makeLenses, (&), (.~), (^.))

import Data.ProtoLens (Message (defMessage))
import Data.String (IsString (fromString))
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
import Proto.FactStatementStoreService (FactStatementStore, GetFactStatementRequest, GetFactStatementResponse)
import Proto.FactStatementStoreService_Fields (msg)
import Proto.FactStatementStoreService_Fields qualified as Proto

data FactStatementStoreGrpcOpts = FactStatementStoreGrpcOpts
  { _config :: FilePath
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
        [Server.unary (RPC :: RPC FactStatementStore "getFactStatement") handleReq]

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

handleReq :: Server.UnaryHandler IO GetFactStatementRequest GetFactStatementResponse
handleReq _ _ =
  return $
    defMessage
      & Proto.error . msg .~ "Finally"
