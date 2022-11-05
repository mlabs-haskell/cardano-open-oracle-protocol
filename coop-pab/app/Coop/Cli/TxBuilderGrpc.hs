module Coop.Cli.TxBuilderGrpc (txBuilderService, TxBuilderGrpcOpts (..)) where

import Lens.Micro ((&), (.~))
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
import Plutus.V1.Ledger.Value (AssetClass)
import Plutus.V2.Ledger.Api (PubKeyHash)
import Proto.TxBuilderService (CreateGcFsTxReq, CreateGcFsTxResp, CreateMintFsTxReq, CreateMintFsTxResp, TxBuilder)

import Data.ProtoLens (Message (defMessage))
import GHC.Exts (fromString)
import Proto.TxBuilderService_Fields (msg, otherErr)
import Proto.TxBuilderService_Fields qualified as Proto.TxBuilderService

data TxBuilderGrpcOpts = TxBuilderGrpcOpts
  { tbgo'pabConfig :: FilePath
  , tbgo'coopDeploymentFile :: FilePath
  , tbgo'authWallets :: [PubKeyHash]
  , tbgo'fee :: (AssetClass, Integer)
  , tbgo'grpcAddress :: String
  , tbgo'grpcPort :: Int
  , tbgo'tlsCertFile :: FilePath
  , tbgo'tlsKeyFile :: FilePath
  }
  deriving stock (Show, Eq)

txBuilderService :: TxBuilderGrpcOpts -> IO ()
txBuilderService opts = do
  runServer
    (fromString $ tbgo'grpcAddress opts, tbgo'grpcPort opts)
    (tbgo'tlsCertFile opts, tbgo'tlsKeyFile opts)

runServer :: (Warp.HostPreference, Int) -> (FilePath, FilePath) -> IO ()
runServer (h, p) (certFile, keyFile) = do
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

routes :: [ServiceHandler]
routes =
  [ Server.unary (RPC :: RPC TxBuilder "createMintFsTx") handleCreateMintFsTx
  , Server.unary (RPC :: RPC TxBuilder "createGcFsTx") handleCreateGcFsTx
  ]

handleCreateMintFsTx :: Server.UnaryHandler IO CreateMintFsTxReq CreateMintFsTxResp
handleCreateMintFsTx _ _ =
  return $
    defMessage
      & Proto.TxBuilderService.error . otherErr . msg .~ "Finally"

handleCreateGcFsTx :: Server.UnaryHandler IO CreateGcFsTxReq CreateGcFsTxResp
handleCreateGcFsTx _ _ =
  return $
    defMessage
      & Proto.TxBuilderService.error . otherErr . msg .~ "Finally"
