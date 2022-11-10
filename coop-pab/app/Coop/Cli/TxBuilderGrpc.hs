module Coop.Cli.TxBuilderGrpc (txBuilderService, TxBuilderGrpcOpts (..)) where

import Lens.Micro ((&), (.~), (^.))
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
import Plutus.V1.Ledger.Value (AssetClass, assetClassValue)
import Plutus.V2.Ledger.Api (PubKeyHash)
import Proto.TxBuilderService (CreateGcFsTxReq, CreateGcFsTxResp, CreateMintFsTxReq, CreateMintFsTxResp, TxBuilder)

import BotPlutusInterface.Config (loadPABConfig)
import BotPlutusInterface.Types (pcOwnPubKeyHash)
import Cardano.Proto.Aux (ProtoCardano (toCardano))
import Coop.Pab (runMintFsTx)
import Coop.Pab.Aux (runBpi)
import Coop.Types (CoopDeployment)
import Data.Aeson (decodeFileStrict)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import GHC.Exts (fromString)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash))
import Proto.TxBuilderService_Fields (alreadyPublished, mintFsSuccess, msg, otherErr, submitter)
import Proto.TxBuilderService_Fields qualified as Proto.TxBuilderService

data TxBuilderGrpcOpts = TxBuilderGrpcOpts
  { tbgo'pabConfig :: FilePath
  , tbgo'coopDeploymentFile :: FilePath
  , tbgo'authWallets :: [PubKeyHash]
  , tbgo'fee :: (PubKeyHash, AssetClass, Integer)
  , tbgo'grpcAddress :: String
  , tbgo'grpcPort :: Int
  , tbgo'tlsCertFile :: FilePath
  , tbgo'tlsKeyFile :: FilePath
  }
  deriving stock (Show, Eq)

txBuilderService :: TxBuilderGrpcOpts -> IO ()
txBuilderService opts = do
  coopDeployment <- fromMaybe (error "txBuilderService: Must have a CoopDeployment file in JSON") <$> decodeFileStrict @CoopDeployment (tbgo'coopDeploymentFile opts)
  pabConf <- either (\err -> error $ "txBuilderService: Must have a PABConfig file in Config format: " <> err) id <$> loadPABConfig (tbgo'pabConfig opts)

  let (feeCollector, feeAc, feeQ) = tbgo'fee opts
      feeValue = assetClassValue feeAc feeQ
      authenticators = PaymentPubKeyHash <$> tbgo'authWallets opts
      runMintFsTxOnReq = runMintFsTx coopDeployment authenticators (feeValue, PaymentPubKeyHash feeCollector)

      handleCreateMintFsTx :: Server.UnaryHandler IO CreateMintFsTxReq CreateMintFsTxResp
      handleCreateMintFsTx _ req = do
        sub <- toCardano (req ^. submitter)
        (_, errOrAcs) <-
          runBpi @Text
            pabConf
              { pcOwnPubKeyHash = sub
              }
            (runMintFsTxOnReq req)
        either
          (\err -> return $ defMessage & Proto.TxBuilderService.error . otherErr . msg .~ err)
          (\alreadyPublished' -> return $ defMessage & mintFsSuccess . alreadyPublished .~ alreadyPublished')
          errOrAcs

      routes :: [ServiceHandler]
      routes =
        [ Server.unary (RPC :: RPC TxBuilder "createMintFsTx") handleCreateMintFsTx
        , Server.unary (RPC :: RPC TxBuilder "createGcFsTx") handleCreateGcFsTx
        ]

  runServer
    routes
    (fromString $ tbgo'grpcAddress opts, tbgo'grpcPort opts)
    (tbgo'tlsCertFile opts, tbgo'tlsKeyFile opts)

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

handleCreateGcFsTx :: Server.UnaryHandler IO CreateGcFsTxReq CreateGcFsTxResp
handleCreateGcFsTx _ _ =
  return $
    defMessage
      & Proto.TxBuilderService.error . otherErr . msg .~ "Finally"
