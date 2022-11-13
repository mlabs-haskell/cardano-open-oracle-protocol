module Coop.Cli.TxBuilderGrpc (txBuilderService, TxBuilderGrpcOpts (..)) where

import Control.Lens (makeLenses, (&), (.~), (^.))
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
import BotPlutusInterface.Files (txFileName)
import BotPlutusInterface.Types (PABConfig, RawTx (_cborHex), pcOwnPubKeyHash, pcTxFileDir)
import Cardano.Proto.Aux (ProtoCardano (toCardano))
import Coop.Pab (runGcFsTx, runMintFsTx)
import Coop.Pab.Aux (runBpi)
import Coop.Types (CoopDeployment)
import Data.Aeson (decodeFileStrict)
import Data.Maybe (fromMaybe)
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import GHC.Exts (fromString)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash), TxId)
import Proto.Plutus_Fields (cborBase16)
import Proto.TxBuilderService_Fields (gcFsTx, info, mintFsTx, msg, otherErr, submitter, success)
import Proto.TxBuilderService_Fields qualified as Proto.TxBuilderService
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath ((</>))

data TxBuilderGrpcOpts = TxBuilderGrpcOpts
  { _pabConfig :: FilePath
  , _coopDeploymentFile :: FilePath
  , _authWallets :: [PubKeyHash]
  , _fee :: (PubKeyHash, AssetClass, Integer)
  , _grpcAddress :: String
  , _grpcPort :: Int
  , _tlsCertFile :: FilePath
  , _tlsKeyFile :: FilePath
  , _mintFsTxValidityMinutes :: Integer
  }
  deriving stock (Show, Eq)

makeLenses ''TxBuilderGrpcOpts

txBuilderService :: TxBuilderGrpcOpts -> IO ()
txBuilderService opts = do
  coopDeployment <- fromMaybe (error "txBuilderService: Must have a CoopDeployment file in JSON") <$> decodeFileStrict @CoopDeployment (opts ^. coopDeploymentFile)
  pabConf <- either (\err -> error $ "txBuilderService: Must have a PABConfig file in Config format: " <> err) id <$> loadPABConfig (opts ^. pabConfig)

  let (feeCollector, feeAc, feeQ) = opts ^. fee
      feeValue = assetClassValue feeAc feeQ
      authenticators = PaymentPubKeyHash <$> opts ^. authWallets
      runMintFsTxOnReq =
        runMintFsTx
          coopDeployment
          authenticators
          (feeValue, PaymentPubKeyHash feeCollector)
          (False, opts ^. mintFsTxValidityMinutes)

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
          ( \(mayTxId, info') -> do
              maybe
                ( return $
                    defMessage
                      & Proto.TxBuilderService.error . otherErr . msg .~ "Failed creating mint-fact-statement-tx"
                      & info .~ info'
                )
                ( \txId -> do
                    mayRawTx <- readSignedTx pabConf txId
                    either
                      ( \err ->
                          return $
                            defMessage
                              & Proto.TxBuilderService.error . otherErr . msg .~ ("Failed creating mint-fact-statement-tx: " <> err)
                              & info .~ info'
                      )
                      ( \rawTx ->
                          return $
                            defMessage
                              & info .~ info'
                              & Proto.TxBuilderService.error . otherErr . msg .~ "wuasapp"
                              & success . mintFsTx . cborBase16 .~ rawTx
                      )
                      mayRawTx
                )
                mayTxId
          )
          errOrAcs

      runGcFsTxOnReq =
        runGcFsTx
          coopDeployment
          False

      handleCreateGcFsTx :: Server.UnaryHandler IO CreateGcFsTxReq CreateGcFsTxResp
      handleCreateGcFsTx _ req = do
        sub <- toCardano (req ^. submitter)
        (_, errOrAcs) <-
          runBpi @Text
            pabConf
              { pcOwnPubKeyHash = sub
              }
            (runGcFsTxOnReq req)
        either
          (\err -> return $ defMessage & Proto.TxBuilderService.error . otherErr . msg .~ err)
          ( \(mayTxId, info') -> do
              maybe
                ( return $
                    defMessage
                      & Proto.TxBuilderService.error . otherErr . msg .~ "Failed creating a gc-fact-statement-tx"
                      & info .~ info'
                )
                ( \txId -> do
                    mayRawTx <- readSignedTx pabConf txId
                    either
                      ( \err ->
                          return $
                            defMessage
                              & Proto.TxBuilderService.error . otherErr . msg .~ ("Failed creating a gc-fact-statement-tx: " <> err)
                              & info .~ info'
                      )
                      ( \rawTx ->
                          return $
                            defMessage
                              & success . gcFsTx . cborBase16 .~ rawTx
                              & info .~ info'
                      )
                      mayRawTx
                )
                mayTxId
          )
          errOrAcs

      routes :: [ServiceHandler]
      routes =
        [ Server.unary (RPC :: RPC TxBuilder "createMintFsTx") handleCreateMintFsTx
        , Server.unary (RPC :: RPC TxBuilder "createGcFsTx") handleCreateGcFsTx
        ]

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

readSignedTx :: PABConfig -> TxId -> IO (Either Text Text)
readSignedTx pabConf txId = do
  txFolderPath <- makeAbsolute (unpack . pcTxFileDir $ pabConf)
  let path :: FilePath
      path = txFolderPath </> unpack (txFileName txId "signed")
  fileExists <- doesFileExist path
  if fileExists
    then do
      mayRawTx <- decodeFileStrict @RawTx path
      maybe
        ( do
            return . Left . Text.pack $ "Must have a properly formatter RawTx in Json at " <> path
        )
        (return . Right . _cborHex)
        mayRawTx
    else do
      return . Left . Text.pack $ "Must find signed transaction file at " <> path
