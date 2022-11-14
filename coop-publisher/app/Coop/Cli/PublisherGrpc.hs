{-# LANGUAGE BlockArguments #-}

module Coop.Cli.PublisherGrpc (publisherService, PublisherGrpcOpts (..)) where

import Control.Lens (makeLenses, (&), (.~), (^.))
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (fromString)
import Network.GRPC.Client (RawReply)
import Network.GRPC.Client.Helpers (GrpcClient, GrpcClientConfig (_grpcClientConfigCompression), grpcClientConfigSimple, rawUnary, setupGrpcClient)
import Network.GRPC.HTTP2.Encoding as Encoding (
  GRPCInput,
  GRPCOutput,
  gzip,
  uncompressed,
 )
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.HTTP2.Types (IsRPC (path))
import Network.GRPC.Server as Server (
  ServiceHandler,
  UnaryHandler,
  runGrpc,
  unary,
 )
import Network.HTTP2.Client (ClientIO, HostName, PortNumber, TooMuchConcurrency, runClientIO)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS (tlsSettings)
import Proto.FactStatementStoreService (FactStatementStore, GetFactStatementResponse)
import Proto.FactStatementStoreService_Fields (fsIdsWithPlutus, plutusData, success)
import Proto.PublisherService (
  CreateGcFsTxRequest,
  CreateGcFsTxResponse,
  CreateMintFsTxRequest,
  CreateMintFsTxResponse,
  Publisher,
 )
import Proto.PublisherService qualified as PublisherService
import Proto.PublisherService_Fields (fsId, fsIds, fsInfos, fsStoreErr, gcAfter, info, maybe'error, mintFsTx, msg, otherErr, submitter, txBuilderErr, txBuilderInfo)
import Proto.PublisherService_Fields qualified as PublisherService
import Proto.TxBuilderService (FactStatementInfo, TxBuilder)
import Proto.TxBuilderService qualified as TxBuilder
import Proto.TxBuilderService_Fields (factStatements, fs)

data PublisherGrpcOpts = PublisherGrpcOpts
  { _grpcAddress :: String
  , _grpcPort :: Int
  , _tlsCertFile :: FilePath
  , _tlsKeyFile :: FilePath
  , _fsStoreAddress :: String
  , _fsStorePort :: Int
  , _txBuilderAddress :: String
  , _txBuilderPort :: Int
  }
  deriving stock (Show, Eq)

makeLenses ''PublisherGrpcOpts

publisherService :: PublisherGrpcOpts -> IO ()
publisherService opts = do
  let handleCreateMintFsTx :: Server.UnaryHandler IO CreateMintFsTxRequest CreateMintFsTxResponse
      handleCreateMintFsTx _ req = do
        print (show req)
        getFsRespOrErr <-
          call'
            (opts ^. fsStoreAddress)
            (fromInteger . toInteger $ opts ^. fsStorePort)
            (RPC :: RPC FactStatementStore "getFactStatement")
            (defMessage & fsIds .~ ((^. fsId) <$> req ^. fsInfos))
        either
          ( \err -> do
              return $ defMessage & PublisherService.error .~ err
          )
          ( \(getFsResp :: GetFactStatementResponse) -> do
              print (show getFsResp)
              case getFsResp ^. maybe'error of
                Nothing -> do
                  let fsIdToGcAfter = Map.fromList [(fsI ^. fsId, fsI ^. gcAfter) | fsI <- req ^. fsInfos]
                      fsInfos' =
                        [ (defMessage :: FactStatementInfo)
                          & fsId .~ fsI ^. fsId
                          & fs .~ fsI ^. plutusData
                          & gcAfter .~ gcAf
                        | fsI <- getFsResp ^. success . fsIdsWithPlutus
                        , gcAf <- maybe [] return $ Map.lookup (fsI ^. fsId) fsIdToGcAfter
                        ]
                  let crMintFsTxReq :: TxBuilder.CreateMintFsTxReq
                      crMintFsTxReq =
                        defMessage
                          & factStatements .~ fsInfos'
                          & submitter .~ req ^. submitter
                  print (show crMintFsTxReq)
                  createMintFsRespOrErr <-
                    call'
                      (opts ^. txBuilderAddress)
                      (fromInteger . toInteger $ opts ^. txBuilderPort)
                      (RPC :: RPC TxBuilder "createMintFsTx")
                      crMintFsTxReq
                  either
                    (\err -> return $ defMessage & PublisherService.error .~ err)
                    ( \(createMintFsResp :: TxBuilder.CreateMintFsTxResp) -> do
                        print (show createMintFsResp)
                        case createMintFsResp ^. maybe'error of
                          Nothing ->
                            return $
                              (defMessage :: CreateMintFsTxResponse)
                                & mintFsTx .~ createMintFsResp ^. success . mintFsTx
                                & info . txBuilderInfo .~ createMintFsResp ^. info
                          Just er ->
                            return $
                              (defMessage :: CreateMintFsTxResponse)
                                & PublisherService.error . txBuilderErr .~ er
                                & info . txBuilderInfo .~ createMintFsResp ^. info
                    )
                    createMintFsRespOrErr
                Just er -> return $ (defMessage :: CreateMintFsTxResponse) & PublisherService.error . fsStoreErr .~ er
          )
          getFsRespOrErr

      handleCreateGcFsTx :: Server.UnaryHandler IO CreateGcFsTxRequest CreateGcFsTxResponse
      handleCreateGcFsTx _ _req = return defMessage

      routes :: [ServiceHandler]
      routes =
        [ Server.unary (RPC :: RPC Publisher "createMintFsTx") handleCreateMintFsTx
        , Server.unary (RPC :: RPC Publisher "createGcFsTx") handleCreateGcFsTx
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

formatRpcError :: (IsRPC r, Show a) => r -> a -> Text
formatRpcError r err = (Text.pack . show . path $ r) <> (Text.pack . show $ err)

call :: (GRPCInput r i, GRPCOutput r a) => r -> GrpcClient -> i -> ClientIO (Either PublisherService.Error a)
call r grpc req = parseRet <$> rawUnary r grpc req
  where
    parseRet :: Either TooMuchConcurrency (RawReply a) -> Either PublisherService.Error a
    parseRet =
      either
        (\err -> Left $ defMessage & otherErr . msg .~ formatRpcError r err)
        ( \(rawRep :: RawReply a) ->
            either
              (\err -> Left $ defMessage & otherErr . msg .~ formatRpcError r err)
              ( \(_, _, errOrResp) ->
                  either
                    ( \err ->
                        Left $ defMessage & otherErr . msg .~ formatRpcError r err
                    )
                    return
                    errOrResp
              )
              rawRep
        )

call' :: (GRPCOutput r b, GRPCInput r i) => HostName -> PortNumber -> r -> i -> IO (Either PublisherService.Error b)
call' addr port r req = do
  ret <- runClientIO do
    cli <- mkClient addr port
    call r cli req
  return $
    either
      (\err -> Left $ defMessage & otherErr . msg .~ formatRpcError r err)
      (either Left Right)
      ret

mkClient :: HostName -> PortNumber -> ClientIO GrpcClient
mkClient host port =
  setupGrpcClient ((grpcClientConfigSimple host port True) {_grpcClientConfigCompression = uncompressed})
