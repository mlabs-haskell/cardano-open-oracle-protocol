{-# LANGUAGE BlockArguments #-}

module Coop.Cli.PublisherGrpc (publisherService, PublisherGrpcOpts (..)) where

import Control.Exception (bracket)
import Control.Lens (both, makeLenses, traverseOf_, (&), (.~), (^.))
import Control.Monad (void)
import Data.Map qualified as Map
import Data.ProtoLens (Message (defMessage))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16)
import GHC.Exts (fromString)
import Network.GRPC.Client (RawReply)
import Network.GRPC.Client.Helpers (GrpcClient, GrpcClientConfig (_grpcClientConfigCompression), close, grpcClientConfigSimple, rawUnary, setupGrpcClient)
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
import Network.HTTP2.Client (ClientIO, HostName, TooMuchConcurrency, runClientIO)
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
import Proto.PublisherService_Fields (fsId, fsIds, fsInfos, fsStoreErr, gcAfter, gcFsTx, info, maybe'error, mintFsTx, msg, otherErr, submitter, txBuilderErr, txBuilderInfo)
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
  , _fsStorePort :: Word16
  , _txBuilderAddress :: String
  , _txBuilderPort :: Int
  }
  deriving stock (Show, Eq)

makeLenses ''PublisherGrpcOpts

publisherService :: PublisherGrpcOpts -> IO ()
publisherService opts =
  let setup =
        (,)
          <$> mkClient (opts ^. fsStoreAddress) (fromIntegral $ opts ^. fsStorePort)
          <*> mkClient (opts ^. txBuilderAddress) (fromIntegral $ opts ^. txBuilderPort)

      cleanup = traverseOf_ both closeClient

      serve fsStoreClient txBuilderClient =
        let handleCreateMintFsTx :: Server.UnaryHandler IO CreateMintFsTxRequest CreateMintFsTxResponse
            handleCreateMintFsTx _ req = do
              print ("Got from user: " <> show req)
              getFsRespOrErr <-
                call'
                  fsStoreClient
                  (RPC :: RPC FactStatementStore "getFactStatement")
                  (defMessage & fsIds .~ ((^. fsId) <$> req ^. fsInfos))
              either
                (\err -> return $ defMessage & PublisherService.error .~ err)
                ( \(getFsResp :: GetFactStatementResponse) -> do
                    print ("Got from FactStatementStore: " <> show getFsResp)
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
                        print ("Sending CreateMintFsTxReq to TxBuilder: " <> show crMintFsTxReq)
                        createMintFsRespOrErr <-
                          call'
                            txBuilderClient
                            (RPC :: RPC TxBuilder "createMintFsTx")
                            crMintFsTxReq
                        either
                          (\err -> return $ defMessage & PublisherService.error .~ err)
                          ( \(createMintFsResp :: TxBuilder.CreateMintFsTxResp) -> do
                              print ("Got from TxBuilder: " <> show getFsResp)
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
            handleCreateGcFsTx _ req = do
              print ("Got from user: " <> show req)
              let txBuilderReq :: TxBuilder.CreateGcFsTxReq
                  txBuilderReq =
                    defMessage
                      & fsIds .~ req ^. fsIds
                      & submitter .~ req ^. submitter
              print ("Sending CreateGcFsTxRequest to TxBuilder: " <> show txBuilderReq)
              createGcFsRespOrErr <-
                call'
                  txBuilderClient
                  (RPC :: RPC TxBuilder "createGcFsTx")
                  txBuilderReq
              either
                (\err -> return $ defMessage & PublisherService.error .~ err)
                ( \(createGcFsResp :: TxBuilder.CreateGcFsTxResp) -> do
                    print ("Got from TxBuilder: " <> show createGcFsResp)
                    case createGcFsResp ^. maybe'error of
                      Nothing ->
                        return $
                          (defMessage :: CreateGcFsTxResponse)
                            & gcFsTx .~ createGcFsResp ^. success . gcFsTx
                            & info . txBuilderInfo .~ createGcFsResp ^. info
                      Just er ->
                        return $
                          (defMessage :: CreateGcFsTxResponse)
                            & PublisherService.error . txBuilderErr .~ er
                            & info . txBuilderInfo .~ createGcFsResp ^. info
                )
                createGcFsRespOrErr

            routes :: [ServiceHandler]
            routes =
              [ Server.unary (RPC :: RPC Publisher "createMintFsTx") handleCreateMintFsTx
              , Server.unary (RPC :: RPC Publisher "createGcFsTx") handleCreateGcFsTx
              ]
         in runServer
              routes
              (fromString $ opts ^. grpcAddress, opts ^. grpcPort)
              (opts ^. tlsCertFile, opts ^. tlsKeyFile)
   in bracket setup cleanup $ uncurry serve

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

call' :: (GRPCOutput r b, GRPCInput r i) => GrpcClient -> r -> i -> IO (Either PublisherService.Error b)
call' client r req = do
  ret <- runClientIO $ call r client req
  return $
    either
      (\err -> Left $ defMessage & otherErr . msg .~ formatRpcError r err)
      (either Left Right)
      ret

mkClient :: HostName -> Word16 -> IO GrpcClient
mkClient host port = do
  result <-
    runClientIO $
      setupGrpcClient
        ( (grpcClientConfigSimple host (fromIntegral port) True)
            { _grpcClientConfigCompression = uncompressed
            }
        )

  either
    (fail . mappend "Error while connecting to grpc server: " . show)
    pure
    result

closeClient :: GrpcClient -> IO ()
closeClient = void . runClientIO . close
