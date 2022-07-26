{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lens.Micro ((&))
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
import Network.Wai.Handler.WarpTLS (defaultTlsSettings)
import Proto.Google.Protobuf.Empty (Empty)
import Proto.Oracle (CreateResourceTransactionRequest, CreateResourceTransactionResponse, GetCatalogResponse, GetSignatoriesResponse, Oracle)

import Data.ProtoLens (Message (defMessage))
import Prelude hiding (readFile)

main :: IO ()
main = do
  runServer
    "localhost"
    5081

runServer :: Warp.HostPreference -> Int -> IO ()
runServer h p = do
  let warpSettings =
        Warp.defaultSettings
          & Warp.setPort p
          & Warp.setHost h
  Server.runGrpc
    defaultTlsSettings
    warpSettings
    routes
    [ Encoding.uncompressed
    , Encoding.gzip
    ]

routes :: [ServiceHandler]
routes =
  [ Server.unary (RPC :: RPC Oracle "getCatalog") handleGetCatalog
  , Server.unary (RPC :: RPC Oracle "getSignatories") handleGetSignatories
  , Server.unary (RPC :: RPC Oracle "createResourceTransaction") handleCreateResourceTransaction
  ]

handleGetCatalog :: Server.UnaryHandler IO Empty GetCatalogResponse
handleGetCatalog _ _ = let resp = defMessage in pure resp

handleGetSignatories :: Server.UnaryHandler IO Empty GetSignatoriesResponse
handleGetSignatories _ _ = pure undefined

handleCreateResourceTransaction :: Server.UnaryHandler IO CreateResourceTransactionRequest CreateResourceTransactionResponse
handleCreateResourceTransaction _ _ = pure undefined
