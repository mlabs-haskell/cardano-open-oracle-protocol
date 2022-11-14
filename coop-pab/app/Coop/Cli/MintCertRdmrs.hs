module Coop.Cli.MintCertRdmrs (MintCertRdmrsOpts (..), mintCertRdmrs) where

import BotPlutusInterface.Config (loadPABConfig)
import BotPlutusInterface.Types (PABConfig (pcOwnPubKeyHash))

import Codec.Serialise (writeFileSerialise)
import Coop.Pab (mintCertRedeemers)
import Coop.Pab.Aux (DeployMode, loadCoopPlutus, runBpi)
import Data.Text (Text)
import Plutus.V2.Ledger.Api (PubKeyHash)

data MintCertRdmrsOpts = MintCertRdmrsOpts
  { mcro'mode :: DeployMode
  , mcro'pabConfig :: FilePath
  , mcro'certRdmrWalletPkh :: PubKeyHash
  , mcro'nCertRdmrTokens :: Integer
  , mcro'certRdmrAcFile :: FilePath
  }
  deriving stock (Show, Eq)

mintCertRdmrs :: MintCertRdmrsOpts -> IO ()
mintCertRdmrs opts = do
  coopPlutus <- loadCoopPlutus (mcro'mode opts)

  pabConf <- either (\err -> error $ "mintCertRdmrs: Must have a PABConfig file in Config format: " <> err) id <$> loadPABConfig (mcro'pabConfig opts)

  (_, errOrAcs) <-
    runBpi @Text
      pabConf
        { pcOwnPubKeyHash = mcro'certRdmrWalletPkh opts
        }
      $ mintCertRedeemers coopPlutus (mcro'nCertRdmrTokens opts)
  either
    (\err -> error $ "mintCertRdmrs: Must have $CERT-RDMR asset class" <> show err)
    ( \certRdmrAc -> do
        putStrLn $ "mintCertRdmrs: Minted $CERT-RDMR tokens with AssetClass " <> show certRdmrAc
        -- FIXME: Why is it complaining NOW about the OverlappingInstances when using Aeson?
        writeFileSerialise (mcro'certRdmrAcFile opts) certRdmrAc
    )
    errOrAcs
  return ()
