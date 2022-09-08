{-# LANGUAGE BlockArguments #-}

module Coop.Plutus (
  mkFsMp,
  fsV,
  mkAuthMp,
  certV,
  mkCertMp,
) where

import Control.Monad.Fail (MonadFail (fail))
import Coop.Plutus.Aux (pboolC, pcurrencyValue, pdatumFromTxOut, pdjust, pdnothing, pfindMap, pfindOwnInput', pfoldTxInputs, pfoldTxOutputs, phasCurrency, pmaybeDataC, pmustBeSignedBy, pmustHandleSpentWithMp, pmustMint, pmustPayTo, pmustSpendAtLeast, pmustValidateAfter, pownCurrencySymbol, ptryFromData, punit)
import Coop.Plutus.Types (PAuthMpParams, PAuthMpRedeemer (PAuthMpBurn, PAuthMpMint), PAuthParams, PCertDatum, PCertMpParams, PCertMpRedeemer (PCertMpBurn, PCertMpMint), PFsDatum, PFsMpParams, PFsMpRedeemer (PFsMpBurn, PFsMpMint))
import Plutarch (POpaque, popaque)
import Plutarch.Api.V1 (AmountGuarantees (NonZero, Positive), KeyGuarantees (Sorted), PCurrencySymbol, PMap (PMap), PMaybeData, PMintingPolicy, PScriptContext, PTxInInfo, PTxOut, PValidator, PValue)
import Plutarch.Api.V1.Value (PTokenName (PTokenName), passertPositive, pnormalize, pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Bool (pif)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.List (pmap)
import Plutarch.Num (PNum (pnegate, (#+)))
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList, PByteString, PEq ((#==)), PInteger, PListLike (pcons, pnil), PPair (PPair), PPartialOrd ((#<), (#<=)), Term, pcon, pconsBS, pconstant, pfield, pfoldl, pfromData, pfstBuiltin, phoistAcyclic, plam, plet, psha3_256, pto, (#), (#$), type (:-->))
import Plutarch.TermCont (unTermCont)
import PlutusTx.Prelude (Group (inv))
import Prelude (Applicative (pure), Monad ((>>), (>>=)), Monoid (mempty), Semigroup ((<>)), ($))

-- TODO; Use PPair instead of PTuple, nicer syntax

{- | FsV delegates $FS spending validation to FsMp.

- check that MP of the input token is also in trx mint

NOTE: The validation is delegated to the $FS MP
-}
fsV :: ClosedTerm PValidator
fsV = phoistAcyclic $
  plam $ \_ _ ctx -> unTermCont do
    ptraceC "@FsV"

    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["mint"] ctx'.txInfo

    ownIn <- pletC $ pfindOwnInput' # ctx
    ownVal <- pletC $ pfromData $ pfield @"value" # (pfield @"resolved" # ownIn)

    pmatchC (pto ownVal) >>= \case
      PMap elems ->
        pure $
          pmap
            # plam
              ( \kv -> unTermCont do
                  cs <- pletC $ pfromData $ pfstBuiltin # kv
                  pboolC
                    (fail "@FsV: Spent currency symbol must be in mint")
                    (ptraceC "@Fsv: Spent currency symbol in mint" >> pure punit)
                    (phasCurrency # cs # txInfo.mint)
              )
            # elems
    ptraceC "@FsV: All spent currency symbols are in mint"

    pure $ popaque $ pconstant ()

-- | Minting policy that validates minting and burning of $FS tokens
mkFsMp :: ClosedTerm (PAsData PFsMpParams :--> PMintingPolicy)
mkFsMp = phoistAcyclic $
  plam $ \params red ctx -> unTermCont do
    ptraceC "FsMp"

    red' <- pletC $ pfromData (ptryFromData @PFsMpRedeemer red)

    pmatchC red' >>= \case
      PFsMpBurn _ -> pure $ fsMpBurn # pfromData params # ctx
      PFsMpMint _ -> pure $ fsMpMint # pfromData params # ctx

{- | Validates burning $FS tokens

- check each input and accumulate $FS tokens
  - skip if doesn't hold $FS token
  - check the trx is signed by the submitter as indicated in the FsDatum
  - check that the trx validates after time as indicated in the FsDatum
  - accumulate validated $FS token
- check that all accumulated spent $FS tokens are burned

NOTE: FsV delegates spending validation to FsMp
-}
fsMpBurn :: ClosedTerm (PFsMpParams :--> PScriptContext :--> POpaque)
fsMpBurn = phoistAcyclic $
  plam $ \_ ctx -> unTermCont do
    ptraceC "FsMp burn"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose
    minted <- pletC $ pfield @"mint" # ctx'.txInfo

    let foldFn shouldBurn txInInfo = unTermCont do
          txOut <- pletC $ pfield @"resolved" # txInInfo
          txIn <- pletFieldsC @'["value", "address"] $ pfield @"resolved" # txInInfo
          fsDatum <- pletFieldsC @'["fd'submitter", "fd'gcAfter", "fd'fsId"] $ pdatumFromTxOut @PFsDatum # ctx # txOut

          pboolC
            (ptraceC "FsMp burn: Skipping foreign input" >> pure shouldBurn)
            ( do
                ptraceC "FsMp burn: Found own input"

                _ <- pletC $ pmustBeSignedBy # ctx # fsDatum.fd'submitter
                ptraceC "FsMp burn: Submitter signed"

                _ <- pletC $ pmustValidateAfter # ctx # fsDatum.fd'gcAfter
                ptraceC "FsMp burn: Time validates"

                ownSpent <- pletC $ pcurrencyValue # ownCs # txIn.value
                pure $ shouldBurn <> inv ownSpent
            )
            (phasCurrency # ownCs # txIn.value)

    -- Contains negative quantities
    shouldBurnTotal <- pletC $ pfoldTxInputs # ctx # plam foldFn # mempty
    ownMinted <- pletC $ pcurrencyValue # ownCs # minted
    _ <- pletC $ ownMinted #== shouldBurnTotal
    ptraceC "FsMp burn: $FS spent are valid and burned"

    pure $ popaque $ pconstant ()

fsMpMint :: ClosedTerm (PFsMpParams :--> PScriptContext :--> POpaque)
fsMpMint = phoistAcyclic $
  plam $ \params ctx -> unTermCont do
    ptraceC "FsMp mint"

    validAuthInputs <- pletC $ caValidateAuthInputs # (pfield @"fmp'authParams" # params) # ctx
    ptraceC "FsMp mint: Validated authentication inputs"

    ctx' <- pletFieldsC @'["purpose"] ctx
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose
    fsMintParseOutput' <- pletC $ fsMintParseOutput # params # ctx # ownCs

    restAuths <- pletC $ pfoldTxOutputs # ctx # fsMintParseOutput' # validAuthInputs
    ptraceC "FsMp mint: Validated and authenticated Fact Statement outputs"

    pboolC
      (fail "FsMp mint: Auth inputs must ALL be used")
      (ptraceC "FsMp mint: Auth inputs are all used")
      (restAuths #== pnil)

    -- TODO: Check that the total $FS pain is minted
    pure $ popaque $ pconstant ()

fsMintParseOutput ::
  Term
    s
    ( PFsMpParams
        :--> PScriptContext
        :--> PCurrencySymbol
        :--> PBuiltinList PTxInInfo
        :--> PTxOut
        :--> PBuiltinList PTxInInfo
    )
fsMintParseOutput = phoistAcyclic $
  plam $ \params ctx ownCs validAuthInputs txOut -> unTermCont do
    ptraceC "fsMintParseOutput"
    txOut' <- pletFieldsC @'["value"] txOut

    hasOwnCs <- pletC $ phasCurrency # ownCs # txOut'.value
    pboolC
      ( do
          ptraceC "fsMintParseOutput: Skipping foreign output"
          pure validAuthInputs
      )
      ( do
          ptraceC "fsMintParseOutput: Found own token in the output"
          pure $ fsMintParseOutputWithFs # params # ctx # ownCs # validAuthInputs # txOut
      )
      hasOwnCs

{- | Handles a transaction output that holds an $FS token.

- check that 1 $FS is minted and associated with a valid $AUTH
  - $FS token name is formed by hashing TxOutRef+Num of the associated $AUTH input
- check that 1 $FS is sent to FsV

The $AUTH input is emitted only if it's been validated.

NOTE: Fails hard
-}
fsMintParseOutputWithFs ::
  Term
    s
    ( PFsMpParams
        :--> PScriptContext
        :--> PCurrencySymbol
        :--> PBuiltinList PTxInInfo
        :--> PTxOut
        :--> PBuiltinList PTxInInfo
    )
fsMintParseOutputWithFs = phoistAcyclic $
  plam $ \params ctx ownCs validAuthInputs txOut -> unTermCont do
    ptraceC "fsMintParseOutputWithFs"
    txOut' <- pletFieldsC @'["value", "address"] txOut
    outVal <- pletC $ pnormalize # txOut'.value
    outAddr <- pletC $ txOut'.address

    pboolC
      (fail "fsMintParseOutputWithFs: Output must be sent to FsV")
      (ptraceC "fsMintParseOutputWithFs: Output sent to FsV")
      (outAddr #== (pfield @"fmp'fsVAddress" # params))

    ownValue <- pletC $ pcurrencyValue # ownCs # outVal

    let matchWithAuth mayFsTnAndAuths authInput =
          unTermCont do
            PPair mayFsTn restAuthInputs <- pmatchC mayFsTnAndAuths

            pmaybeDataC
              ( do
                  -- NOTE: Here we create a UNIQUE $FS token name
                  fsTn <- pletC $ ptokenNameFromTxInInfo # authInput
                  pure $
                    pif -- NOTE: Only outputs a single $FS token!
                      (ownValue #== (PValue.psingleton # ownCs # fsTn # 1))
                      (pcon $ PPair (pdjust fsTn) restAuthInputs)
                      (pcon $ PPair pdnothing (pcons # authInput # restAuthInputs))
              )
              (\_ -> pure mayFsTnAndAuths)
              mayFsTn

    PPair mayFsTn restAuthInputs <-
      pmatchC $
        pfoldl
          # plam matchWithAuth
          # pcon
            ( PPair
                (pdnothing :: Term _ (PMaybeData PTokenName))
                (pnil :: Term _ (PBuiltinList PTxInInfo))
            )
          # validAuthInputs

    pmaybeDataC
      (fail "fsMintParseOutputWithFs: $FS must have a token name formed from a matching input with $AUTH")
      ( \fsTn -> do
          ptraceC "fsMintParseOutputWithFs: $FS validated"
          -- NOTE: This check is sufficient as $FS are unique
          _ <- pletC $ pmustMint # ctx # ownCs # fsTn # 1
          ptraceC "fsMintParseOutputWithFs: $FS minted"
      )
      mayFsTn

    pure restAuthInputs

{- | ptokenNameFromTxInInfo creates a unique TokenName from the given transaction input

TokenName $ sha3_256 (refId `concat` num)
-}
ptokenNameFromTxInInfo :: Term s (PTxInInfo :--> PTokenName)
ptokenNameFromTxInInfo = phoistAcyclic $
  plam $ \inInfo -> unTermCont do
    txId <- pletC $ pfield @"_0" # (pfield @"id" # (pfield @"outRef" # inInfo))
    -- TODO: Check that txIDx < 256
    txIdx <- pletC $ pfield @"idx" # (pfield @"outRef" # inInfo)
    pure $ pcon $ PTokenName $ psha3_256 # (pconsBS # txIdx # txId)

{- | FsMp authentication

\$AUTH inputs are checked for validity against their corresponding $CERT input.

\$CERT is an NFT, $AUTH is an FT issued in batches that are associated with the same $CERT
-}
caValidateAuthInputs ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PTxInInfo
    )
caValidateAuthInputs = phoistAcyclic $
  plam $ \params ctx -> unTermCont $ do
    validCerts <- pletC $ caParseRefs # params # ctx
    pure $ caParseInputs # params # ctx # validCerts

caParseInputs ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PBuiltinList PTxInInfo
    )
caParseInputs = phoistAcyclic $
  plam $ \params ctx certs -> unTermCont $ do
    ptraceC "caParseInputs"
    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["inputs", "mint"] ctx'.txInfo

    PPair validAuthInputs authTokensToBurn <-
      pmatchC $
        pfoldTxInputs
          # ctx
          # (caParseInput # params # ctx # certs)
          # pcon (PPair pnil mempty)

    -- Contains negative quantities
    authMinted <- pletC $ pcurrencyValue # (pfield @"ap'authTokenCs" # params) # txInfo.mint
    pboolC
      (fail "caParseInputs: Spent $AUTH tokens must be burned")
      (ptraceC "caParseInputs: Spent $AUTH tokens were burned" >> pure validAuthInputs)
      (authMinted #== authTokensToBurn)

caParseInput ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PPair (PBuiltinList PTxInInfo) (PValue 'Sorted 'NonZero)
        :--> PTxInInfo
        :--> PPair (PBuiltinList PTxInInfo) (PValue 'Sorted 'NonZero)
    )
caParseInput = phoistAcyclic $
  plam $ \params ctx certs acc txIn -> unTermCont do
    ptraceC "caParseInput"
    txInOut <- pletC $ pfield @"resolved" # txIn
    txInVal <- pletC $ pnormalize # (pfield @"value" # txInOut)

    authTokenCs <- pletC $ pfield @"ap'authTokenCs" # params
    hasAuthCs <- pletC $ phasCurrency # authTokenCs # txInVal
    pboolC
      ( do
          ptraceC "caParseInput: Skipping non $AUTH input"
          pure acc
      )
      ( do
          ptraceC "caParseInput: Found $AUTH token in the input"
          pure $ caParseInputWithAuth # ctx # authTokenCs # certs # acc # txIn
      )
      hasAuthCs

{- | Handles a transaction input that holds an $AUTH token.

- check that there's exactly one $AUTH token that's associated with a valid Certificate
- accumulates the $AUTH token to burn

NOTE: Fails hard
-}
caParseInputWithAuth ::
  Term
    s
    ( PScriptContext
        :--> PCurrencySymbol
        :--> PBuiltinList PCertDatum
        :--> PPair (PBuiltinList PTxInInfo) (PValue 'Sorted 'NonZero)
        :--> PTxInInfo
        :--> PPair (PBuiltinList PTxInInfo) (PValue 'Sorted 'NonZero)
    )
caParseInputWithAuth = phoistAcyclic $
  plam $ \_ authTokenCs certs acc txIn -> unTermCont do
    ptraceC "caParseInputWithAuth"
    txInOut <- pletC $ pfield @"resolved" # txIn
    txInVal <- pletC $ pnormalize # (pfield @"value" # txInOut)

    authValue <- pletC $ pcurrencyValue # authTokenCs # txInVal

    mayAuthTn <-
      pletC $
        pfindMap
          # plam
            ( \certDat ->
                plet
                  (pcon (PTokenName $ pfield @"cert'id" # certDat)) -- TODO: Perhaps use Value?
                  ( \authTn ->
                      pif
                        (authValue #== (PValue.psingleton # authTokenCs # authTn # 1)) -- NOTE: Only contains a single $AUTH token!
                        pdnothing
                        (pdjust authTn)
                  )
            )
          # certs

    pmaybeDataC
      (fail "caParseInputWithAuth: $AUTH must be validated with a $CERT")
      ( \authTn -> do
          ptraceC "caParseInputWithAuth: $AUTH valid"
          PPair validAuthInputs shouldBeBurned <- pmatchC acc
          validAuthInputs' <- pletC $ pcons # txIn # validAuthInputs
          shouldBeBurned' <- pletC $ shouldBeBurned <> PValue.psingleton # authTokenCs # authTn # (pnegate # 1)
          pure $ pcon $ PPair validAuthInputs' shouldBeBurned'
      )
      mayAuthTn

caParseRefs ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
    )
caParseRefs = phoistAcyclic $
  plam $ \params ctx -> unTermCont $ do
    -- TODO: Migrate to reference inputs
    ptraceC "caParseRefs"
    pure $ pfoldTxInputs # ctx # (caParseRef # params # ctx) # pnil

caParseRef ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PTxInInfo
        :--> PBuiltinList PCertDatum
    )
caParseRef = phoistAcyclic $
  plam $ \params ctx acc txInInfo -> unTermCont do
    ptraceC "caParseRef"
    txInOut <- pletC $ pfield @"resolved" # txInInfo
    txInVal <- pletC $ pfield @"value" # txInOut
    certTokenCs <- pletC $ pfield @"ap'certTokenCs" # params
    certVal <- pletC $ pcurrencyValue # certTokenCs # txInVal
    pboolC
      ( do
          ptraceC "caParseRef: Skipping non $CERT input"
          pure acc
      )
      ( do
          ptraceC "caParseRef: Found $CERT input"
          pure $ caParseRefWithCert # ctx # certTokenCs # (passertPositive # certVal) # acc # txInInfo
      )
      (certVal #== mempty)

{- | Handles a transaction input that holds an $CERT token.

- check that the transaction's validation range is contained withing the Certificate's validity range
- check that the Certificate ref input has 1 $CERT token with appropriate ID (TokenName)
- accumulate valid CertDatum

NOTE: Fails hard
-}
caParseRefWithCert ::
  Term
    s
    ( PScriptContext
        :--> PCurrencySymbol
        :--> PValue 'Sorted 'Positive
        :--> PBuiltinList PCertDatum
        :--> PTxInInfo
        :--> PBuiltinList PCertDatum
    )
caParseRefWithCert = phoistAcyclic $
  plam $ \ctx certTokenCs certVal acc txInInfo -> unTermCont do
    ptraceC "caParseRefWithCert"
    txIn <- pletC $ pfield @"resolved" # txInInfo

    certDat' <- pletC $ (pdatumFromTxOut @PCertDatum) # ctx # txIn
    certDat <- pletFieldsC @'["cert'validity", "cert'id"] certDat'

    pboolC
      (fail "caParseRefWithCert: $CERT token name must match CertDatum ID")
      (ptraceC "caParseRefWithCert: $CERT token name matched CertDatum ID")
      (pvalueOf # certVal # certTokenCs # pcon (PTokenName certDat.cert'id) #== 1)

    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["validRange"] ctx'.txInfo
    pboolC
      (fail "caParseRefWithCert: cert is invalid")
      (ptraceC "caParseRefWithCert: cert is valid")
      (pcontains # certDat.cert'validity # txInfo.validRange)

    pure $ pcons # certDat' # acc

{- | Authentication scripts

Authentication works with 3 different tokens, namely $AA, $AUTH and $CERT.

\$AA is the Authentication Authority token that is owned by the 'admin' and is
where the security of the entire system relies on.

\$AUTH is the Authentication Grant token that is minted only if authorized by the
\$AA holder. These tokens can be minted in batches and sent to an operational
(ie. hot) wallet.

\$CERT is a Certificate token that is associated with $AUTH tokens via AUTH ID
and it's sole purpose is to hold some information about $AUTH that the
vertifying scripts can MUST use a reference input to validate $AUTH inputs.
-}

-- | WIP
certV :: ClosedTerm PValidator
certV = phoistAcyclic $
  plam $ \_ _ ctx -> unTermCont do
    ptraceC "@CertV"

    _ <- pletC $ pmustHandleSpentWithMp # ctx
    ptraceC "@CertV: Spending delegated to AuthMp"

    pure $ popaque $ pconstant ()

mkCertMp :: ClosedTerm (PAsData PCertMpParams :--> PMintingPolicy)
mkCertMp = phoistAcyclic $
  plam $ \params red ctx -> unTermCont do
    ptraceC "CertMp"

    red' <- pletC $ pfromData (ptryFromData @PCertMpRedeemer red)

    pmatchC red' >>= \case
      PCertMpBurn _ -> pure $ certMpBurn # ctx
      PCertMpMint _ -> pure $ certMpMint # pfromData params # ctx

certMpBurn :: ClosedTerm (PScriptContext :--> POpaque)
certMpBurn = phoistAcyclic $
  plam $ \ctx -> unTermCont do
    ptraceC "CertMp burn"

    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose

    let foldFn acc txInInfo = unTermCont do
          txIn <- pletC $ pfield @"resolved" # txInInfo
          txInVal <- pletC $ pfield @"value" # txIn
          pboolC
            (ptraceC "CertMp burn: Skipping foreign input" >> pure acc)
            ( do
                ptraceC "CertMp burn: Found own input"
                certDatum' <- pletC $ pdatumFromTxOut @PCertDatum # ctx # txIn
                certDatum <-
                  pletFieldsC
                    @'[ "cert'id"
                      , "cert'validity"
                      , "cert'redeemerAc"
                      ]
                    $ certDatum'
                ptraceC "CertMp burn: Parse the CertDatum"

                -- TODO: _ <- pletC $ pmustValidateAfter # ctx # (pfield @"to" # certDatum.cert'validity)
                ptraceC "CertMp burn: Can collect"

                redeemerAc <- pletFieldsC @'["_0", "_1"] certDatum.cert'redeemerAc

                _ <- pletC $ pmustSpendAtLeast # ctx # redeemerAc._0 # redeemerAc._1 # 1
                ptraceC "CertMp burn: At least 1 $CERT_RDMR spent"

                certTn <- pletC $ pcon (PTokenName $ certDatum.cert'id)
                pboolC
                  (fail "CertMp burn: Must spend a single $CERT token")
                  (ptraceC "CertMp burn: Spent a single $CERT token")
                  (pvalueOf # txInVal # ownCs # certTn #== 1)

                _ <- pletC $ pmustMint # ctx # ownCs # certTn # (pnegate # 1)
                ptraceC "CertMp burn: $CERT spent and burned"

                pure acc
            )
            (phasCurrency # ownCs # txInVal)

    _ <- pletC $ pfoldTxInputs # ctx # plam foldFn # punit
    pure $ popaque $ pconstant ()

-- | Minting $CERT tokens
certMpMint :: ClosedTerm (PCertMpParams :--> PScriptContext :--> POpaque)
certMpMint = phoistAcyclic $
  plam $ \params ctx -> unTermCont do
    ptraceC "CertMp"
    ptraceC "CertMp mint"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose
    certParams <- pletFieldsC @'["cmp'authAuthorityAc", "cmp'requiredAtLeastAaQ", "cmp'certVAddress"] params
    aaCs <- pletC $ pfield @"_0" # certParams.cmp'authAuthorityAc
    aaTn <- pletC $ pfield @"_1" # certParams.cmp'authAuthorityAc

    tnBytes <- pletC $ pmustSpendAtLeastAa # ctx # aaCs # aaTn # certParams.cmp'requiredAtLeastAaQ
    ptraceC "CertMp mint: Spent at least a specified quanitity of $AA tokens"

    certTn <- pletC $ pcon $ PTokenName tnBytes
    _ <- pletC $ pmustMint # ctx # ownCs # certTn # 1
    ptraceC "CertMp mint: Minted 1 $CERT"
    -- TODO: verify datum
    _ <- pletC $ pmustPayTo # ctx # ownCs # certTn # 1 # certParams.cmp'certVAddress
    ptraceC "CertMp mint: Paid 1 $CERT to @CertV"

    pure $ popaque $ pconstant ()

mkAuthMp :: ClosedTerm (PAsData PAuthMpParams :--> PMintingPolicy)
mkAuthMp = phoistAcyclic $
  plam $ \params red ctx -> unTermCont do
    ptraceC "AuthMp"

    red' <- pletC $ pfromData (ptryFromData @PAuthMpRedeemer red)

    pmatchC red' >>= \case
      PAuthMpBurn _ -> pure $ authMpBurn # ctx
      PAuthMpMint _ -> pure $ authMpMint # pfromData params # ctx

{- | Validates minting of $AUTH tokens

- check that at least N $AA tokens are spent
- hash spent $AA inputs and create a unique token name for $AUTH token
- check if M $AUTH tokens with the same unique token name have been minted and paid

NOTE: One transaction can yield ($AUTH, <unique_token_name>, M)
-}
authMpMint :: ClosedTerm (PAuthMpParams :--> PScriptContext :--> POpaque)
authMpMint = phoistAcyclic $
  plam $ \params ctx -> unTermCont do
    ptraceC "AuthMp mint"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose
    minted <- pletC $ pfield @"mint" # ctx'.txInfo
    authParams <- pletFieldsC @'["amp'authAuthorityAc", "amp'requiredAtLeastAaQ", "amp'certVAddress"] params
    aaCs <- pletC $ pfield @"_0" # authParams.amp'authAuthorityAc
    aaTn <- pletC $ pfield @"_1" # authParams.amp'authAuthorityAc

    tnBytes <- pletC $ pmustSpendAtLeastAa # ctx # aaCs # aaTn # authParams.amp'requiredAtLeastAaQ
    authTn <- pletC $ pcon $ PTokenName tnBytes
    _ <- pletC $ 0 #< (pvalueOf # minted # ownCs # authTn)

    pure $ popaque $ pconstant ()

{- | Validates burning of $AUTH tokens

- accumulate all spent $AUTH tokens and check if all are burned

NOTE: $AUTH tokens can be burned freely
-}
authMpBurn :: ClosedTerm (PScriptContext :--> POpaque)
authMpBurn = phoistAcyclic $
  plam $ \ctx -> unTermCont do
    ptraceC "AuthMp burn $AUTH"

    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    minted <- pletC $ pfield @"mint" # ctx'.txInfo
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose

    let foldFn shouldBurn txInInfo = unTermCont do
          txIn <- pletC $ pfield @"resolved" # txInInfo
          txInVal <- pletC $ pfield @"value" # txIn
          pboolC
            (ptraceC "AuthMp burn $AUTH: Skipping foreign input" >> pure shouldBurn)
            ( do
                ptraceC "AuthMp burn $AUTH: Found own input"

                ownSpent <- pletC $ pcurrencyValue # ownCs # txInVal
                pure $ shouldBurn <> inv ownSpent
            )
            (phasCurrency # ownCs # txInVal)

    shouldBurnTotal <- pletC $ pfoldTxInputs # ctx # plam foldFn # mempty
    ownMinted <- pletC $ pcurrencyValue # ownCs # minted
    _ <- pletC $ ownMinted #== shouldBurnTotal

    pure $ popaque $ pconstant ()

{- | Checks for total spent $AA tokens

- accumulate all spent $AA tokens and check if totals are at least as specified
- create unique bytestring from $AA inputs by hashing the concatenation of (idx,id) pairs
-}
pmustSpendAtLeastAa :: ClosedTerm (PScriptContext :--> PCurrencySymbol :--> PTokenName :--> PInteger :--> PByteString)
pmustSpendAtLeastAa = phoistAcyclic $
  plam $ \ctx aaCs aaTn atLeastAaQ -> unTermCont do
    ptraceC "pmustSpendAtLeastAa"
    let foldFn acc txIn = unTermCont do
          -- check if $AA input
          txIn' <- pletFieldsC @'["resolved", "outRef"] txIn
          txInVal <- pletC $ pfield @"value" # txIn'.resolved
          pboolC
            (ptraceC "pmustSpendAtLeastAa: Skipping non $AA input" >> pure acc)
            ( do
                ptraceC "pmustSpendAtLeastAa: Found an $AA input"
                PPair aaVal tnBytes <- pmatchC acc
                -- accumulate token name bytes
                txId <- pletC $ pfield @"_0" #$ pfield @"id" # txIn'.outRef
                txIdx <- pletC $ pfield @"idx" # txIn'.outRef
                tnBytes' <- pletC $ tnBytes <> pconsBS # txIdx # txId
                -- accumulate token quantities
                aaVal' <- pletC $ aaVal #+ (pvalueOf # txInVal # aaCs # aaTn)

                pure $ pcon $ PPair aaVal' tnBytes'
            )
            (phasCurrency # aaCs # txInVal)

    PPair aaTokensSpent tnBytes <- pmatchC $ pfoldTxInputs # ctx # plam foldFn # pcon (PPair 0 mempty)

    pboolC
      (fail "pmustSpendAtLeastAa: Must spend at least the specified amount of AA tokens")
      (ptraceC "pmustSpendAtLeastAa: Spent at least the specified amount of AA tokens")
      (atLeastAaQ #<= aaTokensSpent)

    pure $ psha3_256 # tnBytes
