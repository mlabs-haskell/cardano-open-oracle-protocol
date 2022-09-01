{-# LANGUAGE BlockArguments #-}

module Coop.Plutus (
  mkFsMp,
  mkFsV,
  mkAuthMp,
  certV,
) where

import Control.Monad.Fail (MonadFail (fail))
import Coop.Plutus.Aux (pboolC, pcurrencyTokens, pdatumFromTxOut, pdjust, pdnothing, pfindMap, pfindOwnInput', pfoldTxInputs, pfoldTxOutputs, phasCurrency, pmaybeDataC, pmustBeSignedBy, pmustHandleSpentWithMp, pmustMint, pmustPayTo, pmustSpend, pmustValidateAfter, pownCurrencySymbol, ptryFromData, punit)
import Coop.Plutus.Types (PAuthMpParams, PAuthParams, PCertDatum, PFsDatum, PFsMpParams, PFsMpRedeemer (PFsMpBurn, PFsMpMint), PFsVParams)
import Plutarch (POpaque, popaque)
import Plutarch.Api.V1 (AmountGuarantees (NonZero), KeyGuarantees (Sorted), PCurrencySymbol, PMap (PMap), PMaybeData, PMintingPolicy, PScriptContext, PTuple, PTxInInfo, PTxOut, PValidator, PValue, ptuple)
import Plutarch.Api.V1.AssocMap (plookup, psingleton)
import Plutarch.Api.V1.Value (PTokenName (PTokenName), pnoAdaValue, pnormalize, pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Bool (pif)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Extra.TermCont (pletC, pletFieldsC, pmatchC, ptraceC)
import Plutarch.List (pmap)
import Plutarch.Num (PNum (pnegate, (#+)))
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList, PEq ((#==)), PListLike (pcons, pnil), PMaybe (PJust, PNothing), PPartialOrd ((#<)), Term, getField, pcon, pconsBS, pconstant, pdata, pfield, pfoldl, pfromData, pfstBuiltin, phoistAcyclic, plam, plet, psha3_256, pto, (#), (#$), type (:-->))
import Plutarch.TermCont (unTermCont)
import Prelude (Applicative (pure), Monad ((>>), (>>=)), Monoid (mempty), Semigroup ((<>)), ($))

{- | FsV delegates $FS spending validation to FsMp.

- check that MP of the input token is also in trx mint

NOTE: The validation is delegated to the $FS MP
-}
mkFsV :: ClosedTerm (PAsData PFsVParams :--> PValidator)
mkFsV = phoistAcyclic $
  plam $ \_ _ _ ctx -> unTermCont do
    ptraceC "@FsV"

    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["mint"] ctx'.txInfo
    mint <- pletC $ pto $ pfromData txInfo.mint

    ownIn <- pletC $ pfindOwnInput' # ctx
    ownVal <- pletC $ pfromData $ pfield @"value" # (pfield @"resolved" # ownIn)

    pmatchC (pto ownVal) >>= \case
      PMap elems ->
        pure $
          pmap
            # plam
              ( \kv -> unTermCont do
                  cs <- pletC $ pfromData $ pfstBuiltin # kv
                  pmatchC (plookup # cs # mint) >>= \case
                    PNothing -> fail "@FsV: Spent currency symbol not in mint"
                    PJust _ -> pure punit
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
  - check that the $FS matches the ID in FSDatum and has quantity 1
  - accumulate validated $FS token
- check that all accumulated spent $FS tokens are burned

NOTE: FsV delegates spending validation to FsMp
-}
fsMpBurn :: ClosedTerm (PFsMpParams :--> PScriptContext :--> POpaque)
fsMpBurn = phoistAcyclic $
  plam $ \_ ctx -> unTermCont do
    ptraceC "$FS burn"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose

    let foldFn shouldBurn txInInfo = unTermCont do
          txOut <- pletC $ pfield @"resolved" # txInInfo
          txIn <- pletFieldsC @'["value", "address"] $ pfield @"resolved" # txInInfo
          fsDatum <- pletFieldsC @'["fd'submitter", "fd'gcAfter", "fd'fsCs", "fd'id"] $ pdatumFromTxOut @PFsDatum # ctx # txOut

          isOwnToken <- pletC $ ownCs #== fsDatum.fd'fsCs
          pboolC
            ( do
                ptraceC "FsMp: Not my token"
                pure shouldBurn
            )
            ( do
                _ <- pletC $ pmustBeSignedBy # ctx # fsDatum.fd'submitter
                ptraceC "@FsMp: Submitter signed"

                _ <- pletC $ pmustValidateAfter # ctx # fsDatum.fd'gcAfter
                ptraceC "@FsMp: Time validates"

                fsTn <- pletC $ pcon (PTokenName fsDatum.fd'id)
                idMatches <- pletC $ pvalueOf # pfromData txIn.value # fsDatum.fd'fsCs # fsTn #== 1
                pboolC (fail "FsMp: ID must match") (ptraceC "FsMp: ID matches") idMatches

                pure $ shouldBurn <> (PValue.psingleton # ownCs # fsTn # (pnegate # 1))
            )
            isOwnToken

    -- Contains negative quantities
    shouldBurnTotal <- pletC $ pfoldTxInputs # ctx # plam foldFn # mempty
    mintedNonAda <- pletC $ pnoAdaValue #$ pnormalize # (pfield @"mint" # ctx'.txInfo)
    _ <- pletC $ mintedNonAda #== shouldBurnTotal
    ptraceC "$FS burn: $FS spent are valid and burned"

    pure $ popaque $ pconstant ()

fsMpMint :: ClosedTerm (PFsMpParams :--> PScriptContext :--> POpaque)
fsMpMint = phoistAcyclic $
  plam $ \params ctx -> unTermCont do
    ptraceC "$FS mint"

    validAuthInputs <- pletC $ caValidateAuthInputs # (pfield @"fmp'authParams" # params) # ctx
    ptraceC "$FS mint: Validated authentication inputs"

    ctx' <- pletFieldsC @'["purpose"] ctx
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose
    fsMintParseOutput' <- pletC $ fsMintParseOutput # params # ctx # ownCs

    restAuths <- pletC $ pfoldTxOutputs # ctx # fsMintParseOutput' # validAuthInputs
    ptraceC "$FS mint: Validated Fact Statement outputs"

    pboolC
      (fail "$FS mint: Auth inputs must ALL be used")
      (ptraceC "$FS mint: Auth inputs are all used")
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
    outVal <- pletC $ pnormalize # txOut'.value

    hasOwnCs <- pletC $ phasCurrency # ownCs # outVal
    pboolC
      ( do
          ptraceC "fsMintParseOutput: own token not in the output, skipping"
          pure validAuthInputs
      )
      ( do
          ptraceC "fsMintParseOutput: found own token in the output, continuing"
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

    ownTokens <- pletC $ pcurrencyTokens # ownCs # outVal

    let foldFn mayFsTnAndAuths authInput =
          unTermCont do
            fsTn <- pletC $ ptokenNameFromTxInInfo # authInput
            restAuths <- pletC $ pfield @"_1" # mayFsTnAndAuths
            pure $
              pif -- NOTE: Only outputs a single $FS token!
                (ownTokens #== (psingleton # fsTn # 1))
                (ptuple # pdata (pdjust fsTn) # pdata restAuths) -- Q: Hmm...I want to break from here
                (ptuple # pdata pdnothing # pdata (pcons # authInput # restAuths))

    mayFsTnAndAuths <-
      pletC $
        pfoldl
          # plam foldFn
          # ( ptuple
                # pdata (pdnothing :: Term _ (PMaybeData PTokenName))
                # pdata (pnil :: Term _ (PBuiltinList PTxInInfo))
            )
          # validAuthInputs

    mayFsTn <- pletC $ pfield @"_0" # mayFsTnAndAuths
    restAuthInputs <- pletC $ pfield @"_1" # mayFsTnAndAuths

    pmaybeDataC
      (fail "fsMintParseOutputWithFs: invalid $FS found")
      ( \fsTn -> do
          ptraceC "fsMintParseOutputWithFs: $FS validated"
          -- NOTE: This check is sufficient as $FS are NFTs
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
    txInfo <- pletFieldsC @'["inputs", "mint"] (pfield @"txInfo" # ctx)

    insNToBurn <-
      pletC $
        pfoldl
          # (caParseInput # params # ctx # certs)
          # (ptuple # pdata pnil # pdata mempty)
          # txInfo.inputs

    validAuthInputs <- pletC $ pfromData $ pfield @"_0" # insNToBurn
    authTokensToBurn <- pletC $ pfromData $ pfield @"_1" # insNToBurn
    mintedNoAda <- pletC $ pnoAdaValue #$ pnormalize # txInfo.mint

    -- Contains negative quantities
    pboolC
      (fail "caParseInputs: Spent $AUTH tokens must be burned")
      (ptraceC "caParseInputs: Spent $AUTH tokens were burned" >> pure validAuthInputs)
      (mintedNoAda #== authTokensToBurn)

caParseInput ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PTuple (PBuiltinList PTxInInfo) (PValue 'Sorted 'NonZero)
        :--> PTxInInfo
        :--> PTuple (PBuiltinList PTxInInfo) (PValue 'Sorted 'NonZero)
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
          ptraceC "caParseInput: auth token not in the input, skipping"
          pure acc
      )
      ( do
          ptraceC "caParseInput: auth token in the output, continuing"
          pure $ caParseInputWithAuth # ctx # authTokenCs # certs # acc # txIn
      )
      hasAuthCs

{- | Handles a transaction input that holds an $AUTH token.

- check that there's exactly one $AUTH token that's associated with a valid Certificate
- check that the $AUTH token is burned

The TxInInfo is emitted only if it's been validated and burned.
NOTE: Fails hard
-}
caParseInputWithAuth ::
  Term
    s
    ( PScriptContext
        :--> PCurrencySymbol
        :--> PBuiltinList PCertDatum
        :--> PTuple (PBuiltinList PTxInInfo) (PValue 'Sorted 'NonZero)
        :--> PTxInInfo
        :--> PTuple (PBuiltinList PTxInInfo) (PValue 'Sorted 'NonZero)
    )
caParseInputWithAuth = phoistAcyclic $
  plam $ \_ authTokenCs certs acc txIn -> unTermCont do
    ptraceC "caParseInputWithAuth"
    txInOut <- pletC $ pfield @"resolved" # txIn
    txInVal <- pletC $ pnormalize # (pfield @"value" # txInOut)

    authTokens <- pletC $ pcurrencyTokens # authTokenCs # txInVal

    mayAuthTn <-
      pletC $
        pfindMap
          # plam
            ( \certDat ->
                plet
                  (pcon (PTokenName $ pfield @"cert'id" # certDat))
                  ( \authTn ->
                      pif
                        (authTokens #== (psingleton # authTn # 1)) -- NOTE: Only contains a single $AUTH token!
                        pdnothing
                        (pdjust authTn)
                  )
            )
          # certs

    pmaybeDataC
      (fail "caParseInputWithAuth: couldn't validate the auth token, skipping")
      ( \authTn -> do
          ptraceC "caParseInputWithAuth: auth token validated, continuing"
          validAuthInput <- pletC $ pcons # txIn # (pfield @"_0" # acc)
          shouldBeBurned <- pletC $ pfield @"_1" # acc <> PValue.psingleton # authTokenCs # authTn # (pnegate # 1)
          pure $ ptuple # pdata validAuthInput # pdata shouldBeBurned
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
    txInfo <- pletFieldsC @'["inputs"] (pfield @"txInfo" # ctx)
    pure $ pfoldl # (caParseRef # params # ctx) # pnil # txInfo.inputs

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
    txInVal <- pletC $ pnormalize # (pfield @"value" # txInOut)
    certTokenCs <- pletC $ pfield @"ap'certTokenCs" # params

    pboolC
      ( do
          ptraceC "caParseRef: cert token not in the input, skipping"
          pure acc
      )
      ( do
          ptraceC "caParseRef: cert token in the input, continuing"
          pure $ caParseRefWithCert # ctx # certTokenCs # txInVal # acc # txInInfo
      )
      (phasCurrency # certTokenCs # txInVal)

{- | Handles a transaction input that holds an $CERT token.

- check that the transaction's validator range is contained withing the Certificate's validity range
- check that the Certificate ref input has 1 $CERT token with appropriate ID (TokenName)

The CertDatum is emitted only if it's been validated.
The function fails hard if any of the checks fails.
-}
caParseRefWithCert ::
  Term
    s
    ( PScriptContext
        :--> PCurrencySymbol
        :--> PValue 'Sorted 'NonZero
        :--> PBuiltinList PCertDatum
        :--> PTxInInfo
        :--> PBuiltinList PCertDatum
    )
caParseRefWithCert = phoistAcyclic $
  plam $ \ctx certTokenCs txInVal acc txIn -> unTermCont do
    ptraceC "caParseRefWithCert"
    txInOut <- pletC $ pfield @"resolved" # txIn

    certDat <- pletC $ (pdatumFromTxOut @PCertDatum) # ctx # txInOut
    certValidity <- pletC $ pfield @"cert'validity" # certDat
    certId <- pletC $ pfield @"cert'id" # certDat

    certTokens <- pletC $ pcurrencyTokens # certTokenCs # txInVal
    pboolC
      (fail "caParseRefWithCert: cert mismatched id")
      (ptraceC "caParseRefWithCert: cert has a matching id")
      (certTokens #== (psingleton # pcon (PTokenName certId) # 1))

    ctx' <- pletFieldsC @'["txInfo"] ctx
    txInfo <- pletFieldsC @'["validRange"] (getField @"txInfo" ctx')
    txValidRange <- pletC $ pfromData $ getField @"validRange" txInfo
    pboolC
      (fail "caParseRefWithCert: cert is invalid")
      (ptraceC "caParseRefWithCert: cert is valid")
      (pcontains # certValidity # txValidRange)

    pure $ pcons # certDat # acc

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

authMpBurnCert :: ClosedTerm (PScriptContext :--> POpaque)
authMpBurnCert = phoistAcyclic $
  plam $ \ctx -> unTermCont do
    ptraceC "AuthMp burn $CERT"
    ptraceC "AuthMp burn $CERT"

    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose

    let foldFn acc txInInfo = unTermCont do
          txIn <- pletC $ pfield @"resolved" # txInInfo
          txInVal <- pletC $ pfield @"value" # txIn
          pboolC
            (ptraceC "AuthMp burn $CERT: Skipping foreign input" >> pure acc)
            ( do
                ptraceC "AuthMp burn $CERT: Found own input"
                certDatum' <- pletC $ pdatumFromTxOut @PCertDatum # ctx # txIn
                certDatum <-
                  pletFieldsC
                    @'[ "cert'id"
                      , "cert'validity"
                      , "cert'redeemerAc"
                      ]
                    $ certDatum'

                -- _ <- pletC $ pmustValidateAfter # ctx # (pfield @"to" # certDatum.cert'validity)
                ptraceC "AuthMp burn $CERT: Can collect"

                redeemerAc <- pletFieldsC @'["_0", "_1"] certDatum.cert'redeemerAc

                _ <- pletC $ pmustSpend # ctx # redeemerAc._0 # redeemerAc._1 # 1
                ptraceC "AuthMp burn $CERT: $CERT_RDMR spent"

                certTn <- pletC $ pcon (PTokenName $ certDatum.cert'id)
                pboolC
                  (fail "AuthMp burn $CERT: Must spend a single $CERT token")
                  (fail "AuthMp burn $CERT: Spent a single $CERT token")
                  (pvalueOf # txInVal # ownCs # certTn #== 1)
                _ <- pletC $ pmustMint # ctx # ownCs # certTn # (pnegate # 1)
                ptraceC "AuthMp burn $CERT: $CERT spent and burned"

                pure acc
            )
            (phasCurrency # ownCs # txInVal)

    _ <- pletC $ pfoldTxInputs # ctx # plam foldFn # punit
    pure $ popaque $ pconstant ()

-- | WIP: Minting policy that validates minting and burning of $AUTH and $CERT tokens tokens
mkAuthMp :: ClosedTerm (PAsData PAuthMpParams :--> PMintingPolicy)
mkAuthMp = phoistAcyclic $
  plam $ \params _ ctx -> unTermCont do
    ptraceC "AuthMp"
    ptraceC "AuthMp mint"
    ctx' <- pletFieldsC @'["txInfo", "purpose"] ctx
    ownCs <- pletC $ pownCurrencySymbol # ctx'.purpose
    authParams <- pletFieldsC @'["amp'authAuthorityAc", "amp'authAuthorityQ", "amp'certVAddress"] params

    let foldFn aasAndTnBytes txIn = unTermCont do
          tnBytes <- pletC $ pfield @"_1" # aasAndTnBytes
          aaVal <- pletC $ pfield @"_0" # aasAndTnBytes

          -- accumulate token name bytes
          txIn' <- pletFieldsC @'["resolved", "outRef"] txIn
          txId <- pletC $ pfield @"_0" #$ pfield @"id" # txIn'.outRef
          txIdx <- pletC $ pfield @"idx" # txIn'.outRef
          tnBytes' <- pletC $ tnBytes <> pconsBS # txIdx # txId

          -- accumulate token quantities
          txInVal <- pletC $ pfield @"value" # txIn'.resolved
          aaCs <- pletC $ pfield @"_0" # authParams.amp'authAuthorityAc
          aaTn <- pletC $ pfield @"_1" # authParams.amp'authAuthorityAc

          aaVal' <- pletC $ aaVal #+ (pvalueOf # txInVal # aaCs # aaTn)
          pure $ ptuple # pdata aaVal' # pdata tnBytes'

    aasAndTnBytes <- pletC $ pfoldTxInputs # ctx # plam foldFn # (ptuple # pdata 0 # pdata mempty)

    aaTokensSpent <- pletC $ pfield @"_0" # aasAndTnBytes
    pboolC
      (fail "AuthMp: Must spend the specified amount of AA tokens")
      (ptraceC "AuthMp: Spent the specified amount of AA tokens")
      (aaTokensSpent #== authParams.amp'authAuthorityQ)

    tnBytes <- pletC $ pfield @"_1" # aasAndTnBytes

    -- NOTE: cert- is prepended so $CERT is distinguishable from $AUTH, otherwise a compromised $AUTH can create an arbitrary $CERT
    certTn <- pletC $ pcon $ PTokenName (psha3_256 # tnBytes)
    _ <- pletC $ pmustMint # ctx # ownCs # certTn # 1
    -- TODO: verify datum
    _ <- pletC $ pmustPayTo # ctx # ownCs # certTn # 1 # authParams.amp'certVAddress

    authTn <- pletC $ pcon $ PTokenName (psha3_256 # pconstant "auth-" <> tnBytes)
    _ <- pletC $ (pvalueOf # (pfield @"mint" # ctx'.txInfo) # ownCs # authTn) #< 0

    pure $ popaque $ pconstant ()
