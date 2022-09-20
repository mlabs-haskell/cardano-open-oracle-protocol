{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Coop.Plutus (
  mkFsMp,
  fsV,
  mkAuthMp,
  certV,
  mkCertMp,
  pmustSpendAtLeastAa,
) where

import Coop.Plutus.Aux (pcurrencyTokenQuantity, pcurrencyValue, pdatumFromTxOut, pdjust, pdnothing, pfindMap, pfoldTxInputs, pfoldTxOutputs, phasCurrency, pmaybeData, pmustBeSignedBy, pmustHandleSpentWithMp, pmustMint, pmustMintCurrency, pmustPayCurrencyWithDatumTo, pmustSpendAtLeast, pmustValidateAfter, pownCurrencySymbol, ptryFromData, punit)
import Coop.Plutus.Types (PAuthMpParams, PAuthMpRedeemer (PAuthMpBurn, PAuthMpMint), PAuthParams, PCertDatum, PCertMpParams, PCertMpRedeemer (PCertMpBurn, PCertMpMint), PFsDatum, PFsMpParams, PFsMpRedeemer (PFsMpBurn, PFsMpMint))
import Plutarch (POpaque, pmatch, popaque)
import Plutarch.Api.V1.Value (passertPositive, pnormalize, pvalueOf)
import Plutarch.Api.V1.Value qualified as PValue
import Plutarch.Api.V2 (
  AmountGuarantees (NonZero, Positive),
  KeyGuarantees (Sorted),
  PCurrencySymbol,
  PMaybeData,
  PMintingPolicy,
  PTokenName (PTokenName),
  PTuple,
  PTxInInfo,
  PTxOut,
  PValidator,
  PValue,
 )
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Bool (pif)
import Plutarch.Extra.Interval (pcontains)
import Plutarch.Monadic qualified as P
import Plutarch.Num (PNum (pnegate, (#+)))
import Plutarch.Prelude (ClosedTerm, PAsData, PBuiltinList, PByteString, PEq ((#==)), PInteger, PListLike (pcons, pnil), PPair (PPair), PPartialOrd ((#<), (#<=)), Term, pcon, pconsBS, pfield, pfoldl, pfromData, phoistAcyclic, plam, plet, pletFields, psha3_256, ptrace, ptraceError, (#), (#$), type (:-->))
import PlutusTx.Prelude (Group (inv))
import Prelude (Monoid (mempty), Semigroup ((<>)), const, ($))

-- | Delegates spending validation to corresponding minting policies
fsV :: ClosedTerm PValidator
fsV = phoistAcyclic $
  plam $ \_ _ ctx -> ptrace "@FsV" P.do
    -- ERR[Andrea]: should we be allowed to spend a fsV input as we mint a new $FS token?
    --              fsMpMint does not check we are after "fd'gcAfter" for any fsV inputs.
    _ <- plet $ pmustHandleSpentWithMp # ctx
    ptrace "@FsV: Spending delegated to FsMp" $ popaque punit

-- | Minting policy that validates minting and burning of $FS tokens
mkFsMp :: ClosedTerm (PAsData PFsMpParams :--> PMintingPolicy)
mkFsMp = phoistAcyclic $
  plam $ \params red ctx -> ptrace "FsMp" P.do
    red' <- plet $ pfromData (ptryFromData @PFsMpRedeemer red)

    pmatch red' \case
      PFsMpBurn _ -> fsMpBurn # pfromData params # ctx
      PFsMpMint _ -> fsMpMint # pfromData params # ctx

{- | Validates burning $FS tokens

- check each input and accumulate $FS tokens
  - skip if doesn't hold $FS token
  - check the trx is signed by the submitter as indicated in the FsDatum
  - check that the trx validates after time as indicated in the FsDatum
  - accumulate validated $FS token
- check that all accumulated spent $FS tokens are burned

NOTE: @FsV delegates spending validation to FsMp
-}
fsMpBurn :: ClosedTerm (PFsMpParams :--> PScriptContext :--> POpaque)
fsMpBurn = phoistAcyclic $
  plam $ \_ ctx -> ptrace "FsMp burn" P.do
    ctx' <- pletFields @'["txInfo", "purpose"] ctx
    ownCs <- plet $ pownCurrencySymbol # ctx'.purpose
    minted <- plet $ pfield @"mint" # ctx'.txInfo

    let foldFn shouldBurn txInInfo = P.do
          txOut <- plet $ pfield @"resolved" # txInInfo
          txIn <- pletFields @'["value", "address"] $ pfield @"resolved" # txInInfo
          -- WARN[Andrea]: this will error out on non-FsDatum inputs, such as wallet ones to cover fees.
          --               The comment below refers to this line.
          fsDatum <- pletFields @'["fd'submitter", "fd'gcAfter", "fd'fsId"] $ pdatumFromTxOut @PFsDatum # ctx # txOut

          pif
            (phasCurrency # ownCs # txIn.value)
            ( ptrace "FsMp burn: Found own input" P.do
                _ <- plet $ pmustBeSignedBy # ctx # fsDatum.fd'submitter
                ptrace "FsMp burn: Submitter signed"

                -- TODO(Andrea): Please check that I didn't mess up interval handling
                -- INFO[Andrea]: looks good to me.
                _ <- plet $ pmustValidateAfter # ctx # fsDatum.fd'gcAfter
                ptrace "FsMp burn: Time validates"

                ownSpent <- plet $ pcurrencyValue # ownCs # txIn.value
                shouldBurn <> inv ownSpent
            )
            (ptrace "FsMp burn: Skipping foreign input" shouldBurn)

    -- Contains negative quantities
    -- WARN[Andrea]: will fail parsing a `fsDatum` if non-fsV inputs are present.
    --               It's possible extra pure Ada ones are needed to pay fees?
    shouldBurnTotal <- plet $ pfoldTxInputs # ctx # plam foldFn # mempty
    ownMinted <- plet $ pcurrencyValue # ownCs # minted
    pif
      (ownMinted #== shouldBurnTotal)
      (ptrace "FsMp burn: $FS spent are valid and burned" $ popaque punit)
      (ptraceError "FsMp mint: $FS spent must be valid and burned")

{- | Validates minting of $FS tokens.

- check all $AUTH inputs against their corresponding $CERT inputs 'referenced' at @CertV
- with all the valid $AUTH inputs proceed to check $FS outputs paid to @FsV
  - each $FS output MUST be exclusively authenticated by one $AUTH input
-}
fsMpMint :: ClosedTerm (PFsMpParams :--> PScriptContext :--> POpaque)
fsMpMint = phoistAcyclic $
  plam $ \params ctx -> ptrace "FsMp mint" P.do
    validAuthInputs <- plet $ caValidateAuthInputs # (pfield @"fmp'authParams" # params) # ctx
    ptrace "FsMp mint: Validated authentication inputs"

    ctx' <- pletFields @'["purpose"] ctx
    ownCs <- plet $ pownCurrencySymbol # ctx'.purpose
    -- MINOR[Andrea]: could be made into a `let`
    fsMintParseOutput' <- plet $ fsMintParseOutput # params # ctx # ownCs

    restAuths <- plet $ pfoldTxOutputs # ctx # fsMintParseOutput' # validAuthInputs
    ptrace "FsMp mint: Validated and authenticated Fact Statement outputs"

    pif
      (restAuths #== pnil)
      (ptrace "FsMp mint: Auth inputs are all used" $ popaque punit)
      (ptraceError "FsMp mint: Auth inputs must ALL be used")

-- | Skips foreign inputs and processes $FS inputs.
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
  plam $ \params ctx ownCs validAuthInputs txOut -> ptrace "fsMintParseOutput" P.do
    txOut' <- pletFields @'["value"] txOut

    hasOwnCs <- plet $ phasCurrency # ownCs # txOut'.value
    pif
      hasOwnCs
      (ptrace "fsMintParseOutput: Found own token in the output" $ fsMintParseOutputWithFs # params # ctx # ownCs # validAuthInputs # txOut)
      (ptrace "fsMintParseOutput: Skipping foreign output" validAuthInputs)

{- | Handles a transaction output that holds an $FS token.

- check that 1 $FS is minted and associated with a valid $AUTH
  - $FS token name is formed by hashing TxOutRef+Num of the associated $AUTH input
- check that 1 $FS is sent to FsV

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
  plam $ \params ctx ownCs validAuthInputs txOut -> ptrace "fsMintParseOutputWithFs" P.do
    -- WARN[Andrea]: no checks on the datum? I guess if it's malformed
    -- it will just mean the submitter can't use it or recyle it.
    txOut' <- pletFields @'["value", "address"] txOut
    -- PERF[Andrea]: In cardax I have found value normalizations to be
    -- quite expensive, something to keep in mind if you have budget
    -- problems.
    outVal <- plet $ pnormalize # txOut'.value
    outAddr <- plet $ txOut'.address

    _ <-
      plet $
        pif
          -- PERF[Andrea]: the field projection could be done outside the fold.
          (outAddr #== (pfield @"fmp'fsVAddress" # params))
          (ptrace "fsMintParseOutputWithFs: Output sent to FsV" $ popaque punit)
          (ptraceError "fsMintParseOutputWithFs: Output must be sent to FsV")

    ownValue <- plet $ pcurrencyValue # ownCs # outVal

    let matchWithAuth mayFsTnAndAuths authInput = P.do
          PPair mayFsTn restAuthInputs <- pmatch mayFsTnAndAuths

          pmaybeData
            mayFsTn
            ( P.do
                -- NOTE: Here we create a UNIQUE $FS token name
                fsTn <- plet $ ptokenNameFromTxInInfo # authInput
                pif -- NOTE: Only outputs a single $FS token!
                  (ownValue #== (PValue.psingleton # ownCs # fsTn # 1))
                  (ptrace "fsMintParseOutputWithFs: Found the $FS token" $ pcon $ PPair (pdjust fsTn) restAuthInputs)
                  (pcon $ PPair pdnothing (pcons # authInput # restAuthInputs))
            )
            (const mayFsTnAndAuths)

    PPair mayFsTn restAuthInputs <-
      pmatch $
        pfoldl
          # plam matchWithAuth
          # pcon
            ( PPair
                (pdnothing :: Term s (PMaybeData PTokenName))
                (pnil :: Term s (PBuiltinList PTxInInfo))
            )
          # validAuthInputs

    pmaybeData
      mayFsTn
      (ptraceError "fsMintParseOutputWithFs: $FS must have a token name formed from a matching $AUTH input")
      ( \fsTn -> ptrace "fsMintParseOutputWithFs: $FS validated" P.do
          -- NOTE: This check is sufficient as $FS are unique
          -- INFO[Andrea]: I would say this is sufficient to prevent unwanted mints because you
          --               also check _all outputs_ for $FS tokens, and would error if
          --               you found ones with unapproved token names.
          -- ERR[Andrea]: allows $FS with other token names to be
          --              burned at the same time, without checking `fd'gcAfter`.
          --              Would be safer to accumulate a `shouldBeMinted` value and check it all at once.
          _ <- plet $ pmustMint # ctx # ownCs # fsTn # 1
          ptrace "fsMintParseOutputWithFs: $FS minted" restAuthInputs
      )

{- | ptokenNameFromTxInInfo creates a unique TokenName from the given transaction input

TokenName $ sha3_256 (refId `concat` num)
-}
ptokenNameFromTxInInfo :: Term s (PTxInInfo :--> PTokenName)
ptokenNameFromTxInInfo = phoistAcyclic $
  plam $ \inInfo -> P.do
    txId <- plet $ pfield @"_0" # (pfield @"id" # (pfield @"outRef" # inInfo))
    -- TODO: Check that txIDx < 256
    txIdx <- plet $ pfield @"idx" # (pfield @"outRef" # inInfo)
    pcon $ PTokenName $ psha3_256 # (pconsBS # txIdx # txId)

{- | Validates $AUTH inputs against associated $CERT reference inputs.

- check $AUTH inputs for validity against their corresponding $CERT input
- check that $AUTH spent is also burned

NOTES:
- $CERT is a unique token locked @CertV that authenticating scripts can reference
- $AUTH is minted in batches and associated with a $CERT token via the token name
-}
caValidateAuthInputs ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PTxInInfo
    )
caValidateAuthInputs = phoistAcyclic $
  plam $ \params ctx -> ptrace "caValidateAuthInputs" P.do
    validCerts <- plet $ caParseRefs # params # ctx
    caParseInputs # params # ctx # validCerts

caParseInputs ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
        :--> PBuiltinList PTxInInfo
    )
caParseInputs = phoistAcyclic $
  plam $ \params ctx certs -> ptrace "caParseInputs" P.do
    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["inputs", "mint"] ctx'.txInfo

    PPair validAuthInputs authTokensToBurn <-
      pmatch $
        pfoldTxInputs
          # ctx
          # (caParseInput # params # ctx # certs)
          # pcon (PPair pnil mempty)

    -- Contains negative quantities
    authBurned <- plet $ pcurrencyValue # (pfield @"ap'authTokenCs" # params) # txInfo.mint
    pif
      (authBurned #== authTokensToBurn)
      (ptrace "caParseInputs: Spent $AUTH tokens were burned" validAuthInputs)
      (ptraceError "caParseInputs: Spent $AUTH tokens must be burned")

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
  plam $ \params ctx certs acc txIn -> ptrace "caParseInput" P.do
    txInOut <- plet $ pfield @"resolved" # txIn
    txInVal <- plet $ pnormalize # (pfield @"value" # txInOut)

    -- PERF[Andrea]: lookup field in `caParseInputs` so it's done only once?
    authTokenCs <- plet $ pfield @"ap'authTokenCs" # params
    hasAuthCs <- plet $ phasCurrency # authTokenCs # txInVal
    pif
      hasAuthCs
      (ptrace "caParseInput: Found $AUTH token in the input" $ caParseInputWithAuth # ctx # authTokenCs # certs # acc # txIn)
      (ptrace "caParseInput: Skipping non $AUTH input" acc)

{- | Handles a transaction input that holds an $AUTH token.

- check that there's at least one $AUTH token that's associated with a valid Certificate
- accumulates 1 $AUTH token to burn

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
  plam $ \_ authTokenCs certs acc txIn -> ptrace "caParseInputWithAuth" P.do
    txInOut <- plet $ pfield @"resolved" # txIn
    txInVal <- plet $ pnormalize # (pfield @"value" # txInOut)

    mayAuthTn <-
      plet $
        pfindMap
          # plam
            ( \certDat ->
                plet
                  (pcon (PTokenName $ pfield @"cert'id" # certDat))
                  ( \authTn ->
                      pif
                        (0 #< (pvalueOf # txInVal # authTokenCs # authTn)) -- NOTE: Contains at least 1 $AUTH
                        (pdjust authTn)
                        pdnothing
                  )
            )
          # certs

    pmaybeData
      mayAuthTn
      (ptraceError "caParseInputWithAuth: $AUTH must be validated with a $CERT")
      ( \authTn -> ptrace "caParseInputWithAuth: $AUTH valid" P.do
          PPair validAuthInputs shouldBeBurned <- pmatch acc
          validAuthInputs' <- plet $ pcons # txIn # validAuthInputs
          shouldBeBurned' <- plet $ shouldBeBurned <> PValue.psingleton # authTokenCs # authTn # (pnegate # 1)
          pcon $ PPair validAuthInputs' shouldBeBurned'
      )

-- WARN[Andrea]: what should happen with multiple $AUTH tokens on
--               a single utxo?  currently the $AUTH matching the
--               first `cert` in `certs` has to be burned, and the
--               other $AUTH tokens are sent back in the outputs I
--               presume.

-- TODO: Migrate to reference inputs
caParseRefs ::
  Term
    s
    ( PAuthParams
        :--> PScriptContext
        :--> PBuiltinList PCertDatum
    )
caParseRefs = phoistAcyclic $
  plam $ \params ctx -> ptrace "caParseRefs" pfoldTxInputs # ctx # (caParseRef # params # ctx) # pnil

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
  plam $ \params ctx acc txInInfo -> ptrace "caParseRef" P.do
    txIn <- plet $ pfield @"resolved" # txInInfo
    txInVal <- plet $ pfield @"value" # txIn
    certTokenCs <- plet $ pfield @"ap'certTokenCs" # params
    certVal <- plet $ pcurrencyValue # certTokenCs # txInVal
    pif
      (certVal #== mempty)
      (ptrace "caParseRef: Skipping non $CERT input" acc)
      (ptrace "caParseRef: Found $CERT input" $ caParseRefWithCert # ctx # certTokenCs # (passertPositive # certVal) # acc # txInInfo)

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
  plam $ \ctx certTokenCs certVal acc txInInfo -> ptrace "caParseRefWithCert" P.do
    txIn <- plet $ pfield @"resolved" # txInInfo

    certDat' <- plet $ (pdatumFromTxOut @PCertDatum) # ctx # txIn
    certDat <- pletFields @'["cert'validity", "cert'id"] certDat'

    _ <-
      plet $
        pif
          (pvalueOf # certVal # certTokenCs # pcon (PTokenName certDat.cert'id) #== 1)
          (ptrace "caParseRefWithCert: $CERT token name matched CertDatum ID" punit)
          (ptraceError "caParseRefWithCert: $CERT token name must match CertDatum ID")

    ctx' <- pletFields @'["txInfo"] ctx
    txInfo <- pletFields @'["validRange"] ctx'.txInfo
    pif
      (pcontains # certDat.cert'validity # txInfo.validRange)
      (ptrace "caParseRefWithCert: cert is valid" $ pcons # certDat' # acc)
      (ptraceError "caParseRefWithCert: cert is invalid")

{- | Authentication scripts

Authentication works with 3 different tokens, namely $AA, $AUTH and $CERT.

- $AA is the Authentication Authority token that is owned by the 'admin' and is
where the security of the entire system relies on. It's used with AuthMp to authorize minting of new $AUTH/$CERT tokens.

- $AUTH is the Authentication Grant token that is minted only if authorized by the
\$AA holder. These tokens can be minted in batches and sent to any number of operational
(ie. hot) wallets.

- $CERT is a Certificate token that is associated with $AUTH tokens via an identifier that is
stored in token name part of both tokens. It's sole purpose is to hold some information about $AUTH that the
authenticating scripts CAN use to validate $AUTH inputs. These are locked @CertV.

- $CERT-RDMR is a token used to collect the no longer valid $CERT output @CertV

- @CertV is a script where $CERT tokens are locked at and authenticating scripts can 'reference' these outputs when performing validation.
-}

-- | Delegates spending validation to corresponding minting policies
certV :: ClosedTerm PValidator
certV = phoistAcyclic $
  plam $ \_ _ ctx -> ptrace "@CertV" P.do
    -- WARN[Andrea]: Allows spending CertV input while minting $CERT without checking validity range.
    _ <- plet $ pmustHandleSpentWithMp # ctx
    ptrace "@CertV: Spending delegated to CertMp" $ popaque punit

mkCertMp :: ClosedTerm (PAsData PCertMpParams :--> PMintingPolicy)
mkCertMp = phoistAcyclic $
  plam $ \params red ctx -> ptrace "CertMp" P.do
    red' <- plet $ pfromData (ptryFromData @PCertMpRedeemer red)

    pmatch red' \case
      PCertMpBurn _ -> certMpBurn # ctx
      PCertMpMint _ -> certMpMint # pfromData params # ctx

{- | Validates burning of $CERT tokens

For each $CERT input
- check the transaction validates after the certificate validity period
- check that $CERT-RDMR token as specified in the CertDatum is spent
- check that the spent $CERT is also burned

Notes:
 - skips foreign inputs
 - can burn multiple $CERT tokens
-}
certMpBurn :: ClosedTerm (PScriptContext :--> POpaque)
certMpBurn = phoistAcyclic $
  plam $ \ctx -> ptrace "CertMp burn" P.do
    ctx' <- pletFields @'["txInfo", "purpose"] ctx
    ownCs <- plet $ pownCurrencySymbol # ctx'.purpose

    let foldFn shouldBurn txInInfo = P.do
          txIn <- plet $ pfield @"resolved" # txInInfo
          txInVal <- plet $ pfield @"value" # txIn
          pif
            (phasCurrency # ownCs # txInVal)
            ( ptrace "CertMp burn: Found own input" P.do
                certDatum' <- plet $ pdatumFromTxOut @PCertDatum # ctx # txIn
                certDatum <-
                  pletFields
                    @'[ "cert'id"
                      , "cert'validity"
                      , "cert'redeemerAc"
                      ]
                    $ certDatum'
                ptrace "CertMp burn: Parsed the CertDatum"

                certValidUntil <- plet $ pfield @"_0" #$ pfield @"to" # certDatum.cert'validity
                _ <- plet $ pmustValidateAfter # ctx # certValidUntil
                ptrace "CertMp burn: Can collect invalid cert"

                redeemerAc <- pletFields @'["_0", "_1"] certDatum.cert'redeemerAc
                -- TODO(perf): Consider adding a TxOutRef to the redeemer as it would make it more efficient to find the $CERT-RDMR input
                _ <- plet $ pmustSpendAtLeast # ctx # redeemerAc._0 # redeemerAc._1 # 1
                ptrace "CertMp burn: At least 1 $CERT-RDMR spent"

                certVal <- plet $ PValue.psingleton # ownCs # pcon (PTokenName $ certDatum.cert'id) # 1
                _ <-
                  plet $
                    pif
                      (pcurrencyValue # ownCs # txInVal #== certVal)
                      (ptrace "CertMp burn: Spent 1 $CERT token" punit)
                      (ptraceError "CertMp burn: Must spend a 1 $CERT token")

                shouldBurn <> inv certVal
            )
            (ptrace "CertMp burn: Skipping foreign input" shouldBurn)

    shouldBurn <- plet $ pfoldTxInputs # ctx # plam foldFn # mempty

    _ <- plet $ pmustMintCurrency # ctx # ownCs # shouldBurn

    ptrace "CertMp burn: All $CERTs spent and burned" $ popaque punit

{- | Validates minting of $CERT tokens

- check that the $AA quantity as specified in the CertMpParams is spent
- accumulate the $AA inputs into a unique token name to use for the $CERT token minted
- check that 1 $CERT is paid to @CertV
- check that the $CERT outputs at @CertV has a valid CertDatum.cert'id

Notes:
- can mint only 1 $CERT token per transaction
-}
certMpMint :: ClosedTerm (PCertMpParams :--> PScriptContext :--> POpaque)
certMpMint = phoistAcyclic $
  plam $ \params ctx -> ptrace "CertMp mint" P.do
    ctx' <- pletFields @'["txInfo", "purpose"] ctx
    ownCs <- plet $ pownCurrencySymbol # ctx'.purpose
    certParams <- pletFields @'["cmp'authAuthorityAc", "cmp'requiredAtLeastAaQ", "cmp'certVAddress"] params

    tnBytes <- plet $ pmustSpendAtLeastAa # ctx # certParams.cmp'authAuthorityAc # certParams.cmp'requiredAtLeastAaQ
    ptrace "CertMp mint: Spent at least a required quantity of $AA tokens"

    certTn <- plet $ pcon $ PTokenName tnBytes
    _ <- plet $ pmustMintCurrency # ctx # ownCs # (PValue.psingleton # ownCs # certTn # 1)
    ptrace "CertMp mint: Minted 1 $CERT"

    _ <-
      plet $
        pmustPayCurrencyWithDatumTo # ctx
          # ownCs
          # (PValue.psingleton # ownCs # certTn # 1)
          # plam (\(certDatum :: Term s PCertDatum) -> (pfield @"cert'id" # certDatum) #== tnBytes)
          # certParams.cmp'certVAddress
    ptrace "CertMp mint: Paid 1 $CERT to @CertV and attached a valid datum" $ popaque punit

mkAuthMp :: ClosedTerm (PAsData PAuthMpParams :--> PMintingPolicy)
mkAuthMp = phoistAcyclic $
  plam $ \params red ctx -> ptrace "AuthMp" P.do
    red' <- plet $ pfromData (ptryFromData @PAuthMpRedeemer red)

    pmatch red' \case
      PAuthMpBurn _ -> authMpBurn # ctx
      PAuthMpMint _ -> authMpMint # pfromData params # ctx

{- | Validates minting of $AUTH tokens

- check that at least N $AA tokens are spent
- hash spent $AA inputs and create a unique token name for $AUTH token being minted
- check if >1 $AUTH tokens with the same unique token name have been minted (exclusively)
-}
authMpMint :: ClosedTerm (PAuthMpParams :--> PScriptContext :--> POpaque)
authMpMint = phoistAcyclic $
  plam $ \params ctx -> ptrace "AuthMp mint" P.do
    ctx' <- pletFields @'["txInfo", "purpose"] ctx
    ownCs <- plet $ pownCurrencySymbol # ctx'.purpose
    minted <- plet $ pfield @"mint" # ctx'.txInfo
    authMpParams <- pletFields @'["amp'authAuthorityAc", "amp'requiredAtLeastAaQ", "amp'certVAddress"] params

    tnBytes <- plet $ pmustSpendAtLeastAa # ctx # authMpParams.amp'authAuthorityAc # authMpParams.amp'requiredAtLeastAaQ
    ptrace "AuthMp mint: Spent at least a required quantity of $AA tokens"
    authTn <- plet $ pcon $ PTokenName tnBytes

    pif
      (0 #< (pcurrencyTokenQuantity # ownCs # authTn # minted))
      (ptrace "AuthMp mint: At least one $AUTH token is minted" $ popaque punit)
      (ptraceError "AuthMp mint: Must mint at least one $AUTH token")

{- | Validates burning of $AUTH tokens

- accumulate all spent $AUTH tokens and check if all are burned

NOTE: $AUTH tokens can be burned freely
-}
authMpBurn :: ClosedTerm (PScriptContext :--> POpaque)
authMpBurn = phoistAcyclic $
  plam $ \ctx -> ptrace "AuthMp burn $AUTH" P.do
    ctx' <- pletFields @'["txInfo", "purpose"] ctx
    minted <- plet $ pfield @"mint" # ctx'.txInfo
    ownCs <- plet $ pownCurrencySymbol # ctx'.purpose

    let foldFn shouldBurn txInInfo = P.do
          txIn <- plet $ pfield @"resolved" # txInInfo
          txInVal <- plet $ pfield @"value" # txIn
          pif
            (phasCurrency # ownCs # txInVal)
            ( ptrace "AuthMp burn $AUTH: Found own input" P.do
                ownSpent <- plet $ pcurrencyValue # ownCs # txInVal
                shouldBurn <> inv ownSpent
            )
            (ptrace "AuthMp burn $AUTH: Skipping foreign input" shouldBurn)

    -- INFO[Andrea]: this is actually safe against trying to mint/burn
    --               for other TokenNames, certMpBurn should be changed to something similar.
    shouldBurnTotal <- plet $ pfoldTxInputs # ctx # plam foldFn # mempty
    ownMinted <- plet $ pcurrencyValue # ownCs # minted
    pif
      (ownMinted #== shouldBurnTotal)
      (ptrace "AuthMp burn: Burned all spent $AUTH tokens" $ popaque punit)
      (ptraceError "AuthMp burn: All spent $AUTH tokens must be burned")

{- | Checks for total spent $AA tokens

- accumulate all spent $AA tokens and check if totals are at least as specified
- create unique bytestring from $AA inputs by hashing the concatenation of (idx,id) pairs
-}
pmustSpendAtLeastAa :: ClosedTerm (PScriptContext :--> PTuple PCurrencySymbol PTokenName :--> PInteger :--> PByteString)
pmustSpendAtLeastAa = phoistAcyclic $
  plam $ \ctx aaAc atLeastAaQ -> ptrace "pmustSpendAtLeastAa" P.do
    aaCs <- plet $ pfield @"_0" # aaAc
    aaTn <- plet $ pfield @"_1" # aaAc

    let foldFn acc txIn = P.do
          -- check if $AA input
          txIn' <- pletFields @'["resolved", "outRef"] txIn
          txInVal <- plet $ pfield @"value" # txIn'.resolved
          pif
            (phasCurrency # aaCs # txInVal)
            ( ptrace "pmustSpendAtLeastAa: Found an $AA input" P.do
                PPair aaVal tnBytes <- pmatch acc
                -- accumulate token name bytes
                txId <- plet $ pfield @"_0" #$ pfield @"id" # txIn'.outRef
                txIdx <- plet $ pfield @"idx" # txIn'.outRef
                tnBytes' <- plet $ pconsBS # txIdx # txId <> tnBytes
                -- accumulate token quantities
                aaVal' <- plet $ aaVal #+ (pvalueOf # txInVal # aaCs # aaTn)

                pcon $ PPair aaVal' tnBytes'
            )
            (ptrace "pmustSpendAtLeastAa: Skipping non $AA input" acc)

    PPair aaTokensSpent tnBytes <- pmatch $ pfoldTxInputs # ctx # plam foldFn # pcon (PPair 0 mempty)

    pif
      (atLeastAaQ #<= aaTokensSpent)
      (ptrace "pmustSpendAtLeastAa: Spent at least the specified amount of AA tokens" $ psha3_256 # tnBytes)
      (ptraceError "pmustSpendAtLeastAa: Must spend at least the specified amount of AA tokens")
