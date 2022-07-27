# Publishing a fact statement

Definitions:

- **Publisher** is a service that interacts with Submitter clients and Resolver services. It packages fee and fact statements in Cardano transactions,
- **Publisher's Work** is the work done by the Publisher that includes a Resolver in a process called 'query resolution',
  - Requesting information from a remote commerical data APIs,
  - Requesting information from a decentralized oracle pool (validation, consensus).
- **Submitter** is a Publisher client that eventually submits a fact statement transaction on Cardano,
- **Consumer** is a Cardano DApp (Plutus program + frontend) that uses a published Fact Statement.

Properities:

- Publisher's Work has to be paid for in terms of Fees,
- The Publisher can't be overwhelmed by 'free' operations,
- The Fee can be collected by the Publisher only after the Submitter successfully submitted the Fact Statement transaction,
- Consumer Plutus programs can verify that a certain Fact Statement is signed by the Publisher

## Interaction

```mermaid
sequenceDiagram
  title Publishing a fact statement on Cardano
  actor factMp as $FACT Minting Policy
  actor feeMp as $FEE Minting Policy
  actor cardano as Cardano
  actor submitterWallet as Submitter's wallet
  actor submitter as Submitter
  actor publisher as Publisher
  actor publisherWallet as Publisher's wallet

  submitter ->>+ publisher: getCatalog() [no fee]
  note right of publisher: Talks to its backend to get the list of fact statement types
  publisher -->>- submitter: [{urn: "/climate/temp/usa", type: r, fee: 1 ADA},<br />{urn: "/climate/temp/uk", type: r, fee: 1 ADA}]
  note over submitter: Wants to publish the Temperature in UK at Thu, 21 Jul 2022 14:12:44 GMT

  submitter ->>+ publisher: createFeeTransaction(utxoWithFee)
  note right of publisher: Prepares a Fee Transaction<br/>feeTrx = <br/>{<br/>input = utxoWithFee,<br/>minted = 1 $FEE(publisher)<br/>outDatum = (publisher, sessionId),<br/>outValue = input.value + minted.value,<br/>outAddress = @FeeV<br/>}
    publisher ->>+ publisherWallet: signTrx(feeTrx)
    publisherWallet -->>- publisher: feeTrx {signatories += publisher}
  publisher -->>- submitter: feeTrx
  submitter ->>+ submitterWallet: signAndSubmit(feeTrx)
    submitterWallet ->>+ cardano: submit(feeTrx {signatories += submitter})
      cardano ->>+ feeMp: validate(feeTrx)
      note right of feeMp: Checks a Fee Transaction<br/>? OutAddr = @FeeV,<br/>(Publisher, _) = OutDatum,<br/>Publisher in Signatories,<br/>asset($FEE, Publisher, 1) in Minted,<br/>asset($FEE, Publisher, 1) in OutValue.
      feeMp -->>- cardano: Ok!
    cardano -->>- submitterWallet: feeTrxId
  submitterWallet -->>- submitter: feeTrxId
  
  submitter ->>+ publisher: createFactStatementTransaction(<br />feeTrxId=feeTrxId,<br />at=1658412764,<br />urn="/prices/goog")
  note right of publisher: Checks the feeTrx<br/>? TrxUtxos(feeTrxId, utxo(@FeeV, (publisher, sessionId), Value)),<br/>1 $FEE(publisher) in Value,<br/>1 ADA in Value
  note right of publisher: Prepares the Fact Statement transaction<br/>factStatementTrx = {<br/>minted = 1 $FACT(publisher), <br/>outDatum = (publisher, submitter, sessionId, factStatement),<br/>outAddress = @FactV}
    publisher ->>+ publisherWallet: signTrx(factStatementTrx)
    publisherWallet -->>- publisher: factStatementTrx {signatories += publisher}
  publisher -->>- submitter: factStatementTrx
  submitter ->>+ submitterWallet: signAndSubmit(factStatementTrx)
      submitterWallet ->>+ cardano: submit(factStatementTrx {signatories += submitter})
      cardano ->>+ factMp: validate(factStatementTrx)
      note right of factMp: Checks a Fact Statement Transaction<br/>? OutAddr = @FactV,<br/>(Publisher, _, _, _) = OutDatum,<br/>Publisher in Signatories,<br/>asset($FACT, Publisher, 1) in Minted,<br/>asset($FACT, Publisher, 1) in OutValue.
      factMp -->>- cardano: Ok!
    cardano -->>- submitterWallet: factStatementTrxId
  submitterWallet -->>- submitter: factStatementTrxId
```
