---
tag:              "docs-00-design"
date:             2022-06-28
revision:         1
revision-notes:   "Updated the document's structure"
author:           FIXME
reviewers:        FIXME
status:           WIP
---

# Design Document

For the Cardano open oracle protocol, this document describes the design goals,
options considered, and the rationale for the design option selected for
implementation.

**Table of contents**

- [Design Document](#design-document)
  - [Considered Designs](#considered-designs)
  - [Orcfax On-Chain Bulletin Board](#orcfax-on-chain-bulletin-board)
    - [Summary](#summary)
    - [Context](#context)
    - [Description](#description)
      - [Example](#example)
    - [Querying the Oracle](#querying-the-oracle)
    - [Providing the Transaction to be Signed](#providing-the-transaction-to-be-signed)
    - [Posting the Transaction](#posting-the-transaction)
    - [User Motivations](#user-motivations)
  - [Resources](#resources)

## Considered Designs

As part of the design process, diverse ideas have been considered, with their
pros and cons discussed. These ideas are captured by the following Architectural Design Records (aka. ADRs):

  1. On-Chain Database discussed in [(docs-proposals-00-onchain-db)](./proposals/00-onchain-db.md)
  2. Off-Chain Database with On-Chain knowledge representation discussed in [(docs-proposals-01-offchain-db-mtree)](./proposals/01-offchain-db-mtree.md)
  3. [Off-Chain Database using signature scheme to validate correct information](./proposals/02-signature-scheme.md)

To summarise the above findings, the document
[docs-proposals-03-comparative-summary](./proposals/03-comparative-summary.md)
has been set-up, and captures a more condensed, comparison based critical view
of the above ideas. Following the above exploration an emergent design has been
reached, which will now be further expanded on in this document.

---

## Orcfax On-Chain Bulletin Board

### Summary

The Orcfax On-Chain Bulletin Board is an emerging, novel on-chain Oracle design.
The protocol, offers a sustainable way of posting trust-maintaining, verified
information on-chain, in a decentralised manner. Furthermore, the protocol
leverages the features introduced by the Vasil hardfork, allowing for seamless
protocol interoperability. Lastly the Orcfax proposal maintains a well defined
revenue stream for the Oracle providers. We also believe that the proposal is
scalable and can be adopted by many Oracle providers, enabling a well
distributed framework of decentralised information.

### Context

FIXME: add more about the context of:
- hardfork
- a common Oracle design pre-hardfork

### Description

The Orcfax proposal can be summarised as follows:

> Users post Oracle approved information on-chain for themselves and other users
to use in their own transactions. The Orcfax protocol specifies: how information
is received by the user, how information is verified by the Oracle, a
standardised format for the transaction and resulting EUTxO, how the information
is consumed, and how the Oracle gets compensated for the Service.

Throughout the document we will make use of scenario based examples to clarify
the use-cases in discussion.

![orcfax diagram](.imgs/Orcfax-orcfax-diagram.png)

#### Example

> Imagine a public bulletin board in the Roman forum. 
>
> Citizens know to come look
> at the board to find out the latest information that they care about.
>
> For example, if someone wants to refer to the current imperial price of gold,
> all they have to do is point their companion to the board.
>
> To prevent the board from being overcrowded with messages, each message posted
> must have a deposit of 2 denarii attached.
>
> Of course, the imperial administration itself can't be bothered to actually
> post the price on the board. Instead, if citizens want the current price to be
> visible on the board, they must go to the administration and get a signed
> message about the current price—paying the government for this service—and
> then post it themselves together with the required deposit.
>
> Later on, they can come back to rip down the message from the board and
> recover the deposit. To ensure that the contents of the board are relatively
> stable, messages must stay on the board for some minimum period of time. You
> wouldn't want to tell your companion to go to the forum and verify that you're
> telling the truth about the price, only for the message to be ripped down when
> they get there.
>
> Perhaps, when a benevolent imperator is in power, he may instruct his
> administration to waive the fee for signature and maybe even to subsidize the
> citizen's deposit for posting on the board. If he's feeling particularly
> generous, he can instruct the administration to just sign and post the
> messages themselves on the board.

### Querying the Oracle

To receive some information held by the Oracle, a User can query the Oracle via an offchain API (offchain) to receive the information that the Oracle would agree to sign. This communication is standardised and includes any required fee, expiration date, and Time To Live (TTL) information regarding the posted information. It is at the latitude of the User if they want to proceed to create a transaction with the received information or not. 

### Providing the Transaction to be Signed

The User can provide the information back to the Oracle, having formulated the transaction as already deemed acceptable. The Oracle now signs this transaction and posts it back to the User.

### Posting the Transaction
With the Oracle signed transaction, all that the user must do is to.

### User Motivations
FIXME

---

## Resources

- Leonard Lys and Maria Potop-Butucaru, Distributed Blockchain Price Oracle,
  <https://eprint.iacr.org/2022/603.pdf>
