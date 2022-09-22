# COOP onchain architecture

```mermaid
graph LR
    AA($AA Wallet) -->|$AA x aaQ| MintAuthCertTrx{Mint $AUTH + Mint $CERT}
    MintAuthCertTrx -->|$AA x aaQ| AA
    MintAuthCertTrx -->|$AUTH/ID x authQ | AuthWallet($AUTH + $CERT_RDMR Wallet)
    MintAuthCertTrx -->|"$CERT/ID x 1 + (validity, $CERT-RDMR)"| CertV("@CertV")
    
    CertV -.->|"$CERT/ID x 1 + (validity, $CERT-RDMR)"| MintFsTrx{Mint $FS + Burn $AUTH}
    CertV -->|"$CERT/ID x 1+ (validity, $CERT-RDMR)"| BurnCertTrx{Burn $CERT}
    
    AuthWallet -->|$AUTH/ID x 1| MintFsTrx
    AuthWallet -->|$AUTH/ID x unused| BurnAuthTrx{Burn $AUTH}
    AuthWallet -->|"$CERT_RDMR x 1+"| BurnCertTrx
    
    BurnCertTrx --> |$CERT_RDMR x 1+| AuthWallet
    
    SubmitterWallet -->|$FEE x feeQ| MintFsTrx
    SubmitterWallet -->|Signature| BurnFsTrx
    
    MintFsTrx -->|$FS/ID x 1 + Fact Statement| FsV("@FsV")
    MintFsTrx -->|$FEE x feeQ| FeeWallet("Fee Wallet")

    FsV -.->|$FS/ID x 1 + Fact Statement| Consumer1{Consumer1}
    FsV -.->|$FS/ID x 1 + Fact Statement| Consumer2{Consumer2}
    FsV -->|$FS/ID x 1 + Fact Statement| BurnFsTrx{Burn $FS}

    BurnFsTrx --> |$ADA x minUtxoAda| SubmitterWallet
```

Legend:

- Rectangle - something with an Address (Validator or Wallet)
- Diamond - transaction
- Line - consumes TxOut
- Dotted - references TxOut
- $CurrencySymbol/TokenName x Quantity + Datum

## Tokens

### Authentication authority token - $AA

### Certificate token - $CERT

### Certificate redeemer token - $CERT-RDMR

> INFO[Andrea]: instead of a $CERT-RDMR token you could just have a
> PKH in the @CertV datum, and check the burn $CERT transaction is
> signed by that, like you do for burning $FS tokens and submitters.
> This would only grant the PKH access to a small amount of Ada, so it
> does not seem like the extra token indirection is warranted.

### Authentication token - $AUTH

### Fact statement token - $FS

## Scripts

### CertMp

### @CertV

### AuthMp

### FsMp

### @FsV

## Wallets

### God wallet

### $AA wallet

### $AUTH and $CERT-RDMR wallet

### $FEE wallet

## References

- [Eternal keys considered harmful #34](https://github.com/mlabs-haskell/cardano-open-oracle-protocol/issues/34)
