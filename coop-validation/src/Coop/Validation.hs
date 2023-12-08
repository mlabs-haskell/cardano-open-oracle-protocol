{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Coop.Validation (mkAuthenticationPolicy, mkCertificatePolicy, mkFactStatementPolicy, mkMainPolicy, mkCertificateValidator, mkMainValidator) where

import Coop.Validation.Policies.Authentication as AuthPol (
  mkPolicy,
 )
import Coop.Validation.Policies.Certificate as CertPol (mkPolicy)
import Coop.Validation.Policies.FactStatement as FactStatPol (
  mkPolicy,
 )
import Coop.Validation.Policies.Main as MainPol (mkPolicy)
import Coop.Validation.Validators.Certificate as CertVal (
  mkValidator,
 )
import Coop.Validation.Validators.Main as MainVal (mkValidator)

mkAuthenticationPolicy = AuthPol.mkPolicy

mkCertificatePolicy = CertPol.mkPolicy

mkFactStatementPolicy = FactStatPol.mkPolicy

mkMainPolicy = MainPol.mkPolicy

mkCertificateValidator = CertVal.mkValidator

mkMainValidator = MainVal.mkValidator
