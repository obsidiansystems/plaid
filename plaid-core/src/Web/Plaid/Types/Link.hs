{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Plaid.Types.Link where

import Data.Text (Text)
import GHC.Generics (Generic)

import Web.Plaid.Types.Core

-- TODO: Remove Plaid (even PlaidLink) prefix

data PlaidLinkInstitution = PlaidLinkInstitution
  { _plaidLinkInstitution_name :: !Text
  , _plaidLinkInstitution_id :: !Text
  } deriving (Eq, Generic, Show)

data PlaidLinkSuccess = PlaidLinkSuccess
  { _plaidLinkSuccess_publicToken :: !Text
  , _plaidLinkSuccess_institution :: !PlaidLinkInstitution
  , _plaidLinkSuccess_sessionId :: !Text
  } deriving (Eq, Generic, Show)

data PlaidLinkExit = PlaidLinkExit
  { _plaidLinkExit_error :: !(Maybe PlaidLinkError)
  , _plaidLinkExit_institution :: !(Maybe PlaidLinkInstitution)
  , _plaidLinkExit_status :: !(Maybe Text)
  , _plaidLinkExit_sessionId :: !Text
  } deriving (Eq, Generic, Show)

data PlaidLinkError = PlaidLinkError
  { _plaidLinkError_displayMessage :: !Text
  , _plaidLinkError_errorCode :: !Text
  , _plaidLinkError_errorMessage :: !Text
  , _plaidLinkError_errorType :: !Text
  } deriving (Eq, Generic, Show)

data PlaidLinkProduct
  = PlaidLinkProduct_Auth
  | PlaidLinkProduct_Identity
  | PlaidLinkProduct_Transactions
  deriving (Bounded, Enum, Eq, Ord, Show)

-- TODO: Use Types.Config
data PlaidLinkConfig = PlaidLinkConfig
  { _plaidLinkConfig_clientName :: !Text
  , _plaidLinkConfig_env :: !Environment
  , _plaidLinkConfig_publicKey :: !Text
  , _plaidLinkConfig_products :: ![PlaidLinkProduct]
  } deriving (Eq, Generic, Show)
