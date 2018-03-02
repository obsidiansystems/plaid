{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Plaid.Types.Link where

import Data.Text (Text)
import GHC.Generics (Generic)

import Web.Plaid.Types.Core

data LinkInstitution = LinkInstitution
  { _linkInstitution_name :: !Text
  , _linkInstitution_id :: !Text
  } deriving (Eq, Generic, Show)

data LinkSuccess = LinkSuccess
  { _linkSuccess_publicToken :: !Text
  , _linkSuccess_institution :: !LinkInstitution
  , _linkSuccess_sessionId :: !Text
  } deriving (Eq, Generic, Show)

data LinkExit = LinkExit
  { _linkExit_error :: !(Maybe LinkError)
  , _linkExit_institution :: !(Maybe LinkInstitution)
  , _linkExit_status :: !(Maybe Text)
  , _linkExit_sessionId :: !Text
  } deriving (Eq, Generic, Show)

data LinkError = LinkError
  { _linkError_displayMessage :: !Text
  , _linkError_errorCode :: !Text
  , _linkError_errorMessage :: !Text
  , _linkError_errorType :: !Text
  } deriving (Eq, Generic, Show)

data LinkConfig = LinkConfig
  { _linkConfig_clientName :: !Text
  , _linkConfig_env :: !Environment
  , _linkConfig_publicKey :: !Text
  , _linkConfig_products :: ![Product]
  } deriving (Eq, Generic, Show)

-- TODO: port demo example
