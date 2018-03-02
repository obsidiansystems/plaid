{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Plaid.Types.Http where

import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics

import Web.Plaid.Internal
import Web.Plaid.Types.Core
import Web.Plaid.Types.Api

-- | Request body to exchange token. Example below:
-- @
-- {
--   "client_id": String,
--   "secret": String,
--   "public_token": "public-sandbox-5c224a01-8314-4491-a06f-39e193d5cddc"
-- }
-- @
data ExchangeTokenRequest = ExchangeTokenRequest
  { _exchangeTokenRequest_clientId :: Text
  , _exchangeTokenRequest_secret :: Text
  , _exchangeTokenRequest_publicToken :: Text
  }
  deriving (Eq, Generic, Show)

-- | Response body of exchanging token. Example below:
-- @
-- {
--   "access_token": "access-sandbox-de3ce8ef-33f8-452c-a685-8671031fc0f6",
--   "item_id": "M5eVJqLnv3tbzdngLDp9FL5OlDNxlNhlE55op",
--   "request_id": "Aim3b"
-- }
-- @
data ExchangeTokenResponse = ExchangeTokenResponse
  { _exchangeTokenResponse_accessToken :: Text
  , _exchangeTokenResponse_itemId :: Text
  , _exchangeTokenResponse_requestId :: Text
  }
  deriving (Eq, Generic, Show)

data PaginationOptions = PaginationOptions
  { _paginationOptions_count :: Int
  , _paginationOptions_offset :: Int
  }
  deriving (Eq, Generic, Show)

-- | Request body to get transactions. Example below:
-- {
--   "client_id": "5a8dd7dabdc6a47debd6efcf",
--   "secret": "fe1eeaf6119031b8bd76831b31cf6b",
--   "access_token": "access-sandbox-4754080b-79fd-482b-8fb4-0f4ce80b6158",
--   "start_date": "2017-01-01",
--   "end_date": "2017-02-01",
--   "options": {
--     "count": 250,
--     "offset": 100
--   }
-- }
data TransactionsRequest = TransactionsRequest
  { _transactionsRequest_clientId :: Text
  , _transactionsRequest_secret :: Text
  , _transactionsRequest_accessToken :: Text
  , _transactionsRequest_startDate :: Date
  , _transactionsRequest_endDate :: Date
  , _transactionsRequest_options :: PaginationOptions
  }
  deriving (Eq, Generic, Show)

-- | Response body of getting transactions. Example below
-- @
-- {
-- "accounts":[ ...
-- ],
-- "item":{ ...
-- },
-- "request_id": "Bo40m",
-- "total_transactions": 16,
-- "transactions":[]
-- }
-- @
data TransactionsResponse = TransactionsResponse
  { _transactionsResponse_accounts :: [Account]
  , _transactionsResponse_item :: Item
  , _transactionsResponse_requestId :: Text
  , _transactionsResponse_totalTransactions :: Int
  , _transactionsResponse_transactions :: [Transaction]
  }
  deriving (Eq, Generic, Show)


instance FromJSON ExchangeTokenRequest where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON ExchangeTokenResponse where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON TransactionsRequest where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON PaginationOptions where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON TransactionsResponse where
  parseJSON = genericParseJSON fieldLabelMod

instance ToJSON ExchangeTokenRequest where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON ExchangeTokenResponse where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON TransactionsRequest where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON PaginationOptions where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON TransactionsResponse where
  toJSON = genericToJSON fieldLabelMod
