{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Plaid.Types.Api where

import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics

import Web.Plaid.Internal
import Web.Plaid.Types.Core

-- | A transaction. Example below
-- {
--     "account_id": "vokyE5Rn6vHKqDLRXEn5fne7LwbKPLIXGK98d",
--     "amount": 2307.21,
--     "category": [
--       "Shops",
--       "Computers and Electronics"
--     ],
--     "category_id": "19013000",
--     "date": "2017-01-29",
--     "location": {
--      "address": "300 Post St",
--      "city": "San Francisco",
--      "state": "CA",
--      "zip": "94108",
--      "lat": null,
--      "lon": null
--     },
--     "name": "Apple Store",
--     "payment_meta": Object,
--     "pending": false,
--     "pending_transaction_id": null,
--     "account_owner": null,
--     "transaction_id": "lPNjeW1nR6CDn5okmGQ6hEpMo4lLNoSrzqDje",
--     "transaction_type": "place"
-- }
data Transaction = Transaction
  { _transaction_accountId :: Text
  , _transaction_amount :: Money
  , _transaction_category :: Maybe [Text]
  , _transaction_categoryId :: Maybe Text
  , _transaction_date :: Date
  , _transaction_location :: Location
  , _transaction_name :: Text
  , _transaction_paymentMeta :: M.Map Text (Maybe Text)
  , _transaction_pending :: Bool
  , _transaction_pendingTransactionId :: Maybe Text
  , _transaction_accountOwner :: Maybe Text
  , _transaction_transactionId :: Text
  , _transaction_transactionType :: Text
  }
  deriving (Eq, Generic, Show)

data Location = Location
  { _location_address :: Maybe Text
  , _location_city :: Maybe Text
  , _location_state :: Maybe Text
  , _location_zip :: Maybe Text
  , _location_lat :: Maybe Text
  , _location_lon :: Maybe Text
  }
  deriving (Eq, Generic, Show)

-- | An account type. Example below:
-- {
-- "account_id": "6zPbKj8wa8c6pqXxA3pLCqZMelWXlLfjP1rND",
-- "balances":{"available": 200, "current": 210, "limit": null},
-- "mask": "1111",
-- "name": "Plaid Saving",
-- "official_name": "Plaid Silver Standard 0.1% Interest Saving",
-- "subtype": "savings",
-- "type": "depository"
-- },
data Account = Account
  { _account_accountId :: Text
  , _account_balances :: Balances
  , _account_mask :: Text
  , _account_name :: Text
  , _account_officialName :: Text
  , _account_subtype :: Text
  , _account_type :: Text
  }
  deriving (Eq, Generic, Show)

data Balances = Balances
  { _balances_available :: Maybe Money
  , _balances_current :: Money
  , _balances_limit :: Maybe Money
  }
  deriving (Eq, Generic, Show)

-- | An item. Example below
-- {
-- "available_products":[
-- "balance"
-- ],
-- "billed_products":[
-- "transactions"
-- ],
-- "error": null,
-- "institution_id": "ins_107039",
-- "item_id": "nKPQWazXyzc45mWZ8K5gSvDA9aodEMix9xM63",
-- "webhook": ""
-- }
data Item = Item
  { _item_availableProducts :: [Text]
  , _item_billedProducts :: [Text]
  , _item_error :: Maybe Text
  , _item_institutionId :: Text
  , _item_itemId :: Text
  , _item_webhook :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON Transaction where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON Location where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON Account where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON Balances where
  parseJSON = genericParseJSON fieldLabelMod
instance FromJSON Item where
  parseJSON = genericParseJSON fieldLabelMod

instance ToJSON Transaction where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON Location where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON Account where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON Balances where
  toJSON = genericToJSON fieldLabelMod
instance ToJSON Item where
  toJSON = genericToJSON fieldLabelMod
