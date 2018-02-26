{-# LANGUAGE OverloadedStrings #-}

-- Sample data to feed to Plaid's sandbox environment
module Web.Plaid.Sample where

import Data.Text (Text)
import Data.Time (fromGregorian)

import Web.Plaid.Types

-- XXX: Temporary access token until we implement the initial workflow of exchange
-- These are retrieved using the Python quickstart app on the Sandbox environment.
-- https://plaid.com/docs/api/#integrating-with-link
accessToken :: Text
accessToken = "access-sandbox-4754080b-79fd-482b-8fb4-0f4ce80b6158"
itemID :: Text
itemID = "nKPQWazXyzc45mWZ8K5gSvDA9aodEMix9xM63"
-- Howto:
-- 1. Use Plaid Link to get the public token for the user-selected bank
-- 2. Use the exchange API to exchange the public token for the above pair

-- Sandbox keys from https://dashboard.plaid.com/overview/sandbox
clientId = "5a8dd7dabdc6a47debd6efcf"
secret = "fe1eeaf6119031b8bd76831b31cf6b"

transactionsRequest :: TransactionsRequest
transactionsRequest = TransactionsRequest
  clientId secret accessToken start end paginationOptions
  where
    start = fromGregorian 2017 1 1
    end = fromGregorian 2017 2 1

paginationOptions :: PaginationOptions
paginationOptions = PaginationOptions
  250 0
