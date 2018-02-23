{-# LANGUAGE OverloadedStrings #-}

-- Sample data to feed to Plaid's sandbox environment
module Web.Plaid.Sample where

import Data.Text (Text)

import Web.Plaid.Types

-- XXX: Temporary access token until we implement the initial workflow of exchange
-- These are retrieved using the Python quickstart app on the Sandbox environment.
sandboxAccessToken :: Text
sandboxAccessToken = "access-sandbox-4754080b-79fd-482b-8fb4-0f4ce80b6158"
sandboxItemID :: Text
sandboxItemID = "nKPQWazXyzc45mWZ8K5gSvDA9aodEMix9xM63"

-- Sandbox keys from https://dashboard.plaid.com/overview/sandbox
clientId = "5a8dd7dabdc6a47debd6efcf"
secret = "fe1eeaf6119031b8bd76831b31cf6b"

transactionsRequest :: TransactionsRequest
transactionsRequest = TransactionsRequest
  clientId secret sandboxAccessToken "2017-01-01" "2017-02-01" paginationOptions

paginationOptions :: PaginationOptions
paginationOptions = PaginationOptions
  250 0
