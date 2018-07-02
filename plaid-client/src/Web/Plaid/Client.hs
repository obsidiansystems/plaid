{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Plaid.Client (
  getTransactions,
  exchangeToken,
  run)
  where

import Data.Default.Class
import Data.Text (Text)
import Network.HTTP.Req

import Web.Plaid.Types

plaidUrl :: Environment -> Url 'Https
plaidUrl = \case
  Sandbox -> https "sandbox.plaid.com"
  Development -> https "development.plaid.com"
  Production -> https "production.plaid.com"

-- | Get transactions for an item
getTransactions :: Config -> Text -> Date -> Date -> PaginationOptions -> Req TransactionsResponse
getTransactions config accessToken startDate endDate paginationOptions =
  responseBody <$> req POST url body jsonResponse mempty
    where
      url = plaidUrl (_config_env config) /: "transactions" /: "get"
      body = ReqBodyJson r
      r = TransactionsRequest
        (_config_clientId config)
        (_config_secret config)
        accessToken
        startDate
        endDate
        paginationOptions

-- | Exchange the public token for access token and item id
exchangeToken :: Config -> Text -> Req ExchangeTokenResponse
exchangeToken config publicToken =
  responseBody <$> req POST url body jsonResponse mempty
    where
      url = plaidUrl (_config_env config) /: "item" /: "public_token" /: "exchange"
      body = ReqBodyJson r
      r = ExchangeTokenRequest
        (_config_clientId config)
        (_config_secret config)
        publicToken

run :: Req a -> IO a
run = runReq def
