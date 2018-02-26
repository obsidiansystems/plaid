{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Plaid where

import Data.Default.Class
import Data.Text (Text)
import Network.HTTP.Req

import qualified Web.Plaid.Sample as Sample
import Web.Plaid.Types

plaidUrl :: Environment -> Url 'Https
plaidUrl = \case
  Sandbox -> https "sandbox.plaid.com"
  Development -> https "development.plaid.com"
  Production -> https "production.plaid.com"

-- | Get transactions for an item
getTransactions :: Environment -> TransactionsRequest -> Req TransactionsResponse
getTransactions env r = responseBody <$> req POST url body jsonResponse mempty
  where url = plaidUrl env /: "transactions" /: "get"
        body = ReqBodyJson r

-- | Exchange the public token for access token and item id
exchangeToken :: Environment -> ExchangeTokenRequest -> Req ExchangeTokenResponse
exchangeToken env r = responseBody <$> req POST url body jsonResponse mempty
  where url = plaidUrl env /: "item" /: "public_token" /: "exchange"
        body = ReqBodyJson r

demo :: IO (TransactionsResponse)
demo = runReq def $ do
  getTransactions Sandbox Sample.transactionsRequest
