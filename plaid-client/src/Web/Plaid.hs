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

-- TODO Exchange API
-- POST /item/public_token/exchange {get item_id & access_token}

demo :: IO (TransactionsResponse)
demo = runReq def $ do
  r <- req POST
    (plaidUrl Sandbox /: "transactions" /: "get")
    (ReqBodyJson Sample.transactionsRequest)
    jsonResponse
    mempty
  return $ responseBody r :: Req TransactionsResponse
