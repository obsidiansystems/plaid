{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Plaid where

import Data.Aeson
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


-- TODO How to get public token?
-- POST /item/public_token/exchange {get item_id & access_token}

-- TODO: Implement
-- /auth/get
-- /transactions/get
-- /accounts/balance/get

demo :: IO (TransactionsResponse)
demo = runReq def $ do
  r <- req POST
    (plaidUrl Sandbox /: "transactions" /: "get")
    (ReqBodyJson Sample.transactionsRequest)
    jsonResponse
    mempty
  return $ responseBody r :: Req TransactionsResponse
