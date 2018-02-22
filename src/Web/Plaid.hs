{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Plaid where

import Data.Text (Text)

import Web.Plaid.Types

data Environment
  = Sandbox
  | Development
  | Production

environmentUrl :: Environment -> Text
environmentUrl = \case
  Sandbox -> "https://sandbox.plaid.com"
  Development -> "https://development.plaid.com"
  Production -> "https://production.plaid.com"


-- TODO How to get public token?
-- POST /item/public_token/exchange {get item_id & access_token}

-- TODO: Implement
-- /auth/get
-- /transactions/get
-- /accounts/balance/get

-- XXX: Temporary access token until we implement the initial workflow of exchange
-- These are retrieved uisng the Python quickstart app on the Sandbox environment.
sandboxAccessToken :: Text
sandboxAccessToken = "access-sandbox-4754080b-79fd-482b-8fb4-0f4ce80b6158"
sandboxItemID :: Text
sandboxItemID = "nKPQWazXyzc45mWZ8K5gSvDA9aodEMix9xM63"
