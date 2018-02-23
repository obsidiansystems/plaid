{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Plaid where

import Data.Text (Text)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Default.Class
import Network.HTTP.Req

import Web.Plaid.Types
import qualified Web.Plaid.Sample as Sample

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

demo :: IO (TransactionsResponse)
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
demo = runReq def $ do
  let payload = object
        [ "foo" .= (10 :: Int)
        , "bar" .= (20 :: Int) ]
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <- req POST -- method
    (https "sandbox.plaid.com" /: "transactions" /: "get") -- safe by construction URL
    (ReqBodyJson Sample.transactionsRequest) -- use built-in options or add your own
    jsonResponse -- specify how to interpret response
    mempty       -- query params, headers, explicit port number, etc.
  return $ responseBody r :: Req TransactionsResponse
