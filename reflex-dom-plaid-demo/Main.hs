{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom

import Reflex.Dom.Plaid.Link


main :: IO ()
main = mainWidgetWithHead headSection $ do
  el "label" (text "Public Key")
  pubKeyDyn <- _textInput_value <$> textInput def

  el "hr" blank

  go <- button "Open Plaid Dialog"
  dyn_ $ ffor pubKeyDyn $ \pubKey -> do
    text "You can use 'user_good' / 'pass_good' for test credentials in the sandbox"
    el "br" blank
    done <- plaidLinkDialog (PlaidLinkConfig
      { _plaidLinkConfig_clientName = "Test"
      , _plaidLinkConfig_env = PlaidLinkEnvironment_Sandbox
      , _plaidLinkConfig_publicKey = pubKey
      , _plaidLinkConfig_products = [PlaidLinkProduct_Transactions]
      } <$ go)

    el "br" blank

    display =<< holdDyn "Not done" (show <$> done)

  where
    headSection :: DomBuilder t m => m ()
    headSection =
      elAttr "script" ("src"=:"https://cdn.plaid.com/link/v2/stable/link-initialize.js") blank
