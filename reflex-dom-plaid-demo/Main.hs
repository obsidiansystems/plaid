{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom

import Reflex.Dom.Plaid.Link
import qualified Web.Plaid.Types as Plaid

main :: IO ()
main = mainWidgetWithHead headSection $ do
  el "label" (text "Public Key")
  pubKeyDyn <- _textInput_value <$> textInput def

  el "hr" blank

  go <- button "Open Plaid Dialog"
  dyn_ $ ffor pubKeyDyn $ \pubKey -> do
    text "You can use 'user_good' / 'pass_good' for test credentials in the sandbox"
    el "br" blank
    done <- plaidLinkDialog (Plaid.LinkConfig
      { Plaid._linkConfig_clientName = "Test"
      , Plaid._linkConfig_env = Plaid.Sandbox
      , Plaid._linkConfig_publicKey = pubKey
      , Plaid._linkConfig_products = [Plaid.Product_Transactions]
      } <$ go)

    el "br" blank

    display =<< holdDyn "Not done" (show <$> done)

  where
    headSection :: DomBuilder t m => m ()
    headSection =
      elAttr "script" ("src"=:"https://cdn.plaid.com/link/v2/stable/link-initialize.js") blank
