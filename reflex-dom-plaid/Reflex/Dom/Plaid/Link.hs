{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Plaid.Link
  ( PlaidLinkConfig(..)
  , PlaidLinkEnvironment(..)
  , PlaidLinkError(..)
  , PlaidLinkExit(..)
  , PlaidLinkInstitution(..)
  , PlaidLinkProduct(..)
  , PlaidLinkSuccess(..)
  , plaidLinkDialog
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Lens.Operators ((^.))
import Control.Monad (join)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHCJS.DOM.Types (JSM, MonadJSM, fromJSValUnchecked, liftJSM)
import Language.Javascript.JSaddle.Object (fun, js, js0, js1, jsg, jss, obj)
import Language.Javascript.JSaddle.Value (maybeNullOrUndefined, valToText)

import Reflex.Dom.Core

data PlaidLinkInstitution = PlaidLinkInstitution
  { _plaidLinkInstitution_name :: !Text
  , _plaidLinkInstitution_id :: !Text
  } deriving (Eq, Generic, Show)

data PlaidLinkSuccess = PlaidLinkSuccess
  { _plaidLinkSuccess_publicToken :: !Text
  , _plaidLinkSuccess_institution :: !PlaidLinkInstitution
  , _plaidLinkSuccess_sessionId :: !Text
  } deriving (Eq, Generic, Show)

data PlaidLinkExit = PlaidLinkExit
  { _plaidLinkExit_error :: !(Maybe PlaidLinkError)
  , _plaidLinkExit_institution :: !(Maybe PlaidLinkInstitution)
  , _plaidLinkExit_status :: !(Maybe Text)
  , _plaidLinkExit_sessionId :: !Text
  } deriving (Eq, Generic, Show)

data PlaidLinkError = PlaidLinkError
  { _plaidLinkError_displayMessage :: !Text
  , _plaidLinkError_errorCode :: !Text
  , _plaidLinkError_errorMessage :: !Text
  , _plaidLinkError_errorType :: !Text
  } deriving (Eq, Generic, Show)


data PlaidLinkEnvironment
  = PlaidLinkEnvironment_Sandbox
  | PlaidLinkEnvironment_Development
  | PlaidLinkEnvironment_Production
  deriving (Bounded, Enum, Eq, Ord, Show)

data PlaidLinkProduct
  = PlaidLinkProduct_Auth
  | PlaidLinkProduct_Identity
  | PlaidLinkProduct_Transactions
  deriving (Bounded, Enum, Eq, Ord, Show)

data PlaidLinkConfig = PlaidLinkConfig
  { _plaidLinkConfig_clientName :: !Text
  , _plaidLinkConfig_env :: !PlaidLinkEnvironment
  , _plaidLinkConfig_publicKey :: !Text
  , _plaidLinkConfig_products :: ![PlaidLinkProduct]
  } deriving (Eq, Generic, Show)


plaidLinkDialog
  :: (TriggerEvent t m, PerformEvent t m, MonadJSM (Performable m))
  => Event t PlaidLinkConfig -> m (Event t (Either PlaidLinkExit PlaidLinkSuccess))
plaidLinkDialog open = do
  (onResultEvent, onResultCallback) <- newTriggerEvent

  performEvent_ $ ffor open $ \cfg -> liftJSM $
    activatePlaidLinkDialog cfg (liftIO . onResultCallback)

  pure onResultEvent


activatePlaidLinkDialog
  :: PlaidLinkConfig
  -> (Either PlaidLinkExit PlaidLinkSuccess -> JSM ())
  -> JSM ()
activatePlaidLinkDialog cfg onResult = do
  handleMVar <- liftIO newEmptyMVar
  o <- plaidLinkConfigAsObj
  o ^. jss (t_ "onSuccess") (fun $ \_ _ args -> case args of
    (pubTokenJs : metaJs : _) ->
      metaJs ^. js (t_ "institution") >>= mkInstitutionFromObj >>= \case
        Nothing -> liftIO $ putStrLn "Plaid institution was empty for successful response"
        Just institution -> do
          pubToken <- fromJSValUnchecked pubTokenJs
          sessionId <- maybeJs valToText =<< metaJs ^. js (t_ "link_session_id")
          onResult $ Right PlaidLinkSuccess
            { _plaidLinkSuccess_publicToken = pubToken
            , _plaidLinkSuccess_institution = institution
            , _plaidLinkSuccess_sessionId = fromMaybe "" sessionId
            }

    _ -> liftIO $ putStrLn "Plaid onSuccess called with unexpected number of arguments"
    )

  o ^. jss (t_ "onExit") (fun $ \_ _ args -> case args of
    (errJs : metaJs : _) -> do
      err <- maybeJs mkPlaidErrorFromObj errJs
      institution <- maybeJs mkInstitutionFromObj =<< metaJs ^. js (t_ "institution")
      sessionId <- maybeJs valToText =<< o ^. js (t_ "link_session_id")
      status <- maybeJs valToText =<< o ^. js (t_ "status")

      onResult $ Left PlaidLinkExit
        { _plaidLinkExit_error = err
        , _plaidLinkExit_institution = join institution
        , _plaidLinkExit_sessionId = fromMaybe "" sessionId
        , _plaidLinkExit_status = status
        }

    _ -> liftIO $ putStrLn "Plaid onExit called with unexpected number of arguments"
    )

  o ^. jss (t_ "onLoad") (fun $ \_ _ _ -> do
    handle <- liftIO (readMVar handleMVar)
    _ <- handle ^. js0 (t_ "open")
    pure ()
    )

  handle <- jsg (t_ "Plaid") ^. js1 (t_ "create") o
  liftIO $ putMVar handleMVar handle
  pure ()

  where
    plaidLinkConfigAsObj = do
      o <- obj
      o ^. jss (t_ "clientName") (_plaidLinkConfig_clientName cfg)
      o ^. jss (t_ "env") (plaidLinkEnvironmentAsText $ _plaidLinkConfig_env cfg)
      o ^. jss (t_ "key") (_plaidLinkConfig_publicKey cfg)
      o ^. jss (t_ "product") (plaidLinkProductAsText <$> _plaidLinkConfig_products cfg)
      pure o

    mkPlaidErrorFromObj o = do
      displayMessage <- valToText =<< o ^. js (t_ "display_message")
      errorCode <- valToText =<< o ^. js (t_ "error_code")
      errorMessage <- valToText =<< o ^. js (t_ "error_message")
      errorType <- valToText =<< o ^. js (t_ "error_type")
      pure PlaidLinkError
        { _plaidLinkError_displayMessage = displayMessage
        , _plaidLinkError_errorCode = errorCode
        , _plaidLinkError_errorMessage = errorMessage
        , _plaidLinkError_errorType = errorType
        }

    mkInstitutionFromObj o = do
      name' <- maybeJs valToText =<< o ^. js (t_ "name")
      id' <- maybeJs valToText =<< o ^. js (t_ "institution_id")
      pure $ case (name', id') of
        (Just name, Just id_) -> Just PlaidLinkInstitution
          { _plaidLinkInstitution_name = name
          , _plaidLinkInstitution_id = id_
          }
        _ -> Nothing

    maybeJs f x' = maybeNullOrUndefined x' >>= \case
      Nothing -> pure Nothing
      Just x -> Just <$> f x

    plaidLinkEnvironmentAsText :: PlaidLinkEnvironment -> Text
    plaidLinkEnvironmentAsText = \case
      PlaidLinkEnvironment_Sandbox -> "sandbox"
      PlaidLinkEnvironment_Development -> "development"
      PlaidLinkEnvironment_Production -> "production"

    plaidLinkProductAsText :: PlaidLinkProduct -> Text
    plaidLinkProductAsText = \case
      PlaidLinkProduct_Auth -> "auth"
      PlaidLinkProduct_Identity -> "identity"
      PlaidLinkProduct_Transactions -> "transactions"

    t_ :: Text -> Text
    t_ = id
