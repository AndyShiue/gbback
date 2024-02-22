{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Text as T
import Language.LSP.Server
import Language.LSP.Types
--import Language.LSP.Protocol.Message

handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SInitialized $ \_notif -> do
      let params =
            ShowMessageRequestParams
              MtInfo
              "Hello, Guabao!"
              Nothing
      _ <- sendRequest SWindowShowMessageRequest params $ \case
          Right _ ->
            sendNotification SWindowShowMessage (ShowMessageParams MtInfo "Just saying hello again!")
          Left err ->
            sendNotification SWindowShowMessage (ShowMessageParams MtError $ "Something went wrong!\n" <> T.pack (show err))
      pure ()
  , requestHandler STextDocumentHover $ \_req _responder -> pure ()
  ]

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ pure $ Right ()
  , defaultConfig = ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
  }