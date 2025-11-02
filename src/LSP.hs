{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Autodocodec as AC
import qualified Colog.Core as L
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Network.Socket
import Prettyprinter
import SKGraphSchema
import System.IO

diagramAcceptMethod :: SMethod (Method_CustomMethod "diagram/accept")
diagramAcceptMethod = (SMethod_CustomMethod (Proxy @"diagram/accept"))

dummyModel :: A.Value
dummyModel =
  A.object
    [ "clientId" .= T.pack "keith-diagram_sprotty",
      "action"
        .= A.object
          [ "kind" .= T.pack "setModel",
            "newRoot"
              .= A.object
                [ "type" .= T.pack "graph",
                  "revision" .= (0 :: Int),
                  "id" .= T.pack "file:///home/klara/git/plyghd-ls-demonstrator/empty.kgt",
                  "properties" .= (Seq.empty :: Seq.Seq A.Object),
                  "children"
                    .= Seq.fromList
                      [ A.object
                          [ "data" .= (Seq.empty :: Seq.Seq A.Object),
                            "type" .= T.pack "node",
                            "id" .= T.pack "$root",
                            "properties"
                              .= A.object
                                [],
                            "direction" .= (0 :: Int),
                            "selected" .= False,
                            "hoverFeedback" .= False,
                            "children"
                              .= Seq.fromList
                                [ A.object
                                    [ "data" .= (Seq.empty :: Seq.Seq A.Object),
                                      "type" .= T.pack "node",
                                      "id" .= T.pack "$root$Nactor1",
                                      "properties"
                                        .= A.object
                                          [],
                                      "children"
                                        .= Seq.fromList
                                          [ A.object
                                              [ "data"
                                                  .= Seq.fromList
                                                    [ A.object
                                                        [ "type" .= T.pack "KTextImpl",
                                                          "text" .= T.pack "actor_1",
                                                          "styles" .= ((Seq.empty) :: Seq.Seq A.Object),
                                                          "properties"
                                                            .= A.object
                                                              []
                                                        ]
                                                    ],
                                                "properties" .= A.object [],
                                                "type" .= T.pack "label",
                                                "id" .= T.pack "$root$Nactor1$$L0",
                                                "children" .= A.object []
                                              ]
                                          ]
                                    ]
                                ]
                          ]
                      ]
                ]
          ]
    ]

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        let params =
              ShowMessageRequestParams
                MessageType_Info
                "Turn on code lenses?"
                (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
        _ <- sendRequest SMethod_WindowShowMessageRequest params $ \case
          Right (InL (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions (InR Null) Nothing (Just False)

            _ <- registerCapability mempty SMethod_TextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder $ Right $ InL rsp
            pure ()
          Right _ ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "Not turning on code lenses")
          Left err ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error $ "Something went wrong!\n" <> T.pack (show err))
        pure (),
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover (InL ms) (Just range)
            ms = mkMarkdown "Hello world"
            range = Range pos pos
        responder (Right $ InL rsp),
      notificationHandler diagramAcceptMethod $ \_not -> do
        sendNotification diagramAcceptMethod (dummyModel)
        -- sendNotification diagramAcceptMethod (tests)
        pure ()
        -- requestHandler (SMethod_CustomMethod (Proxy @"diagram/accept")) $ \req resp -> do
        --   pure ()
    ]

runServerC :: Handle -> Handle -> ServerDefinition config -> IO Int
runServerC =
  runServerWithHandles
    (L.cmap (fmap $ T.pack . show . pretty) (L.cmap show L.logStringStderr))
    (L.cmap (fmap $ T.pack . show . pretty) (L.cmap show L.logStringStderr))

main :: IO Int
main =
  runTCPServer (Just "127.0.0.1") "5007" lsp
  where
    lsp s = do
      handle <- socketToHandle s ReadWriteMode
      runServerC handle handle $
        ServerDefinition
          { parseConfig = const $ const $ Right (),
            onConfigChange = const $ pure (),
            defaultConfig = (),
            configSection = "demo",
            doInitialize = \env _req -> pure $ Right env,
            staticHandlers = \_caps -> handlers,
            interpretHandler = \env -> Iso (runLspT env) liftIO,
            options = defaultOptions
          }

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a1) -> IO a2
runTCPServer host port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      NE.head <$> getAddrInfo (Just hints) host (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)
