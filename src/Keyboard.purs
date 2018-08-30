module Keyboard where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Query.EventSource as ES
import Web.Event.EventTarget as ET
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

onKeyDown :: HTMLDocument -> (KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
onKeyDown document fn = do
  let target = HTMLDocument.toEventTarget document
  listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
  ET.addEventListener KET.keydown listener false target
  pure $ ET.removeEventListener KET.keydown listener false target

subscribeToKeydown query = do
  document <- H.liftEffect $ DOM.document =<< DOM.window
  H.subscribe $ ES.eventSource' (onKeyDown document) (Just <<< H.request <<< query)
