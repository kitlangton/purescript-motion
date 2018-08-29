module Component where

import Prelude

import CSS (left, position, px, relative)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Stepper (Velocity(..), defaultStepper)

type State = {
  x :: Number,
  destX :: Number,
  velocity :: Velocity
}

data Query a
  = Step a

data Message = Toggled Boolean

ui :: H.Component HH.HTML Query Unit Message Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = {
    x: 1.0,
    destX: 100.0,
    velocity: Velocity 0.0
  }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.button
        [ HE.onClick $ HE.input_ Step ]
        [ HH.text "hi" ]
      , HH.div [
          HP.class_ (ClassName "box"),
          CSS.style do
            position relative
            left (px state.x)
        ] [
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval = case _ of
    Step next -> do
      st <- H.get
      let Tuple x' velocity' = defaultStepper st.x st.velocity st.destX
      H.modify_ _ { x = x', velocity = velocity'}
      if velocity' == Velocity 0.0
        then
          pure next
        else do
          _ <- H.fork $ do
            H.liftAff $ delay $ Milliseconds (1000.0 / 60.0)
            eval $ Step next
          pure next
