module Component where

import Keyboard (subscribeToKeydown)
import Prelude (class Ord, type (~>), Unit, bind, const, discard, map, negate, pure, show, unit, ($), (/), (<$), (<<<), (<>), (==))

import CSS (Transformation(..), fromString, left, position, px, relative, top, transform)
import Data.Foldable (all, foldl)
import Data.List (List)
import Data.Map (Map, alter, fromFoldable, lookup, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class.Console (log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record (class EqualFields)
import Stepper (Velocity(..), defaultStepper)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Web.UIEvent.MouseEvent (MouseEvent)
import Type.Row (class RowToList)

type State = {
  box :: Box,
  boxAnimation :: Animations
}

data Box = Left
  | Right
  | Top
  | Bottom

styleForBox :: Box -> Destinations
styleForBox box =
  case box of
    Left -> fromFoldable [Tuple "left" $ negate 300.0, Tuple "top" 0.0, Tuple "scale" 2.0]
    Right -> fromFoldable [Tuple "left" 300.0, Tuple "top" 0.0, Tuple "scale" 1.0]
    Top -> fromFoldable [Tuple "left" 0.0, Tuple "top" $ negate 300.0, Tuple "scale" 1.5]
    Bottom -> fromFoldable [Tuple "left" 0.0, Tuple "top" 300.0, Tuple "scale" 1.5]

data Query a
  = Step a
  | Init a
  | HandleMouseMove MouseEvent a
  | HandleKeyDown KeyboardEvent (H.SubscribeStatus -> a)
  | SetBox Box a

data Message = Toggled Boolean

ui :: H.Component HH.HTML Query Unit Message Aff
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = {
    box: Left,
    boxAnimation: mkAnimations $ styleForBox Left
  }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div [ HP.class_ (ClassName "main"), HE.onMouseMove $ HE.input HandleMouseMove ]
      [ HH.div [ HP.class_ $ ClassName "box-container"] [
        HH.div [
            HP.class_ (ClassName "box"),
            CSS.style do
              position relative
              left (px $ fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "left" state.boxAnimation)
              top (px $ fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "top" state.boxAnimation)
              transform $ Transformation $ fromString $ "scale(" <> show (fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "scale" state.boxAnimation) <> ")"
          ] [
          ]
        ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval = case _ of
    Init next -> do
      _ <- subscribeToKeydown HandleKeyDown
      pure next
    HandleMouseMove mouseEvent next -> do
      pure next

    HandleKeyDown keyEvent reply -> do
      H.liftEffect $ log (key keyEvent)
      reply H.Listening <$ case key keyEvent of
        "ArrowUp" -> eval $ SetBox Top unit
        "ArrowLeft" -> eval $ SetBox Left unit
        "ArrowDown" -> eval $ SetBox Bottom unit
        "ArrowRight" -> eval $ SetBox Right unit
        _ -> pure unit

    SetBox box next -> do
      st <- H.get
      H.modify_ _ { box = box, boxAnimation = updateAnimations (styleForBox box) st.boxAnimation }
      if allStopped st.boxAnimation
        then
          eval $ Step next
        else
          pure next

    Step next -> do
      animations <- H.gets _.boxAnimation
      let animations' = stepAnimations animations

      H.modify_ _ { boxAnimation = animations' }
      if allStopped animations'
        then
          pure next
        else do
          _ <- H.fork $ do
            H.liftAff $ delay $ Milliseconds (1000.0 / 60.0)
            eval $ Step next
          pure next


-- ANMIMATIONS

-- type Dest d = { dest :: Number | d }

class EqualFields (rs :: RowList) (row :: # Type) | rs -> row where
  equalFields :: RLProxy rs -> Record row -> Record row -> Boolean

instance equalFieldsCons
  ::
  ( IsSymbol name
  , Eq ty
  , Cons name ty tailRow row
  , EqualFields tail row
  ) => EqualFields (Cons name ty tail) row where
  equalFields _ a b = get' a == get' b && equalRest a b
    where
      get' = get (SProxy :: SProxy name)
      equalRest = equalFields (RLProxy :: RLProxy tail)

instance equalFieldsNil :: EqualFields Nil row where
  equalFields _ _ _ = true

type Thing b = Record b
equal
  :: forall r rs
   . RowToList r rs
  => EqualFields rs r
  => Record r
  -> Record r
  -> Record r
equal a b = a

thing :: { string :: String
}
thing = equal { string: "hi"} { string: "No"}

-- dance :: Record String
-- dance  = { sheep: "a"}
-- dance a b = a


newtype Animation = Animation {
  val :: Number,
  dest :: Number,
  velocity :: Velocity
}

derive instance newtypeAnimation :: Newtype Animation _

type Animations = Map String Animation
type Destinations = Map String Number

mkAnimations :: Destinations -> Animations
mkAnimations =
  map (\n -> Animation { val: n, dest: n, velocity: Velocity 0.0} )

stepAnimations :: Animations -> Animations
stepAnimations = map stepAnimaiton

allStopped :: Animations -> Boolean
allStopped = all isStopped

isStopped :: Animation -> Boolean
isStopped (Animation { velocity }) = velocity == Velocity 0.0

stepAnimaiton :: Animation -> Animation
stepAnimaiton (Animation {val, dest, velocity}) =
  let
    Tuple val' velocity' = defaultStepper val velocity dest
  in
    Animation { val: val', dest: dest, velocity: velocity' }

unionWith' :: forall k a b. Ord k => (a -> b -> b) -> Map k a -> Map k b -> Map k b
unionWith' f m1 m2 = foldl go m2 (toUnfoldable m1 :: List (Tuple k a))
  where
  go m (Tuple k a) = alter (map (f a)) k m

updateAnimations :: Destinations -> Animations -> Animations
updateAnimations =
  unionWith' (\dest' (Animation anim) -> Animation anim { dest = dest'} )
