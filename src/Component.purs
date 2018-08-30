module Component where


import Data.Record.Fold
import Record.Extra

import CSS (Transformation(..), animation, fromString, left, position, px, relative, top, transform)
import CSS.Common (initial)
import Data.BooleanAlgebra ((&&))
import Data.Foldable (all, foldl)
import Data.List (List)
import Data.Map (Map, alter, fromFoldable, lookup, toUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class.Console (log)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Keyboard (subscribeToKeydown)
import Prelude (class Ord, type (~>), Unit, bind, const, discard, map, negate, pure, show, unit, ($), (/), (<$), (<<<), (<>), (==))
import Record.Builder (Builder)
import Stepper (Velocity(..), defaultStepper)
import Type.Row (class RowToList)
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Web.UIEvent.MouseEvent (MouseEvent)

type BoxProperties a =
  { left :: a
  , rotate :: a
  , scale :: a
  , top :: a
  }

type DictateProperties a =
  { left :: a
  , scale :: a
  , top :: a
  }

type State =
  { position :: Position
  , boxAnimation :: BoxProperties Animation
  , dictateAnimation :: DictateProperties Animation
  }

data Position = Left
  | Right
  | Top
  | Bottom

styleBoxForPosition :: Position -> BoxProperties Number
styleBoxForPosition position =
  case position of
    Left -> {left: negate 300.0, top: 0.0, scale: 2.0, rotate: 0.0}
    Right -> {left: 300.0, top: 0.0, scale: 1.0, rotate: 180.0}
    Top -> {left: 0.0, top: negate 300.0, scale: 1.5, rotate: 90.0}
    Bottom -> {left: 0.0, top: 300.0, scale:1.5, rotate: negate 90.0}

styleDictateForPosition :: Position -> DictateProperties Number
styleDictateForPosition position =
  case position of
    Left -> { left: 0.0, top: 0.0, scale: 1.5 }
    Right -> {left: 300.0, top: 50.0, scale: 0.5}
    Top -> {left: 0.0, top: 300.0, scale: 1.2}
    Bottom -> {left: 0.0, top: 380.0, scale: 1.0}

dictateForPosition :: Position -> String
dictateForPosition position =
  case position of
    Left -> "Try the arrow keys."
    Right -> "I'm tiny!"
    Top -> "You'll never catch me!"
    Bottom -> "You caught me :("


initialState :: State
initialState = {
  position: Left,
  boxAnimation: makeAnimations $ styleBoxForPosition Left,
  dictateAnimation: makeAnimations $ styleDictateForPosition Left
}

data Query a
  = Step a
  | Init a
  | HandleMouseMove MouseEvent a
  | HandleKeyDown KeyboardEvent (H.SubscribeStatus -> a)
  | SetBox Position a

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


  render :: State -> H.ComponentHTML Query
  render st =
    HH.div [ HP.class_ (ClassName "main"), HE.onMouseMove $ HE.input HandleMouseMove ]
      [ HH.div [ HP.class_ $ ClassName "box-container"] [
        HH.div [
            HP.class_ (ClassName "box"),
            CSS.style do
              position relative
              left $ px st.boxAnimation.left.val
              top $ px st.boxAnimation.top.val
              transform $ Transformation $ fromString $ "scale(" <> show st.boxAnimation.scale.val <> ") " <> "rotate(" <> show st.boxAnimation.rotate.val <> "deg)"
          ] [
            HH.text "→"
          ]
        ],
        renderDictate st,
        HH.div [ HP.class_ (ClassName "attribution")] [
          HH.text "Made by ",
          HH.a [ HP.href "https://github.com/kitlangton/purescript-motion", HP.target "_blank"] [ HH.text "Kit Langton"]
        ]
      ]

  renderDictate :: State -> H.ComponentHTML Query
  renderDictate st =
    HH.div [ HP.class_ (ClassName "dictate") ] [
      HH.div [
       CSS.style do
         position relative
         left $ px st.dictateAnimation.left.val
         top $ px st.dictateAnimation.top.val
         transform $ Transformation $ fromString $ "scale(" <> show st.dictateAnimation.scale.val <> ")"
      ] [
        HH.text $ dictateForPosition st.position
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

    SetBox position next -> do
      st <- H.get
      H.modify_ _ {
        position = position
        , boxAnimation = updateAnimations (styleBoxForPosition position) st.boxAnimation
        , dictateAnimation = updateAnimations (styleDictateForPosition position) st.dictateAnimation
      }
      if allStopped st.boxAnimation
        then
          eval $ Step next
        else
          pure next

    Step next -> do
      boxAnimation <- H.gets _.boxAnimation
      let boxAnimation' = stepAnimations boxAnimation

      dictateAnimation <- H.gets _.dictateAnimation
      let dictateAnimation' = stepAnimations dictateAnimation

      H.modify_ _ { boxAnimation = boxAnimation', dictateAnimation = dictateAnimation' }
      if allStopped boxAnimation' && allStopped dictateAnimation'
        then
          pure next
        else do
          _ <- H.fork $ do
            H.liftAff $ delay $ Milliseconds (1000.0 / 60.0)
            eval $ Step next
          pure next


-- ANMIMATIONS


type Animation = {
  val :: Number,
  dest :: Number,
  velocity :: Velocity
}

-- ACTUAL

type RecordMapper a b = forall row row' xs .
  RowToList row xs =>
  MapRecord xs row a b () row' =>
  Record row -> Record row'

makeAnimations :: RecordMapper Number Animation
makeAnimations =
  mapRecord (\n -> { val: n, dest: n, velocity: Velocity 0.0} )

stepAnimations :: RecordMapper Animation Animation
stepAnimations = mapRecord stepAnimaiton

allStopped :: forall row row' xs xs' row''.
  RFold CollectS xs' row' (AppCat Maybe Builder {} { | row'' }) =>
  RowToList row xs =>
  RowToList row' xs' =>
  MapRecord xs row Animation (Maybe Boolean) () row' =>
  Record row -> Boolean
allStopped = isJust <<< collect <<< mapRecord (isStopped)

isStopped :: Animation -> Maybe Boolean
isStopped ({ velocity }) = if velocity == Velocity 0.0 then Just true else Nothing

stepAnimaiton :: Animation -> Animation
stepAnimaiton ({val, dest, velocity}) =
  let
    Tuple val' velocity' = defaultStepper val velocity dest
  in
    { val: val', dest: dest, velocity: velocity' }

unionWith' :: forall k a b. Ord k => (a -> b -> b) -> Map k a -> Map k b -> Map k b
unionWith' f m1 m2 = foldl go m2 (toUnfoldable m1 :: List (Tuple k a))
  where
  go m (Tuple k a) = alter (map (f a)) k m

updateAnimations :: forall xs a xs' b c xs''.
  RowToList a xs
  => RowToList b xs'
  => RowToList c xs''
  => MapRecord xs'' c (Tuple Number Animation) Animation () b
  => ZipRecord xs a xs' b () c
  => Record a
  -> Record b
  -> Record b
updateAnimations destinations animations =
  mapRecord (\(Tuple d v) -> v { dest = d }) $ zipRecord destinations animations
