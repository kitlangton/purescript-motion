module Component where

import CSS (Transformation(..), color, fromString, left, position, px, relative, top, transform)
import CSS as CSS
import Color.Scheme.Clrs (blue)
import Data.BooleanAlgebra ((&&))
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
import Keyboard (subscribeToKeydown)
import Prelude (class Ord, type (~>), Unit, bind, const, discard, map, negate, pure, show, unit, ($), (/), (<$), (<<<), (<>), (==))
import Record (class EqualFields)
import Stepper (Velocity(..), defaultStepper)
import Type.Row (class RowToList)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Web.UIEvent.MouseEvent (MouseEvent)

type State = {
  position :: Position,
  boxAnimation :: Animations,
  dictateAnimation :: Animations
}

data Position = Left
  | Right
  | Top
  | Bottom

styleBoxForPosition :: Position -> Destinations
styleBoxForPosition position =
  case position of
    Left -> fromFoldable [Tuple "left" $ negate 300.0, Tuple "top" 0.0, Tuple "scale" 2.0, Tuple "rotate" 0.0]
    Right -> fromFoldable [Tuple "left" 300.0, Tuple "top" 0.0, Tuple "scale" 1.0, Tuple "rotate" 180.0]
    Top -> fromFoldable [Tuple "left" 0.0, Tuple "top" $ negate 300.0, Tuple "scale" 1.5, Tuple "rotate" 90.0]
    Bottom -> fromFoldable [Tuple "left" 0.0, Tuple "top" 300.0, Tuple "scale" 1.5, Tuple "rotate" $ negate 90.0]

styleDictateForPosition :: Position -> Destinations
styleDictateForPosition position =
  case position of
    Left -> fromFoldable [Tuple "left" $ 0.0, Tuple "top" 0.0, Tuple "scale" 1.5]
    Right -> fromFoldable [Tuple "left" $ 300.0, Tuple "top" 50.0, Tuple "scale" 0.5]
    Top -> fromFoldable [Tuple "left" 0.0, Tuple "top" $ 300.0, Tuple "scale" 1.2]
    Bottom -> fromFoldable [Tuple "left" 0.0, Tuple "top" 380.0, Tuple "scale" 1.0]

dictateForPosition :: Position -> String
dictateForPosition position =
  case position of
    Left -> "Try the arrow keys."
    Right -> "I'm tiny!"
    Top -> "You'll never catch me!"
    Bottom -> "You caught me :("

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

  initialState :: State
  initialState = {
    position: Left,
    boxAnimation: mkAnimations $ styleBoxForPosition Left,
    dictateAnimation: mkAnimations $ styleDictateForPosition Left
  }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div [ HP.class_ (ClassName "main"), HE.onMouseMove $ HE.input HandleMouseMove ]
      [ HH.div [ HP.class_ $ ClassName "box-container"] [
        HH.div [
            HP.class_ (ClassName "box"),
            CSS.style do
              position relative
              left (px $ fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "left" st.boxAnimation)
              top (px $ fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "top" st.boxAnimation)
              transform $ Transformation $ fromString $ "scale(" <> show (fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "scale" st.boxAnimation) <> ") " <> "rotate(" <> show (fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "rotate" st.boxAnimation) <> "deg)"
          ] [
            HH.text "→"
          ]
        ],
        renderDictate st,
        HH.div [ HP.class_ (ClassName "attribution")] [
          HH.text "Made by ",
          HH.a [ HP.href "http://github.com/kitlangton", HP.target "_blank"] [ HH.text "Kit Langton"]
        ]
      ]

  renderDictate :: State -> H.ComponentHTML Query
  renderDictate st =
    HH.div [ HP.class_ (ClassName "dictate") ] [
      HH.div [
       CSS.style do
         position relative
         left (px $ fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "left" st.dictateAnimation)
         top (px $ fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "top" st.dictateAnimation)
         transform $ Transformation $ fromString $ "scale(" <> show (fromMaybe 0.0 $ map (_.val <<< unwrap) $ lookup "scale" st.dictateAnimation) <> ")"
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

-- type Dest d = { dest :: Number | d }
--
-- class EqualFields (rs :: RowList) (row :: # Type) | rs -> row where
--   equalFields :: RLProxy rs -> Record row -> Record row -> Boolean
--
-- instance equalFieldsCons
--   ::
--   ( IsSymbol name
--   , Eq ty
--   , Cons name ty tailRow row
--   , EqualFields tail row
--   ) => EqualFields (Cons name ty tail) row where
--   equalFields _ a b = get' a == get' b && equalRest a b
--     where
--       get' = get (SProxy :: SProxy name)
--       equalRest = equalFields (RLProxy :: RLProxy tail)
--
-- instance equalFieldsNil :: EqualFields Nil row where
--   equalFields _ _ _ = true
--
-- type Thing b = Record b
-- equal
--   :: forall r rs
--    . RowToList r rs
--   => EqualFields rs r
--   => Record r
--   -> Record r
--   -> Record r
-- equal a b = a
--
-- thing :: { string :: String
-- }
-- thing = equal { string: "hi"} { string: "No"}

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
