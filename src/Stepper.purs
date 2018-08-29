module Stepper where

import Prelude

import Control.Monad.Writer (Writer, tell)
import Data.Ord (abs)
import Data.Tuple (Tuple(..))

defaultStiffness :: Stiffness
defaultStiffness = Stiffness 170.0

defaultDamping :: Damping
defaultDamping = Damping 26.0

defaultSecondsPerFrame :: SecondsPerFrame
defaultSecondsPerFrame = SecondsPerFrame (1.0 / 60.0)

defaultPrecision :: Precision
defaultPrecision = Precision 0.01

interpolate :: Number -> Velocity -> Number -> Writer (Array (Array Number)) Unit
interpolate x velocity destX = do
  let Tuple x' (Velocity velocity') = defaultStepper x velocity destX
  tell [[x', velocity']]
  if velocity' == 0.0
    then
      pure unit
    else
      interpolate x' (Velocity velocity') destX

defaultStepper :: Number -> Velocity -> Number -> Tuple Number Velocity
defaultStepper x velocity destX = stepper defaultSecondsPerFrame x velocity destX defaultStiffness defaultDamping defaultPrecision

newtype Velocity = Velocity Number

derive instance eqVelocity :: Eq Velocity

instance showVelocity :: Show Velocity where
  show (Velocity v) = show v


newtype Damping = Damping Number
newtype Stiffness = Stiffness Number
newtype SecondsPerFrame = SecondsPerFrame Number
newtype Precision = Precision Number

stepper :: SecondsPerFrame -> Number ->  Velocity -> Number -> Stiffness -> Damping -> Precision -> Tuple Number Velocity
stepper
  (SecondsPerFrame secondsPerFrame)
  x
  (Velocity velocity)
  destX
  (Stiffness stiffness)
  (Damping damping)
  (Precision precision) =
  let
    fspring = negate stiffness * (x - destX)
    fdamper = negate damping * velocity
    a = fspring + fdamper
    velocity' = velocity + a * secondsPerFrame
    x' = x + velocity' * secondsPerFrame
  in
    if abs velocity' < precision && abs (x' - destX) < precision
      then
        Tuple destX (Velocity 0.0)
      else
        Tuple x' (Velocity velocity')
