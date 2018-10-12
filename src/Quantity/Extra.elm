module Quantity.Extra exposing (aXbY, aXbYcZ, interpolateFrom)

import Float.Extra as Float
import Quantity exposing (Quantity(..))


interpolateFrom : Quantity Float units -> Quantity Float units -> Float -> Quantity Float units
interpolateFrom (Quantity start) (Quantity end) parameter =
    Quantity (Float.interpolateFrom start end parameter)


aXbY : Float -> Quantity Float units -> Float -> Quantity Float units -> Quantity Float units
aXbY a (Quantity x) b (Quantity y) =
    Quantity (a * x + b * y)


aXbYcZ : Float -> Quantity Float units -> Float -> Quantity Float units -> Float -> Quantity Float units -> Quantity Float units
aXbYcZ a (Quantity x) b (Quantity y) c (Quantity z) =
    Quantity (a * x + b * y + c * z)
