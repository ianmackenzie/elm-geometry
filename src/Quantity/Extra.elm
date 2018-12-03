module Quantity.Extra exposing
    ( Cubed
    , aXbY
    , aXbYcZ
    , greaterThanOrEqualTo
    , interpolateFrom
    , lessThanOrEqualTo
    , scaleAbout
    )

import Float.Extra as Float
import Quantity exposing (Quantity(..))


type Cubed units
    = Cubed units


interpolateFrom : Quantity Float units -> Quantity Float units -> Float -> Quantity Float units
interpolateFrom (Quantity start) (Quantity end) parameter =
    Quantity (Float.interpolateFrom start end parameter)


aXbY : Float -> Quantity Float units -> Float -> Quantity Float units -> Quantity Float units
aXbY a (Quantity x) b (Quantity y) =
    Quantity (a * x + b * y)


aXbYcZ : Float -> Quantity Float units -> Float -> Quantity Float units -> Float -> Quantity Float units -> Quantity Float units
aXbYcZ a (Quantity x) b (Quantity y) c (Quantity z) =
    Quantity (a * x + b * y + c * z)


lessThanOrEqualTo : Quantity number units -> Quantity number units -> Bool
lessThanOrEqualTo (Quantity y) (Quantity x) =
    x <= y


greaterThanOrEqualTo : Quantity number units -> Quantity number units -> Bool
greaterThanOrEqualTo (Quantity y) (Quantity x) =
    x >= y


scaleAbout : Quantity number units -> number -> Quantity number units -> Quantity number units
scaleAbout (Quantity x0) scale (Quantity x) =
    Quantity (x0 + scale * (x - x0))
