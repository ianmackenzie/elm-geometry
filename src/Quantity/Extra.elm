module Quantity.Extra exposing (interpolateFrom)

import Float.Extra as Float
import Quantity exposing (Quantity(..))


interpolateFrom : Quantity Float units -> Quantity Float units -> Float -> Quantity Float units
interpolateFrom (Quantity start) (Quantity end) parameter =
    Quantity (Float.interpolateFrom start end parameter)
