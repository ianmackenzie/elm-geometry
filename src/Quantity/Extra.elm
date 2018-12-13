module Quantity.Extra exposing
    ( Cubed
    , aXbY
    , aXbYcZ
    , greaterThanOrEqualTo
    , interpolateFrom
    , lOverTheta
    , lessThanOrEqualTo
    , rCosTheta
    , rSinTheta
    , rTheta
    , scaleAbout
    , sortBy
    )

import Angle exposing (Angle)
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


sortBy : (a -> Quantity number units) -> List a -> List a
sortBy toQuantity list =
    let
        comparator first second =
            Quantity.compare (toQuantity first) (toQuantity second)
    in
    List.sortWith comparator list


rTheta : Quantity Float units -> Angle -> Quantity Float units
rTheta (Quantity r) (Quantity theta) =
    Quantity (r * theta)


lOverTheta : Quantity Float units -> Angle -> Quantity Float units
lOverTheta (Quantity l) (Quantity theta) =
    Quantity (l / theta)


rCosTheta : Quantity Float units -> Angle -> Quantity Float units
rCosTheta r theta =
    r |> Quantity.scaleBy (Angle.cos theta)


rSinTheta : Quantity Float units -> Angle -> Quantity Float units
rSinTheta r theta =
    r |> Quantity.scaleBy (Angle.sin theta)
