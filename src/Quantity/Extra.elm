module Quantity.Extra exposing
    ( aXbY
    , lOverTheta
    , rCosTheta
    , rSinTheta
    , rTheta
    , scaleAbout
    )

import Angle exposing (Angle)
import Float.Extra as Float
import Quantity exposing (Quantity(..))


aXbY : Float -> Quantity Float units -> Float -> Quantity Float units -> Quantity Float units
aXbY a (Quantity x) b (Quantity y) =
    Quantity (a * x + b * y)


scaleAbout : Quantity number units -> number -> Quantity number units -> Quantity number units
scaleAbout (Quantity x0) scale (Quantity x) =
    Quantity (x0 + scale * (x - x0))


rTheta : Quantity Float units -> Angle -> Quantity Float units
rTheta (Quantity r) (Quantity theta) =
    Quantity (r * theta)


lOverTheta : Quantity Float units -> Angle -> Quantity Float units
lOverTheta (Quantity l) (Quantity theta) =
    Quantity (l / theta)


rCosTheta : Quantity Float units -> Angle -> Quantity Float units
rCosTheta r theta =
    r |> Quantity.multiplyBy (Angle.cos theta)


rSinTheta : Quantity Float units -> Angle -> Quantity Float units
rSinTheta r theta =
    r |> Quantity.multiplyBy (Angle.sin theta)
