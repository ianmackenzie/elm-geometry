module Geometry.ParameterValue
    exposing
        ( ParameterValue
        , checked
        , clamped
        , midpoint
        , one
        , oneHalf
        , toFloat
        , unsafe
        , zero
        )

{-| -}


{-| A parameter value between 0 and 1.
-}
type ParameterValue
    = ParameterValue Float


zero : ParameterValue
zero =
    ParameterValue 0


oneHalf : ParameterValue
oneHalf =
    ParameterValue 0.5


one : ParameterValue
one =
    ParameterValue 1


clamped : Float -> ParameterValue
clamped value =
    if isNaN value then
        ParameterValue value
    else
        ParameterValue (clamp 0 1 value)


checked : Float -> Maybe ParameterValue
checked value =
    if isNaN value then
        Nothing
    else if 0 <= value && value <= 1 then
        Just (ParameterValue value)
    else
        Nothing


unsafe : Float -> ParameterValue
unsafe =
    ParameterValue


midpoint : ParameterValue -> ParameterValue -> ParameterValue
midpoint (ParameterValue firstValue) (ParameterValue secondValue) =
    ParameterValue (firstValue + (secondValue - firstValue) / 2)


toFloat : ParameterValue -> Float
toFloat (ParameterValue value) =
    value
