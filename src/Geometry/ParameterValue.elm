module Geometry.ParameterValue
    exposing
        ( ParameterValue
        , checked
        , clamped
        , midpoint
        , one
        , oneHalf
        , unsafe
        , value
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
clamped givenValue =
    if isNaN givenValue then
        ParameterValue givenValue
    else
        ParameterValue (clamp 0 1 givenValue)


checked : Float -> Maybe ParameterValue
checked givenValue =
    if isNaN givenValue then
        Nothing
    else if 0 <= givenValue && givenValue <= 1 then
        Just (ParameterValue givenValue)
    else
        Nothing


unsafe : Float -> ParameterValue
unsafe =
    ParameterValue


midpoint : ParameterValue -> ParameterValue -> ParameterValue
midpoint (ParameterValue firstValue) (ParameterValue secondValue) =
    ParameterValue (firstValue + (secondValue - firstValue) / 2)


value : ParameterValue -> Float
value (ParameterValue value_) =
    value_
