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

{-|

@docs ParameterValue


# Constants

@docs zero, oneHalf, one


# Construction

@docs clamped, checked, midpoint, unsafe


# Conversion

@docs value

-}


{-| A parameter value between 0 and 1.
-}
type ParameterValue
    = ParameterValue Float


{-| The parameter value 0.0.

    ParameterValue.zero
    --> ParameterValue 0

-}
zero : ParameterValue
zero =
    ParameterValue 0


{-| The parameter value 0.5.

    ParameterValue.oneHalf
    --> ParameterValue 0.5

-}
oneHalf : ParameterValue
oneHalf =
    ParameterValue 0.5


{-| The parameter value 1.0.

    ParameterValue.one
    --> ParameterValue 1

-}
one : ParameterValue
one =
    ParameterValue 1


{-| Construct a valid parameter value by clamping a value to between 0 and 1.

    ParameterValue.clamped 0.75
    --> ParameterValue 0.75

    ParameterValue.clamped -0.25
    --> ParameterValue 0

    ParameterValue.value (ParameterValue.clamped 1.25)
    --> ParameterValue 1

-}
clamped : Float -> ParameterValue
clamped givenValue =
    if isNaN givenValue then
        ParameterValue givenValue
    else
        ParameterValue (clamp 0 1 givenValue)


{-| If the given value is between 0 and 1, return `Just` that value as a
`ParameterValue`. Otherwise, return `Nothing`.

    ParameterValue.checked 0.75
    --> Just (ParameterValue 0.75)

    ParameterValue.checked -0.25
    --> Nothing

    ParameterValue.checked 1.25
    --> Nothing

-}
checked : Float -> Maybe ParameterValue
checked givenValue =
    if isNaN givenValue then
        Nothing
    else if 0 <= givenValue && givenValue <= 1 then
        Just (ParameterValue givenValue)
    else
        Nothing


{-| Construct a `ParameterValue` directly from a `Float` without checking if it
is valid. Where possible, use `ParameterValue.clamped` or
`ParameterValue.checked` instead. If you need to produce a range of parameter
values, check out the [`ParameterValues`](Geometry-ParameterValues) module.
-}
unsafe : Float -> ParameterValue
unsafe =
    ParameterValue


{-| Construct a parameter value by finding the midpoint between two other
parameter values.
-}
midpoint : ParameterValue -> ParameterValue -> ParameterValue
midpoint (ParameterValue firstValue) (ParameterValue secondValue) =
    ParameterValue (firstValue + (secondValue - firstValue) / 2)


{-| Convert a `ParameterValue` to a plain `Float` value between 0 and 1.
-}
value : ParameterValue -> Float
value (ParameterValue value_) =
    value_
