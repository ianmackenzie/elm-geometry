module Curve.ParameterValue
    exposing
        ( ParameterValue
        , checked
        , clamped
        , half
        , leading
        , midpoint
        , midpoints
        , one
        , oneMinus
        , range
        , steps
        , trailing
        , value
        , zero
        )

{-|

@docs ParameterValue


# Constants

@docs zero, half, one


# Construction

@docs clamped, checked, midpoint, oneMinus


# Ranges

@docs steps, leading, trailing, midpoints, range


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

    ParameterValue.half
    --> ParameterValue 0.5

-}
half : ParameterValue
half =
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


{-| Construct a parameter value by finding the midpoint between two other
parameter values.
-}
midpoint : ParameterValue -> ParameterValue -> ParameterValue
midpoint (ParameterValue firstValue) (ParameterValue secondValue) =
    ParameterValue (firstValue + (secondValue - firstValue) / 2)


oneMinus : ParameterValue -> ParameterValue
oneMinus (ParameterValue value_) =
    ParameterValue (1 - value_)


steps : Int -> List ParameterValue
steps n =
    if n < 1 then
        []
    else
        endpointsHelp 0 n (toFloat n) []


leading : Int -> List ParameterValue
leading n =
    if n < 1 then
        []
    else
        endpointsHelp 0 (n - 1) (toFloat n) []


trailing : Int -> List ParameterValue
trailing n =
    if n < 1 then
        []
    else
        endpointsHelp 1 n (toFloat n) []


endpointsHelp : Int -> Int -> Float -> List ParameterValue -> List ParameterValue
endpointsHelp startIndex index divisor accumulated =
    let
        parameterValue =
            ParameterValue (toFloat index / divisor)

        newAccumulated =
            parameterValue :: accumulated
    in
    if index == startIndex then
        newAccumulated
    else
        endpointsHelp startIndex (index - 1) divisor newAccumulated


midpoints : Int -> List ParameterValue
midpoints n =
    if n < 1 then
        []
    else
        midpointsHelp (2 * n - 1) (2 * toFloat n) []


midpointsHelp : Int -> Float -> List ParameterValue -> List ParameterValue
midpointsHelp index divisor accumulated =
    let
        parameterValue =
            ParameterValue (toFloat index / divisor)

        newAccumulated =
            parameterValue :: accumulated
    in
    if index == 1 then
        newAccumulated
    else
        midpointsHelp (index - 2) divisor newAccumulated


range : { numSteps : Int, includeStart : Bool, includeEnd : Bool } -> List ParameterValue
range { numSteps, includeStart, includeEnd } =
    if numSteps < 1 then
        []
    else
        let
            startIndex =
                if includeStart then
                    0
                else
                    1

            endIndex =
                if includeEnd then
                    numSteps
                else
                    numSteps - 1
        in
        if startIndex <= endIndex then
            endpointsHelp startIndex endIndex (toFloat numSteps) []
        else
            []


{-| Convert a `ParameterValue` to a plain `Float` value between 0 and 1.
-}
value : ParameterValue -> Float
value (ParameterValue value_) =
    value_
