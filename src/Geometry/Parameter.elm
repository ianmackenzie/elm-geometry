module Geometry.Parameter
    exposing
        ( Value
        , Values
        , forEach
        , steps
        , value
        , values
        , with
        )

{-|

@docs Value, value, with, Values, values, steps, forEach

-}

import Float.Range as Range exposing (Range)


{-| Represents a single parameter value along a curve. Parameter values range
from 0 to 1.
-}
type Value
    = Value Float


{-| Convert a `Float` value to a parameter value. The given value should be
between 0 and 1.
-}
value : Float -> Value
value =
    Value


{-| Call a function with a given parameter value. Will return `Nothing` if the
given parameter value is not between 0 and 1.
-}
with : Value -> (Float -> a) -> Maybe a
with (Value value_) function =
    if isValid value_ then
        Just (function value_)
    else
        Nothing


{-| Represents a list or range of parameter values.
-}
type Values
    = Values (List Float)
    | Steps Int


{-| Provide a plain list of parameter values, which should all be in the range
0 to 1. Values out of range will be discarded, so

    Parameter.values [ 0, 0.5, 1, 1.5 ]

is equivalent to

    Parameter.values [ 0, 0.5, 1 ]

Where possible, use `Parameter.steps` instead as it is more efficient; the above
could be replaced with

    Parameter.steps 2

-}
values : List Float -> Values
values =
    Values


{-| Specify the number of steps to take between 0 and 1. Note that the number of
parameter values will be one greater than the number of steps!

    Parameter.steps 1
    --> Parameter.values [ 0, 1 ]

    Parameter.steps 2
    --> Parameter.values [ 0, 0.5, 1 ]

    Parameter.steps 5
    --> Parameter.values [ 0, 0.2, 0.4, 0.6, 0.8, 1 ]

Passing a negative or zero number of steps will result in no values being
produced:

    Parameter.steps 0
    --> Parameter.values []

-}
steps : Int -> Values
steps =
    Steps


isValid : Float -> Bool
isValid parameter =
    0 <= parameter && parameter <= 1


call : (Float -> a) -> Float -> List a -> List a
call function parameterValue accumulated =
    if 0 <= parameterValue && parameterValue <= 1 then
        function parameterValue :: accumulated
    else
        accumulated


{-| Call the given function for each parameter value, returning a list of
results.

    Parameter.forEach (Parameter.steps 5) toString
    --> [ "0", "0.2", "0.4", "0.6", "0.8", "1" ]

-}
forEach : Values -> (Float -> a) -> List a
forEach values_ function =
    case values_ of
        Values list ->
            List.foldr (call function) [] list

        Steps n ->
            Range.forEach (Range.from 0 1 (Range.numSteps n)) function
