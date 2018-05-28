module Geometry.Parameter
    exposing
        ( Values
        , forEach
        , steps
        , values
        )

{-|

@docs Values, values, steps, forEach

-}

import Float.Range as Range exposing (Range)


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


{-| Specify the number of steps to take between 0 and 1;

    Parameter.steps 2

is equivalent to (but has a more efficient internal representation than)

    Parameter.values [ 0, 0.5, 1 ]

Note that the number of parameter values will be one greater than the number of
steps!

Passing a negative or zero number of steps will result in no values being
produced;

    Parameter.steps 0

is equivalent to

    Parameter.values []

-}
steps : Int -> Values
steps =
    Steps


call : (Float -> a) -> Float -> List a -> List a
call function parameterValue accumulated =
    if 0 <= parameterValue && parameterValue <= 1 then
        function parameterValue :: accumulated
    else
        accumulated


{-| Call the given function for each parameter value, returning a `List` of
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
