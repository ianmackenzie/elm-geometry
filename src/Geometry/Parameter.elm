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

    Parameter.values [ -0.5, 0, 0.5, 1, 1.5 ]

is equivalent to

    Parameter.values [ 0, 0.5, 1 ]

Where possible, use `Parameter.steps` instead as it is more efficient; the above
could be replaced with

    Parameter.steps 2

-}
values : List Float -> Values
values =
    Values


{-| Specify the number of steps to take from 0 to 1;

    Parameter.steps 2

is equivalent to

    Parameter.values [ 0, 0.5, 1 ]

but more efficient since there is no need to actually construct the list. Note
that the number of parameter values will be one greater than the number of
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

    p1 =
        Point2d.origin

    p2 =
        Point2d.fromCoordinates ( 2, 3 )

    Parameter.forEach (Parameter.steps 5)
        (Point2d.interpolateFrom p1 p2)
    --> [ Point2d.fromCoordinates ( 0, 0 )
    --> , Point2d.fromCoordinates ( 0.4, 0.6 )
    --> , Point2d.fromCoordinates ( 0.8, 1.2 )
    --> , Point2d.fromCoordinates ( 1.2, 1.8 )
    --> , Point2d.fromCoordinates ( 1.6, 2.4 )
    --> , Point2d.fromCoordinates ( 2, 3 )
    --> ]

-}
forEach : Values -> (Float -> a) -> List a
forEach values_ function =
    case values_ of
        Values list ->
            List.foldr (call function) [] list

        Steps n ->
            Range.forEach (Range.from 0 1 (Range.numSteps n)) function
