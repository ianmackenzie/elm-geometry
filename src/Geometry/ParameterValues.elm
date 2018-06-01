module Geometry.ParameterValues
    exposing
        ( ParameterValues
        , forEach
        , inBetween
        , leading
        , map
        , midpoints
        , steps
        , toList
        , trailing
        )

{-| Many things in `elm-geometry` make use of parameter values that vary from 0
to 1. For example, to get a point on a curve, you supply a parameter value
between 0 and 1 - passing 0 will return the start point of the curve, passing 1
will return the end point, and values in between will return points in between.
Similarly, interpolation functions such as [`Point3d.interpolateFrom`](Point3d#interpolateFrom)
take a value between 0 and 1 as an argument. This module helps define ranges of
parameter values between 0 and 1 conveniently and efficiently.

@docs ParameterValues


# Constructors

@docs steps, leading, trailing, inBetween, midpoints


# Evaluation

@docs map, forEach, foldl, foldr, toList

-}

import Float.Range as Range exposing (Range)


{-| Represents a list or range of parameter values.
-}
type ParameterValues
    = ParameterValues Range


{-| Get the full range of parameter values for a given number of steps,
including 0 and 1:

    ParameterValues.toList (ParameterValues.steps 1)
    --> [ 0, 1 ]

    ParameterValues.toList (ParameterValues.steps 2)
    --> [ 0, 0.5, 1 ]

    ParameterValues.toList (ParameterValues.steps 5)
    --> [ 0, 0.2, 0.4, 0.6. 0.8, 1 ]

Note that the number of parameter values is one greater than the number of
steps!

-}
steps : Int -> ParameterValues
steps n =
    ParameterValues (Range.from 0 1 (Range.numSteps n))


stepSize : Int -> Float
stepSize n =
    1 / toFloat n


leading : Int -> ParameterValues
leading n =
    ParameterValues <|
        if n < 1 then
            Range.empty
        else if n == 1 then
            Range.singleton 0
        else
            -- n > 1
            Range.from 0 (1 - stepSize n) (Range.numSteps (n - 1))


trailing : Int -> ParameterValues
trailing n =
    ParameterValues <|
        if n < 1 then
            Range.empty
        else if n == 1 then
            Range.singleton 1
        else
            -- n > 1
            Range.from (stepSize n) 1 (Range.numSteps (n - 1))


inBetween : Int -> ParameterValues
inBetween n =
    ParameterValues <|
        if n < 2 then
            Range.empty
        else if n == 2 then
            Range.singleton 0.5
        else
            -- n > 2
            Range.from (stepSize n) (1 - stepSize n) (Range.numSteps (n - 2))


midpoints : Int -> ParameterValues
midpoints n =
    ParameterValues <|
        if n < 1 then
            Range.empty
        else if n == 1 then
            Range.singleton 0.5
        else
            Range.from
                (stepSize n / 2)
                (1 - stepSize n / 2)
                (Range.numSteps (n - 1))


{-| Call the given function for each parameter value, returning a `List` of
results.

    p1 =
        Point2d.origin

    p2 =
        Point2d.fromCoordinates ( 2, 3 )

    Parameter.forEach (ParameterValues.steps 5)
        (Point2d.interpolateFrom p1 p2)
    --> [ Point2d.fromCoordinates ( 0, 0 )
    --> , Point2d.fromCoordinates ( 0.4, 0.6 )
    --> , Point2d.fromCoordinates ( 0.8, 1.2 )
    --> , Point2d.fromCoordinates ( 1.2, 1.8 )
    --> , Point2d.fromCoordinates ( 1.6, 2.4 )
    --> , Point2d.fromCoordinates ( 2, 3 )
    --> ]

-}
forEach : ParameterValues -> (Float -> a) -> List a
forEach (ParameterValues range) function =
    Range.forEach range function


map : (Float -> a) -> ParameterValues -> List a
map function parameterValues =
    forEach parameterValues function


toList : ParameterValues -> List Float
toList parameterValues =
    forEach parameterValues identity
