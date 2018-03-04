module Geometry.Parameter exposing (Resolution, maxStepSize, numSteps, values)

{-| The convention in `elm-geometry` is that parameter values range from 0 to 1.
For example, you could get the start point of a circular arc using

    Arc3d.pointOn arc 0

the end point as

    Arc3d.pointOn arc 1

and a list of 5 points along the arc (including the start and end points) by
using a list of parameter values:

    parameterValues =
        [ 0, 0.25, 0.5, 0.75, 1 ]

    points =
        List.map (Arc3d.pointOn arc) parameterValues

This module helps create such lists of evenly-spaced parameter values, where the
values always range from 0 to 1. For example, the above could be rewritten as

    parameterValues =
        Parameter.values (Parameter.numSteps 4)

or

    parameterValues =
        Parameter.values (Parameter.maxStepSize 0.25)

@docs values, Resolution, numSteps, maxStepSize

-}

import Float.Extra as Float


singleStep : List Float
singleStep =
    [ 0, 1 ]


linspaced : Int -> List Float
linspaced n =
    -- Only valid for n >= 1
    if n == 1 then
        singleStep
    else
        List.range 0 n
            |> List.map
                (\i -> Float.interpolateFrom 0 1 (toFloat i / toFloat n))


{-| Get a list of values evenly spaced between 0 and 1, given a `Resolution`
value that specifies the desired parameter resolution. See below for examples!
-}
values : Resolution -> List Float
values resolution =
    case resolution of
        NumSteps n ->
            if n >= 1 then
                linspaced n
            else
                []

        MaxStepSize x ->
            if x >= 1 then
                singleStep
            else if x > 0 then
                linspaced (ceiling (1 / x))
            else
                []


{-| Specifies the desired resolution for a list of parameter values.
-}
type Resolution
    = NumSteps Int
    | MaxStepSize Float


{-| Specify the number of steps to take between 0 and 1. Note that the returned
number of parameter values will be one greater than the number of steps!

    Parameter.values (Parameter.numSteps 1)
    --> [ 0, 1 ]

    Parameter.values (Parameter.numSteps 2)
    --> [ 0, 0.5, 1 ]

    Parameter.values (Parameter.numSteps 5)
    --> [ 0, 0.2, 0.4, 0.6, 0.8, 1 ]

Passing a negative or zero number of steps will result in an empty list:

    Parameter.values (Parameter.numSteps 0)
    --> []

-}
numSteps : Int -> Resolution
numSteps =
    NumSteps


{-| Specify the _maximum_ step size from one parameter value to the next. The
actual step size will be chosen to result in an even parameter spacing.

    Parameter.values (Parameter.maxStepSize 0.5)
    --> [ 0, 0.5, 1 ]

    Parameter.values (Parameter.maxStepSize 0.4999)
    --> [ 0, 0.3333, 0.6667, 1 ]

    Parameter.values (Parameter.maxStepSize 1)
    --> [ 0, 1 ]

    Parameter.values (Parameter.maxStepSize 1.5)
    --> [ 0, 1 ]

Passing a negative or zero maximum step size will result in an empty list:

    Parameter.values (Parameter.maxStepSize 0)
    --> []

-}
maxStepSize : Float -> Resolution
maxStepSize =
    MaxStepSize
