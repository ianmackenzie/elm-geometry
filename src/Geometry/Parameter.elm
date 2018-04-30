module Geometry.Parameter exposing (maxStepSize, numSteps)

{-| The convention in `elm-geometry` is that parameter values range from 0 to 1.
For example, you could get the start point of a circular arc using

    Arc3d.pointOn arc 0

the end point as

    Arc3d.pointOn arc 1

and a list of 5 points along the arc (including the start and end points) by
using a list of parameter values:

    Arc3d.pointsOn arc [ 0, 0.25, 0.5, 0.75, 1 ]

This module helps create such lists of evenly-spaced parameter values, where the
values always range from 0 to 1. For example, the above could be rewritten as

    Arc3d.pointsOn arc (Parameter.numSteps 4)

or

    Arc3d.pointsOn arc (Parameter.maxStepSize 0.25)

@docs numSteps, maxStepSize

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


{-| Specify the number of steps to take between 0 and 1. Note that the returned
number of parameter values will be one greater than the number of steps!

    Parameter.numSteps 1
    --> [ 0, 1 ]

    Parameter.numSteps 2
    --> [ 0, 0.5, 1 ]

    Parameter.numSteps 5
    --> [ 0, 0.2, 0.4, 0.6, 0.8, 1 ]

Passing a negative or zero number of steps will result in an empty list:

    Parameter.numSteps 0
    --> []

-}
numSteps : Int -> List Float
numSteps n =
    if n >= 1 then
        linspaced n
    else
        []


{-| Specify the _maximum_ step size from one parameter value to the next. The
actual step size will be chosen to result in an even parameter spacing.

    Parameter.maxStepSize 0.5
    --> [ 0, 0.5, 1 ]

    Parameter.maxStepSize 0.4999
    --> [ 0, 0.3333, 0.6667, 1 ]

    Parameter.maxStepSize 1
    --> [ 0, 1 ]

    Parameter.maxStepSize 1.5
    --> [ 0, 1 ]

Passing a negative or zero maximum step size will result in an empty list:

    Parameter.maxStepSize 0
    --> []

-}
maxStepSize : Float -> List Float
maxStepSize x =
    if x >= 1 then
        singleStep
    else if x > 0 then
        linspaced (ceiling (1 / x))
    else
        []
