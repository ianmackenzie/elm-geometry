module Geometry.Parameter exposing (values)

{-| -}

import Float.Extra as Float


{-| Get a list of values evenly spaced between 0 and 1, given the number of
_steps_ to take from 0 to 1. Note that the number of returned values will be one
greater than the given number of steps!

    Parameter.values 1
    --> [ 0, 1 ]

    Parameter.values 2
    --> [ 0, 0.5, 1 ]

    Parameter.values 5
    --> [ 0, 0.2, 0.4, 0.6, 0.8, 1 ]

Passing anything less than 1 will result in an empty list:

    Parameter.values 0
    --> []

    Parameter.values -1
    --> []

-}
values : Int -> List Float
values n =
    if n < 1 then
        []
    else
        List.range 0 n
            |> List.map
                (\i -> Float.interpolateFrom 0 1 (toFloat i / toFloat n))
