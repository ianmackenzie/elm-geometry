module OpenSolid.Scalar exposing (equalWithin, interpolateFrom)

{-| Convenience functions for working with scalar (floating-point) values.

@docs equalWithin, interpolateFrom

-}


{-| Check if two values are equal within a given tolerance.

    Scalar.equalWithin 1e-6 1.9999 2.0001 ==
        False

    Scalar.equalWithin 1e-3 1.9999 2.0001 ==
        True

-}
equalWithin : Float -> Float -> Float -> Bool
equalWithin tolerance firstValue secondValue =
    abs (secondValue - firstValue) <= tolerance


{-| Interpolate from the first value to the second, based on a parameter that
ranges from zero to one. Passing a parameter value of zero will return the start
value and passing a parameter value of one will return the end value.

    Scalar.interpolateFrom 5 10 0.5
    --> 7.5

    Scalar.interpolateFrom 2 -2 0.75
    --> -1

Parameter values less than zero or greater than one can be used to extrapolate:

    Scalar.interpolateFrom 5 10 1.5
    --> 12.5

    Scalar.interpolateFrom 2 -2 -0.25
    --> 3

-}
interpolateFrom : Float -> Float -> Float -> Float
interpolateFrom start end parameter =
    if parameter <= 0.5 then
        start + parameter * (end - start)
    else
        end + (1 - parameter) * (start - end)
