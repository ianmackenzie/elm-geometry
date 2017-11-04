module OpenSolid.Scalar exposing (equalWithin, hull, hullOf, interpolateFrom)

{-| Convenience functions for working with scalar (floating-point) values.

@docs equalWithin, interpolateFrom, hull, hullOf

-}

import OpenSolid.Geometry.Internal as Internal


{-| Check if two values are equal within a given tolerance.

    Scalar.equalWithin 1e-6 1.9999 2.0001
    --> False

    Scalar.equalWithin 1e-3 1.9999 2.0001
    --> True

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


{-| Construct an interval containing both of the given values (which can be
given in either order).

    Scalar.hull 2 3
    --> Interval.with { minValue = 2, maxValue = 3 }

    Scalar.hull 5 -1
    --> Interval.with { minValue = -1, maxValue = 5 }

-}
hull : Float -> Float -> Internal.Interval
hull firstValue secondValue =
    Internal.Interval
        { minValue = min firstValue secondValue
        , maxValue = max firstValue secondValue
        }


{-| Construct an interval containing all values in the given list. If the list
is empty, returns `Nothing`.

    Scalar.hullOf [ 2, 1, 3 ]
    --> Just (Interval.with { minValue = 1, maxValue = 3 })

    Scalar.hullOf [ -3 ]
    --> Just (Interval.singleton -3)

    Scalar.hullOf []
    --> Nothing

-}
hullOf : List Float -> Maybe Internal.Interval
hullOf values =
    case values of
        [] ->
            Nothing

        first :: rest ->
            let
                minValue =
                    List.foldl min first rest

                maxValue =
                    List.foldl max first rest

                result =
                    Internal.Interval
                        { minValue = minValue
                        , maxValue = maxValue
                        }
            in
            Just result
