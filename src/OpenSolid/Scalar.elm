module OpenSolid.Scalar exposing (equalWithin)

{-| Convenience functions for working with scalar (floating-point) values.

@docs equalWithin
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
