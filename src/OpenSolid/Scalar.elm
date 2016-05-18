module OpenSolid.Scalar exposing (isZeroWithin)

{-| Various convenience functions for dealing with `Float` values.

@docs isZeroWithin
-}


{-| Check if a value is equal to zero, to within a given tolerance.

    Scalar.isZeroWithin 1e-3 0.0005 == True
    Scalar.isZeroWithin 1e-6 0.0005 == False

Note that the tolerance is given first, to allow convenient partial application:

    List.filter (Scalar.isZeroWithin 1e-3) [0.0005, 0.005] == [0.0005]
-}
isZeroWithin : Float -> Float -> Bool
isZeroWithin tolerance value =
    -tolerance <= value && value <= tolerance
