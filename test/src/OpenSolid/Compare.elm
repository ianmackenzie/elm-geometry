module OpenSolid.Compare
    exposing
        ( Comparator
        , by
        , allOf
        , defaultTolerance
        , approximately
        , approximatelyWithin
        , angle
        , angleWithin
        )


type alias Comparator a =
    a -> a -> Bool


by : Comparator b -> (a -> b) -> Comparator a
by comparator property first second =
    comparator (property first) (property second)


allOf : List (Comparator a) -> Comparator a
allOf comparators first second =
    List.all (\comparator -> comparator first second) comparators


defaultTolerance : Float
defaultTolerance =
    1.0e-12


approximately : Comparator Float
approximately =
    approximatelyWithin defaultTolerance


approximatelyWithin : Float -> Comparator Float
approximatelyWithin tolerance first second =
    abs (first - second) <= tolerance


angle : Comparator Float
angle =
    angleWithin defaultTolerance


angleWithin : Float -> Comparator Float
angleWithin tolerance first second =
    let
        difference =
            second - first
    in
        abs (sin difference) <= tolerance && cos difference > 0.0
