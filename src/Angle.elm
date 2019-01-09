module Angle exposing
    ( Angle, Radians
    , radians, inRadians, degrees, inDegrees, turns, inTurns
    , sin, cos, tan, asin, acos, atan, atan2
    )

{-| An `Angle` represents an angle in degrees, radians, or turns. It is stored
as a number of radians.

@docs Angle, Radians


## Conversions

@docs radians, inRadians, degrees, inDegrees, turns, inTurns


## Trigonometry

If you're using `Angle` values instead of plain `Float`s, you'll need to use
these functions instead of [the corresponding ones in core][1].

[1]: https://package.elm-lang.org/packages/elm/core/latest/Basics#trigonometry

@docs sin, cos, tan, asin, acos, atan, atan2

-}

import Quantity exposing (Quantity(..))


{-| -}
type Radians
    = Radians


{-| -}
type alias Angle =
    Quantity Float Radians


{-| Construct an angle from a number of radians.
-}
radians : Float -> Angle
radians numRadians =
    Quantity numRadians


{-| Convert an angle to a number of radians.
-}
inRadians : Angle -> Float
inRadians (Quantity numRadians) =
    numRadians


{-| Construct an angle from a number of degrees.

    Angle.degrees 180
    --> Angle.radians pi

-}
degrees : Float -> Angle
degrees numDegrees =
    radians (pi * (numDegrees / 180))


{-| Convert an angle to a number of degrees.

    Angle.turns 2 |> Angle.inDegrees
    --> 720

-}
inDegrees : Angle -> Float
inDegrees angle =
    180 * (inRadians angle / pi)


{-| Construct an angle from a number of turns.

    Angle.turns -0.25
    --> Angle.degrees -90

-}
turns : Float -> Angle
turns numTurns =
    radians (2 * pi * numTurns)


{-| Convert an angle to a number of turns.

    Angle.radians pi |> Angle.inTurns
    --> 0.5

-}
inTurns : Angle -> Float
inTurns angle =
    inRadians angle / (2 * pi)


{-| -}
sin : Angle -> Float
sin (Quantity angle) =
    Basics.sin angle


{-| -}
cos : Angle -> Float
cos (Quantity angle) =
    Basics.cos angle


{-| -}
tan : Angle -> Float
tan (Quantity angle) =
    Basics.tan angle


{-| -}
asin : Float -> Angle
asin x =
    Quantity (Basics.asin x)


{-| -}
acos : Float -> Angle
acos x =
    Quantity (Basics.acos x)


{-| -}
atan : Float -> Angle
atan x =
    Quantity (Basics.atan x)


{-| -}
atan2 : Quantity Float units -> Quantity Float units -> Angle
atan2 (Quantity y) (Quantity x) =
    Quantity (Basics.atan2 y x)
