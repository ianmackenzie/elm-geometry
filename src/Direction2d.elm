--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Direction2d exposing
    ( Direction2d
    , x, y, positiveX, negativeX, positiveY, negativeY
    , degrees, radians
    , from, perpendicularTo, orthonormalize, orthogonalize
    , fromAngle, toAngle, toVector
    , components, xComponent, yComponent, componentIn, angleFrom
    , equalWithin
    , reverse, rotateClockwise, rotateCounterclockwise, rotateBy, mirrorAcross
    , relativeTo, placeIn
    , random
    , unsafe, unwrap
    )

{-| A `Direction2d` represents a direction like 'up' or 'north' or 'forwards'.
They are represented using X and Y components, and can be converted to vectors
if necessary, but should be thought of as conceptually different. Directions
have several uses, such as:

  - Constructing a vector from a length and direction
  - Determining the component of a vector in a particular direction (for
    example, finding the component of velocity in the up direction to get
    vertical speed)
  - Determining the (signed) angle between two directions
  - Defining the orientation of an axis or reference frame

@docs Direction2d


# Constants

@docs x, y, positiveX, negativeX, positiveY, negativeY


# Literals

@docs degrees, radians


# Constructors

@docs from, perpendicularTo, orthonormalize, orthogonalize


# Conversions

@docs fromAngle, toAngle, toVector


# Properties

@docs components, xComponent, yComponent, componentIn, angleFrom


# Comparison

@docs equalWithin


# Transformations

@docs reverse, rotateClockwise, rotateCounterclockwise, rotateBy, mirrorAcross


# Coordinate conversions

Like other transformations, coordinate transformations of directions depend only
on the orientations of the relevant frames, not the positions of their origin
points.

For the examples, assume the following frames have been defined:

    upsideDownFrame =
        Frame2d.atOrigin |> Frame2d.reverseY

    rotatedFrame =
        Frame2d.atOrigin |> Frame2d.rotateBy (Angle.degrees 30)

@docs relativeTo, placeIn


# Random generation

@docs random


# Advanced

@docs unsafe, unwrap

-}

import Angle exposing (Angle)
import Geometry.Types as Types exposing (Axis2d, Frame2d, Point2d)
import Quantity exposing (Quantity(..), Unitless)
import Quantity.Extra as Quantity
import Random exposing (Generator)
import Vector2d exposing (Vector2d)


{-| -}
type alias Direction2d coordinates =
    Types.Direction2d coordinates


{-| Synonym for `Direction2d.positiveX`.
-}
x : Direction2d coordinates
x =
    positiveX


{-| Synonym for `Direction2d.positiveY`.
-}
y : Direction2d coordinates
y =
    positiveY


{-| The positive X direction.
-}
positiveX : Direction2d coordinates
positiveX =
    Types.Direction2d { x = 1, y = 0 }


{-| The negative X direction.
-}
negativeX : Direction2d coordinates
negativeX =
    Types.Direction2d { x = -1, y = 0 }


{-| The positive Y direction.
-}
positiveY : Direction2d coordinates
positiveY =
    Types.Direction2d { x = 0, y = 1 }


{-| The negative Y direction.
-}
negativeY : Direction2d coordinates
negativeY =
    Types.Direction2d { x = 0, y = -1 }


{-| Construct a direction from a number of degrees, given counterclockwise from
the positive X axis:

    Direction2d.degrees 0
    --> Direction2d.positiveX

    Direction2d.degrees 90
    --> Direction2d.positiveY

This is a convenient shorthand for using `Direction2d.fromAngle` and
`Angle.degrees` if you want to construct a direction at a fixed angle in
degrees.

-}
degrees : Float -> Direction2d coordinates
degrees numDegrees =
    fromAngle (Angle.degrees numDegrees)


{-| Construct a direction from a number of radians, given counterclockwise from
the positive X axis:

    Direction2d.radians pi
    --> Direction2d.negativeX

    Direction2d.radians (-pi / 2)
    --> Direction2d.negativeY

-}
radians : Float -> Direction2d coordinates
radians numRadians =
    fromAngle (Angle.radians numRadians)


{-| Construct a direction directly from its X and Y components. Note that **you
must ensure that the sum of the squares of the given components is exactly
one**:

    Direction2d.unsafe { x = 1, y = 0 }

    Direction2d.unsafe { x = 0, y = -1 }

    Direction2d.unsafe { x = 0.6, y = 0.8 }

are all valid but

    Direction2d.unsafe { x = 2, y = 0 }

    Direction2d.unsafe { x = 1, y = 1 }

are not. Instead of using `Direction2d.unsafe`, it may be easier to use
constructors like [`degrees`](#degrees) or [`fromAngle`](#fromAngle) (which will
always result in a valid direction) or start with existing directions and
transform them as necessary.

-}
unsafe : { x : Float, y : Float } -> Direction2d coordinates
unsafe givenComponents =
    Types.Direction2d givenComponents


{-| Extract the X and Y components of a direction as a record.
-}
unwrap : Direction2d coordinates -> { x : Float, y : Float }
unwrap (Types.Direction2d directionComponents) =
    directionComponents


{-| Attempt to construct the direction from the first given point to the second.
If the two points are coincident, returns `Nothing`.

    point =
        Point2d.meters 1 1

    Direction2d.from Point2d.origin point
    --> Just (Direction2d.degrees 45)

    Direction2d.from point Point2d.origin
    --> Just (Direction2d.degrees -135)

    Direction2d.from point point
    --> Nothing

-}
from : Point2d units coordinates -> Point2d units coordinates -> Maybe (Direction2d coordinates)
from firstPoint secondPoint =
    Vector2d.direction (Vector2d.from firstPoint secondPoint)


{-| Construct a direction perpendicular to the given direction, by rotating the
given direction 90 degrees counterclockwise. Synonym for
`rotateCounterclockwise`.

    Direction2d.perpendicularTo Direction2d.x
    --> Direction2d.y

    Direction2d.perpendicularTo Direction2d.y
    --> Direction2d.negativeX

-}
perpendicularTo : Direction2d coordinates -> Direction2d coordinates
perpendicularTo (Types.Direction2d d) =
    Types.Direction2d
        { x = -d.y
        , y = d.x
        }


{-| Attempt to form a pair of perpendicular directions from the two given
vectors by performing [Gram-Schmidt normalization](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process):

  - The first returned direction will be equal to the direction of the first
    given vector
  - The second returned direction will be as close as possible to the second
    given vector while being perpendicular to the first returned direction

If either of the given vectors are zero, or if the two vectors are parallel,
returns `Nothing`.

    Direction2d.orthonormalize
        (Vector2d.meters 3 3)
        (Vector2d.meters 0 -2)
    --> Just
    -->     ( Direction2d.degrees 45
    -->     , Direction2d.degrees -45
    -->     )

    Direction2d.orthonormalize
        (Vector2d.meters 3 3)
        (Vector2d.meters -2 -2)
    --> Nothing

-}
orthonormalize : Vector2d units coordinates -> Vector2d units coordinates -> Maybe ( Direction2d coordinates, Direction2d coordinates )
orthonormalize xVector xyVector =
    Vector2d.direction xVector
        |> Maybe.andThen
            (\xDirection ->
                let
                    yDirection =
                        perpendicularTo xDirection

                    perpendicularComponent =
                        Vector2d.componentIn yDirection xyVector
                in
                case Quantity.compare perpendicularComponent Quantity.zero of
                    GT ->
                        Just ( xDirection, yDirection )

                    LT ->
                        Just ( xDirection, reverse yDirection )

                    EQ ->
                        Nothing
            )


{-| Attempt to form a pair of perpendicular directions from the two given
directions;

    Direction2d.orthogonalize xDirection yDirection

is equivalent to

    Direction2d.orthonormalize
        (Direction2d.toVector xDirection)
        (Direction2d.toVector yDirection)

-}
orthogonalize : Direction2d coordinates -> Direction2d coordinates -> Maybe ( Direction2d coordinates, Direction2d coordinates )
orthogonalize xDirection yDirection =
    orthonormalize (toVector xDirection) (toVector yDirection)


{-| Construct a direction from an [Angle](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Angle)
given counterclockwise from the positive X direction.

    Direction2d.fromAngle (Angle.degrees 90)
    --> Direction2d.y

-}
fromAngle : Angle -> Direction2d coordinates
fromAngle (Quantity angle) =
    Types.Direction2d
        { x = cos angle
        , y = sin angle
        }


{-| Convert a direction to a polar angle (the counterclockwise angle from the
positive X direction). The result will be in the range -180 to 180 degrees.

    Direction2d.toAngle Direction2d.negativeY
    --> Angle.degrees -90

-}
toAngle : Direction2d coordinates -> Angle
toAngle (Types.Direction2d d) =
    Quantity (atan2 d.y d.x)


{-| Find the counterclockwise angle from the first direction to the
second. The result will be in the range -180 to 180 degrees.

    referenceDirection =
        Direction2d.degrees 30

    Direction2d.angleFrom referenceDirection Direction2d.y
    --> Angle.degrees 60

    Direction2d.angleFrom referenceDirection Direction2d.x
    --> Angle.degrees -30

-}
angleFrom : Direction2d coordinates -> Direction2d coordinates -> Angle
angleFrom (Types.Direction2d d1) (Types.Direction2d d2) =
    let
        relativeX =
            d1.x * d2.x + d1.y * d2.y

        relativeY =
            d1.x * d2.y - d1.y * d2.x
    in
    Quantity (atan2 relativeY relativeX)


{-| Get the X and Y components of a direction as a tuple.

    Direction2d.components (Direction2d.degrees 135)
    --> ( -0.7071, 0.7071 )

-}
components : Direction2d coordinates -> ( Float, Float )
components (Types.Direction2d d) =
    ( d.x, d.y )


{-| Get the X component of a direction.

    Direction2d.xComponent Direction2d.x
    --> 1

    Direction2d.xComponent Direction2d.y
    --> 0

-}
xComponent : Direction2d coordinates -> Float
xComponent (Types.Direction2d d) =
    d.x


{-| Get the Y component of a direction.

    Direction2d.yComponent Direction2d.x
    --> 0

    Direction2d.yComponent Direction2d.y
    --> 1

-}
yComponent : Direction2d coordinates -> Float
yComponent (Types.Direction2d d) =
    d.y


{-| Find the component of one direction in another direction. This is equal to
the cosine of the angle between the directions, or equivalently the dot product
of the two directions converted to unit vectors.

    direction =
        Direction2d.degrees 60

    Direction2d.componentIn Direction2d.x direction
    --> 0.5

    Direction2d.componentIn direction direction
    --> 1

    Direction2d.componentIn Direction2d.x Direction2d.y
    --> 0

This is more general and flexible than using `xComponent` or `yComponent`, both
of which can be expressed in terms of `componentIn`; for example,

    Direction2d.xComponent direction

is equivalent to

    Direction2d.componentIn Direction2d.x direction

-}
componentIn : Direction2d coordinates -> Direction2d coordinates -> Float
componentIn (Types.Direction2d d2) (Types.Direction2d d1) =
    d1.x * d2.x + d1.y * d2.y


{-| Compare two directions within an angular tolerance. Returns true if the
absolute value of the angle between the two given directions is less than the
given tolerance.

    firstDirection =
        Direction2d.degrees 45

    secondDirection =
        Direction2d.degrees 47

    Direction2d.equalWithin (Angle.degrees 5)
        firstDirection
        secondDirection
    --> True

    Direction2d.equalWithin (Angle.degrees 1)
        firstDirection
        secondDirection
    --> False

-}
equalWithin : Angle -> Direction2d coordinates -> Direction2d coordinates -> Bool
equalWithin (Quantity angle) (Types.Direction2d d1) (Types.Direction2d d2) =
    let
        relativeX =
            d1.x * d2.x + d1.y * d2.y

        relativeY =
            d1.x * d2.y - d1.y * d2.x
    in
    abs (atan2 relativeY relativeX) <= angle


{-| Convert a direction to a unitless vector of length 1.

    Direction2d.toVector Direction2d.x
    --> Vector2d.unitless 1 0

-}
toVector : Direction2d coordinates -> Vector2d Unitless coordinates
toVector (Types.Direction2d directionComponents) =
    Types.Vector2d directionComponents


{-| Reverse a direction.
-}
reverse : Direction2d coordinates -> Direction2d coordinates
reverse (Types.Direction2d d) =
    Types.Direction2d
        { x = -d.x
        , y = -d.y
        }


{-| Rotate a direction by 90 degrees clockwise.

    Direction2d.rotateClockwise Direction2d.x
    --> Direction2d.negativeY

-}
rotateClockwise : Direction2d coordinates -> Direction2d coordinates
rotateClockwise (Types.Direction2d d) =
    Types.Direction2d
        { x = d.y
        , y = -d.x
        }


{-| Rotate a direction by 90 degrees counterclockwise.

    Direction2d.rotateClockwise Direction2d.x
    --> Direction2d.y

-}
rotateCounterclockwise : Direction2d coordinates -> Direction2d coordinates
rotateCounterclockwise (Types.Direction2d d) =
    Types.Direction2d
        { x = -d.y
        , y = d.x
        }


{-| Rotate a direction counterclockwise by a given angle.

    Direction2d.rotateBy (Angle.degrees 180) Direction2d.x
    --> Direction2d.negativeX

-}
rotateBy : Angle -> Direction2d coordinates -> Direction2d coordinates
rotateBy (Quantity angle) (Types.Direction2d d) =
    let
        c =
            cos angle

        s =
            sin angle
    in
    Types.Direction2d
        { x = c * d.x - s * d.y
        , y = s * d.x + c * d.y
        }


{-| Mirror a direction across a particular axis. Note that only the direction of
the axis affects the result, since directions are position-independent.

    slopedAxis =
        Axis2d.through
            (Point2d.meters 100 200)
            (Direction2d.degrees 45)

    Direction2d.mirrorAcross slopedAxis Direction2d.x
    --> Direction2d.y

    Direction2d.mirrorAcross slopedAxis Direction2d.y
    --> Direction2d.x

-}
mirrorAcross : Axis2d units coordinates -> Direction2d coordinates -> Direction2d coordinates
mirrorAcross (Types.Axis2d axis) (Types.Direction2d d) =
    let
        (Types.Direction2d a) =
            axis.direction

        yy =
            1 - 2 * a.y * a.y

        xy =
            2 * a.x * a.y

        xx =
            1 - 2 * a.x * a.x
    in
    Types.Direction2d
        { x = yy * d.x + xy * d.y
        , y = xy * d.x + xx * d.y
        }


{-| Take a direction defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Direction2d.relativeTo upsideDownFrame Direction2d.y
    --> Direction2d.negativeY

    Direction2d.relativeTo rotatedFrame Direction2d.x
    --> Direction2d.degrees -30

    Direction2d.relativeTo rotatedFrame Direction2d.y
    --> Direction2d.degrees 60

-}
relativeTo :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Direction2d globalCoordinates
    -> Direction2d localCoordinates
relativeTo (Types.Frame2d frame) (Types.Direction2d d) =
    let
        (Types.Direction2d dx) =
            frame.xDirection

        (Types.Direction2d dy) =
            frame.yDirection
    in
    Types.Direction2d
        { x = d.x * dx.x + d.y * dx.y
        , y = d.x * dy.x + d.y * dy.y
        }


{-| Take a direction defined in local coordinates relative to a given reference
frame, and return that direction expressed in global coordinates.

    Direction2d.placeIn upsideDownFrame Direction2d.y
    --> Direction2d.negativeY

    Direction2d.placeIn rotatedFrame Direction2d.x
    --> Direction2d.degrees 30

    Direction2d.placeIn rotatedFrame Direction2d.y
    --> Direction2d.degrees 120

-}
placeIn :
    Frame2d units globalCoordinates { defines : localCoordinates }
    -> Direction2d localCoordinates
    -> Direction2d globalCoordinates
placeIn (Types.Frame2d frame) (Types.Direction2d d) =
    let
        (Types.Direction2d dx) =
            frame.xDirection

        (Types.Direction2d dy) =
            frame.yDirection
    in
    Types.Direction2d
        { x = d.x * dx.x + d.y * dy.x
        , y = d.x * dx.y + d.y * dy.y
        }


{-| A [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for 2D directions.
-}
random : Generator (Direction2d coordinates)
random =
    Random.map (Angle.radians >> fromAngle) (Random.float -pi pi)
