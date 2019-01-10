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
    , from, perpendicularTo, orthonormalize, orthogonalize, unsafeFromComponents, unsafeFromComponentsIn
    , fromAngle, toAngle
    , components, xComponent, yComponent
    , equalWithin
    , componentIn, angleFrom
    , toVector
    , reverse, rotateClockwise, rotateCounterclockwise, rotateBy, mirrorAcross
    , relativeTo, placeIn
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


# Constructors

@docs from, perpendicularTo, orthonormalize, orthogonalize, unsafeFromComponents, unsafeFromComponentsIn


# Conversions

@docs fromAngle, toAngle


# Properties

@docs components, xComponent, yComponent


# Comparison

@docs equalWithin


# Measurement

@docs componentIn, angleFrom


# Conversion

@docs toVector


# Transformations

@docs reverse, rotateClockwise, rotateCounterclockwise, rotateBy, mirrorAcross


# Coordinate conversions

Like other transformations, coordinate transformations of directions depend only
on the orientations of the relevant frames, not the positions of their origin
points.

For the examples, assume the following frames have been defined:

    upsideDownFrame =
        Frame2d
            { originPoint = Point2d.origin
            , xDirection = Direction2d.positiveX
            , yDirection = Direction2d.negativeY
            }

    rotatedFrame =
        Frame2d.atOrigin |> Frame2d.rotateBy (degrees 30)

@docs relativeTo, placeIn

-}

import Angle exposing (Angle)
import Bootstrap.Axis2d as Axis2d
import Bootstrap.Direction2d as Bootstrap
import Bootstrap.Frame2d as Frame2d
import Geometry.Types as Types exposing (Axis2d, Frame2d, Point2d)
import Quantity exposing (Quantity, Unitless)
import Quantity.Extra as Quantity
import Vector2d exposing (Vector2d)


{-| -}
type alias Direction2d coordinates =
    Types.Direction2d coordinates


{-| Synonym for `Direction2d.positiveX`.
-}
x : Direction2d coordinates
x =
    unsafeFromComponents ( 1, 0 )


{-| Synonym for `Direction2d.positiveY`.
-}
y : Direction2d coordinates
y =
    unsafeFromComponents ( 0, 1 )


{-| The positive X direction.

    Direction2d.components Direction2d.positiveX
    --> ( 1, 0 )

-}
positiveX : Direction2d coordinates
positiveX =
    unsafeFromComponents ( 1, 0 )


{-| The negative X direction.

    Direction2d.components Direction2d.negativeX
    --> ( -1, 0 )

-}
negativeX : Direction2d coordinates
negativeX =
    unsafeFromComponents ( -1, 0 )


{-| The positive Y direction.

    Direction2d.components Direction2d.positiveY
    --> ( 0, 1 )

-}
positiveY : Direction2d coordinates
positiveY =
    unsafeFromComponents ( 0, 1 )


{-| The negative Y direction.

    Direction2d.components Direction2d.negativeY
    --> ( 0, -1 )

-}
negativeY : Direction2d coordinates
negativeY =
    unsafeFromComponents ( 0, -1 )


{-| Construct a direction directly from its X and Y components. Note that **you
must ensure that the sum of the squares of the given components is exactly
one**:

    Direction2d.unsafeFromComponents ( 1, 0 )

    Direction2d.unsafeFromComponents ( 0, -1 )

    Direction2d.unsafeFromComponents ( 0.6, 0.8 )

are all valid but

    Direction2d.unsafeFromComponents ( 2, 0 )

    Direction2d.unsafeFromComponents ( 1, 1 )

are not. Instead of using `Direction2d.unsafeFromComponents`, it may be easier
to use constructors like `Direction2d.fromAngle` (which will always result in a
valid direction) or start with existing directions and transform them as
necessary.

-}
unsafeFromComponents : ( Float, Float ) -> Direction2d coordinates
unsafeFromComponents givenComponents =
    Types.Direction2d givenComponents


{-| Construct a direction directly from its local X and Y components within a
given frame. As with `unsafeFromComponents`, you must ensure that the sum of the
squares of the given components is exactly one.
-}
unsafeFromComponentsIn : Frame2d units coordinates defines -> ( Float, Float ) -> Direction2d coordinates
unsafeFromComponentsIn frame localComponents =
    let
        ( localX, localY ) =
            localComponents

        ( x1, y1 ) =
            components (Frame2d.xDirection frame)

        ( x2, y2 ) =
            components (Frame2d.yDirection frame)
    in
    unsafeFromComponents
        ( x1 * localX + x2 * localY
        , y1 * localX + y2 * localY
        )


{-| Attempt to construct the direction from the first given point to the second.
If the two points are coincident, returns `Nothing`.

    point =
        Point2d.fromCoordinates ( 1, 1 )

    Direction2d.from Point2d.origin point
    --> Just (Direction2d.fromAngle (degrees 45))

    Direction2d.from point Point2d.origin
    --> Just (Direction2d.fromAngle (degrees -135))

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
perpendicularTo =
    Bootstrap.perpendicularTo


{-| Attempt to form a pair of perpendicular directions from the two given
vectors by performing [Gram-Schmidt normalization](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process):

  - The first returned direction will be equal to the direction of the first
    given vector
  - The second returned direction will be as close as possible to the second
    given vector while being perpendicular to the first returned direction

If either of the given vectors are zero, or if the two vectors are parallel,
returns `Nothing`.

    Direction2d.orthonormalize
        (Vector2d.fromComponents ( 3, 3 ))
        (Vector2d.fromComponents ( 0, -2 ))
    --> Just
    -->     ( Direction2d.fromAngle (degrees 45)
    -->     , Direction2d.fromAngle (degrees -45)
    -->     )

    Direction2d.orthonormalize
        (Vector2d.fromComponents ( 3, 3 ))
        (Vector2d.fromComponents ( -2, -2 ))
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


{-| Construct a direction from an angle in radians, given counterclockwise from
the positive X direction.

    Direction2d.fromAngle 0
    --> Direction2d.x

    Direction2d.fromAngle (degrees 90)
    --> Direction2d.y

    Direction2d.fromAngle (degrees -90)
    --> Direction2d.negativeY

-}
fromAngle : Angle -> Direction2d coordinates
fromAngle angle =
    unsafeFromComponents ( Angle.cos angle, Angle.sin angle )


{-| Convert a direction to a polar angle (the counterclockwise angle in radians
from the positive X direction). The result will be in the range -π to π.

    Direction2d.toAngle Direction2d.x
    --> 0

    Direction2d.toAngle Direction2d.y
    --> degrees 90

    Direction2d.toAngle Direction2d.negativeY
    --> degrees -90

-}
toAngle : Direction2d coordinates -> Angle
toAngle direction =
    let
        ( dx, dy ) =
            components direction
    in
    Angle.atan2 (Quantity.float dy) (Quantity.float dx)


{-| Find the counterclockwise angle in radians from the first direction to the
second. The result will be in the range -π to π.

    referenceDirection =
        Direction2d.fromAngle (degrees 30)

    Direction2d.angleFrom referenceDirection Direction2d.y
    --> degrees 60

    Direction2d.angleFrom referenceDirection Direction2d.x
    --> degrees -30

-}
angleFrom : Direction2d coordinates -> Direction2d coordinates -> Angle
angleFrom firstDirection secondDirection =
    let
        ( x1, y1 ) =
            components firstDirection

        ( x2, y2 ) =
            components secondDirection
    in
    Angle.radians (atan2 (x1 * y2 - y1 * x2) (x1 * x2 + y1 * y2))


{-| Get the components of a direction as a tuple (the components it would have
as a unit vector, also know as its direction cosines).

    ( x, y ) =
        Direction2d.components direction

-}
components : Direction2d coordinates -> ( Float, Float )
components direction =
    Bootstrap.components direction


{-| Get the X component of a direction.

    Direction2d.xComponent Direction2d.x
    --> 1

    Direction2d.xComponent Direction2d.y
    --> 0

-}
xComponent : Direction2d coordinates -> Float
xComponent (Types.Direction2d ( dx, _ )) =
    dx


{-| Get the Y component of a direction.

    Direction2d.yComponent Direction2d.x
    --> 0

    Direction2d.yComponent Direction2d.y
    --> 1

-}
yComponent : Direction2d coordinates -> Float
yComponent (Types.Direction2d ( _, dy )) =
    dy


{-| Find the component of one direction in another direction. This is equal to
the cosine of the angle between the directions, or equivalently the dot product
of the two directions converted to unit vectors.

    direction =
        Direction2d.fromAngle (degrees 60)

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
componentIn firstDirection secondDirection =
    let
        ( x1, y1 ) =
            components firstDirection

        ( x2, y2 ) =
            components secondDirection
    in
    x1 * x2 + y1 * y2


{-| Compare two directions within an angular tolerance. Returns true if the
absolute value of the angle between the two given directions is less than the
given tolerance.

    firstDirection =
        Direction2d.fromAngle (degrees 45)

    secondDirection =
        Direction2d.fromAngle (degrees 47)

    Direction2d.equalWithin (degrees 5)
        firstDirection
        secondDirection
    --> True

    Direction2d.equalWithin (degrees 1)
        firstDirection
        secondDirection
    --> False

-}
equalWithin : Angle -> Direction2d coordinates -> Direction2d coordinates -> Bool
equalWithin angle firstDirection secondDirection =
    Quantity.abs (angleFrom firstDirection secondDirection)
        |> Quantity.lessThanOrEqualTo angle


{-| Convert a direction to a unit vector.

    Direction2d.toVector Direction2d.x
    --> Vector2d.fromComponents ( 1, 0 )

-}
toVector : Direction2d coordinates -> Vector2d Unitless coordinates
toVector direction =
    Vector2d.fromTuple (components direction)


{-| Reverse a direction.

    Direction2d.reverse Direction2d.y
    --> Direction2d.negativeY

-}
reverse : Direction2d coordinates -> Direction2d coordinates
reverse direction =
    Bootstrap.reverse direction


{-| Rotate a direction by 90 degrees clockwise.

    Direction2d.rotateClockwise Direction2d.y
    --> Direction2d.x

    Direction2d.rotateClockwise Direction2d.x
    --> Direction2d.negativeY

-}
rotateClockwise : Direction2d coordinates -> Direction2d coordinates
rotateClockwise direction =
    let
        ( dx, dy ) =
            components direction
    in
    unsafeFromComponents ( dy, -dx )


{-| Rotate a direction by 90 degrees counterclockwise.

    Direction2d.rotateClockwise Direction2d.x
    --> Direction2d.y

    Direction2d.rotateClockwise Direction2d.y
    --> Direction2d.negativeX

-}
rotateCounterclockwise : Direction2d coordinates -> Direction2d coordinates
rotateCounterclockwise direction =
    let
        ( dx, dy ) =
            components direction
    in
    unsafeFromComponents ( -dy, dx )


{-| Rotate a direction counterclockwise by a given angle (in radians).

    Direction2d.rotateBy pi Direction2d.x
    --> Direction2d.negativeX

    Direction2d.rotateBy (degrees 45) Direction2d.y
    --> Direction2d.fromAngle (degrees 135)

-}
rotateBy : Angle -> Direction2d coordinates -> Direction2d coordinates
rotateBy givenAngle direction =
    let
        c =
            Angle.cos givenAngle

        s =
            Angle.sin givenAngle

        ( dx, dy ) =
            components direction
    in
    unsafeFromComponents
        ( c * dx - s * dy
        , s * dx + c * dy
        )


{-| Mirror a direction across a particular axis. Note that only the direction of
the axis affects the result, since directions are position-independent.

    slopedAxis =
        Axis2d.through
            (Point2d.fromCoordinates ( 100, 200 ))
            (Direction2d.fromAngle (degrees 45))

    Direction2d.mirrorAcross slopedAxis Direction2d.x
    --> Direction2d.y

    Direction2d.mirrorAcross slopedAxis Direction2d.y
    --> Direction2d.x

-}
mirrorAcross : Axis2d units coordinates -> Direction2d coordinates -> Direction2d coordinates
mirrorAcross givenAxis direction =
    let
        ( ax, ay ) =
            components (Axis2d.direction givenAxis)

        yy =
            1 - 2 * ay * ay

        xy =
            2 * ax * ay

        xx =
            1 - 2 * ax * ax

        ( dx, dy ) =
            components direction
    in
    unsafeFromComponents
        ( yy * dx + xy * dy
        , xy * dx + xx * dy
        )


{-| Take a direction defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Direction2d.relativeTo upsideDownFrame Direction2d.y
    --> Direction2d.negativeY

    Direction2d.relativeTo rotatedFrame Direction2d.x
    --> Direction2d.fromAngle (degrees -30)

    Direction2d.relativeTo rotatedFrame Direction2d.y
    --> Direction2d.fromAngle (degrees 60)

-}
relativeTo : Frame2d units globalCoordinates { defines : localCoordinates } -> Direction2d globalCoordinates -> Direction2d localCoordinates
relativeTo givenFrame direction =
    unsafeFromComponents
        ( componentIn (Frame2d.xDirection givenFrame) direction
        , componentIn (Frame2d.yDirection givenFrame) direction
        )


{-| Take a direction defined in local coordinates relative to a given reference
frame, and return that direction expressed in global coordinates.

    Direction2d.placeIn upsideDownFrame Direction2d.y
    --> Direction2d.negativeY

    Direction2d.placeIn rotatedFrame Direction2d.x
    --> Direction2d.fromAngle (degrees 30)

    Direction2d.placeIn rotatedFrame Direction2d.y
    --> Direction2d.fromAngle (degrees 120)

-}
placeIn : Frame2d units globalCoordinates { defines : localCoordinates } -> Direction2d localCoordinates -> Direction2d globalCoordinates
placeIn givenFrame direction =
    let
        ( x1, y1 ) =
            components (Frame2d.xDirection givenFrame)

        ( x2, y2 ) =
            components (Frame2d.yDirection givenFrame)

        ( dx, dy ) =
            components direction
    in
    unsafeFromComponents
        ( x1 * dx + x2 * dy
        , y1 * dx + y2 * dy
        )
