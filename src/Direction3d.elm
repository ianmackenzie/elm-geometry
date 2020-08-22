--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Direction3d exposing
    ( Direction3d
    , x, y, z, positiveX, negativeX, positiveY, negativeY, positiveZ, negativeZ
    , from
    , xy, yx, yz, zy, zx, xz, on
    , xyZ, yzX, zxY, fromAzimuthInAndElevationFrom
    , perpendicularTo, perpendicularBasis, orthonormalize, orthogonalize
    , toVector
    , components, xComponent, yComponent, zComponent, componentIn, angleFrom, azimuthIn, elevationFrom
    , equalWithin
    , reverse, rotateAround, mirrorAcross, projectOnto
    , relativeTo, placeIn, projectInto
    , random
    , unsafe, unwrap
    )

{-| A `Direction3d` represents a direction like 'up' or 'north' or 'forwards'.
They are represented using X, Y and Z components, and can be converted to
vectors if necessary, but should be thought of as conceptually different.
Directions have several uses, such as:

  - Constructing a vector from a length and direction
  - Determining the component of a vector in a particular direction (for
    example, finding the component of velocity in the up direction to get
    vertical speed)
  - Determining the angle between two directions
  - Defining the orientation of an axis, plane or reference frame

@docs Direction3d


# Constants

@docs x, y, z, positiveX, negativeX, positiveY, negativeY, positiveZ, negativeZ


# Constructors

@docs from


## In a plane

@docs xy, yx, yz, zy, zx, xz, on


## From azimuth and elevation

@docs xyZ, yzX, zxY, fromAzimuthInAndElevationFrom


## Perpendicular directions

@docs perpendicularTo, perpendicularBasis, orthonormalize, orthogonalize


# Conversion

@docs toVector


# Properties

@docs components, xComponent, yComponent, zComponent, componentIn, angleFrom, azimuthIn, elevationFrom


# Comparison

@docs equalWithin


# Transformations

@docs reverse, rotateAround, mirrorAcross, projectOnto


# Coordinate conversions

Like other transformations, coordinate transformations of directions depend only
on the orientations of the relevant frames, not their positions.

For the examples, assume the following definition of a local coordinate frame,
one that is rotated 30 degrees counterclockwise around the Z axis from the
global XYZ frame:

    rotatedFrame =
        Frame3d.atOrigin
            |> Frame3d.rotateAround Axis3d.z (Angle.degrees 30)

@docs relativeTo, placeIn, projectInto


# Random generation

@docs random


# Advanced

@docs unsafe, unwrap

-}

import Angle exposing (Angle)
import Direction2d exposing (Direction2d)
import Geometry.Types as Types exposing (Axis3d, Frame3d, Plane3d, Point3d, SketchPlane3d)
import Quantity exposing (Quantity(..), Unitless)
import Quantity.Extra as Quantity
import Random exposing (Generator)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Direction3d coordinates =
    Types.Direction3d coordinates


{-| Synonym for `Direction3d.positiveX`.
-}
x : Direction3d coordinates
x =
    positiveX


{-| Synonym for `Direction3d.positiveY`.
-}
y : Direction3d coordinates
y =
    positiveY


{-| Synonym for `Direction3d.positiveZ`.
-}
z : Direction3d coordinates
z =
    positiveZ


{-| The positive X direction.
-}
positiveX : Direction3d coordinates
positiveX =
    unsafe { x = 1, y = 0, z = 0 }


{-| The negative X direction.
-}
negativeX : Direction3d coordinates
negativeX =
    unsafe { x = -1, y = 0, z = 0 }


{-| The positive Y direction.
-}
positiveY : Direction3d coordinates
positiveY =
    unsafe { x = 0, y = 1, z = 0 }


{-| The negative Y direction.
-}
negativeY : Direction3d coordinates
negativeY =
    unsafe { x = 0, y = -1, z = 0 }


{-| The positive Z direction.
-}
positiveZ : Direction3d coordinates
positiveZ =
    unsafe { x = 0, y = 0, z = 1 }


{-| The negative Z direction.
-}
negativeZ : Direction3d coordinates
negativeZ =
    unsafe { x = 0, y = 0, z = -1 }


{-| Construct a direction directly from its X, Y and Z components. Note that
**you must ensure that the sum of the squares of the given components is exactly
one**:

    Direction3d.unsafe { x = 1, y = 0, z = 0 }

    Direction3d.unsafe { x = 0, y = -1, z = 0 }

    Direction3d.unsafe { x = 0.6, y = 0, z = 0.8 }

are all valid but

    Direction3d.unsafe { x = 2, y = 0, z = 0 }

    Direction3d.unsafe { x = 1, y = 1, z = 1 }

are not. Instead of using `Direction3d.unsafe`, it may be easier to use
constructors like `Direction3d.on` or `Direction3d.xyZ` (which will always
result in a valid direction), or start with existing directions and transform
them as necessary.

-}
unsafe : { x : Float, y : Float, z : Float } -> Direction3d coordinates
unsafe givenComponents =
    Types.Direction3d givenComponents


{-| Construct a direction in the XY plane given an angle measured from
positive X towards positive Y:

    Direction3d.xy (Angle.degrees 0)
    --> Direction3d.positiveX

    Direction3d.xy (Angle.degrees 90)
    --> Direction3d.positiveY

    Direction3d.xy (Angle.degrees 180)
    --> Direction3d.negativeX

-}
xy : Angle -> Direction3d coordinates
xy (Quantity theta) =
    Types.Direction3d
        { x = cos theta
        , y = sin theta
        , z = 0
        }


{-| Construct a direction in the XY plane given an angle measured from
positive Y towards positive X:

    Direction3d.yx (Angle.degrees 0)
    --> Direction3d.positiveY

    Direction3d.yx (Angle.degrees 90)
    --> Direction3d.positiveX

    Direction3d.yx (Angle.degrees -90)
    --> Direction3d.negativeX

-}
yx : Angle -> Direction3d coordinates
yx (Quantity theta) =
    Types.Direction3d
        { x = sin theta
        , y = cos theta
        , z = 0
        }


{-| Construct a direction in the YZ plane given an angle measured from
positive Y towards positive Z.
-}
yz : Angle -> Direction3d coordinates
yz (Quantity theta) =
    Types.Direction3d
        { x = 0
        , y = cos theta
        , z = sin theta
        }


{-| Construct a direction in the YZ plane given an angle measured from
positive Z towards positive Y.
-}
zy : Angle -> Direction3d coordinates
zy (Quantity theta) =
    Types.Direction3d
        { x = 0
        , y = sin theta
        , z = cos theta
        }


{-| Construct a direction in the XZ plane given an angle measured from
positive Z towards positive X.
-}
zx : Angle -> Direction3d coordinates
zx (Quantity theta) =
    Types.Direction3d
        { x = sin theta
        , y = 0
        , z = cos theta
        }


{-| Construct a direction in the XZ plane given an angle measured from
positive X towards positive Z.
-}
xz : Angle -> Direction3d coordinates
xz (Quantity theta) =
    Types.Direction3d
        { x = cos theta
        , y = 0
        , z = sin theta
        }


{-| Construct a 3D direction lying _on_ a sketch plane by providing a 2D
direction specified in XY coordinates _within_ the sketch plane. This is a
generalized version of `Direction3d.xy`, `Direction3d.yz` etc.; for example,

    Direction3d.xy (Angle.degrees 45)

is equivalent to

    Direction3d.on SketchPlane3d.xy (Direction2d.degrees 45)

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Direction2d coordinates2d -> Direction3d coordinates3d
on (Types.SketchPlane3d sketchPlane) (Types.Direction2d d) =
    let
        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection
    in
    Types.Direction3d
        { x = d.x * i.x + d.y * j.x
        , y = d.x * i.y + d.y * j.y
        , z = d.x * i.z + d.y * j.z
        }


{-| Construct a direction given:

  - Its azimuth angle _in_ the XY plane, from positive X towards positive Y
  - Its elevation angle _from_ the XY plane towards positive Z

See
[here](http://www.ece.northwestern.edu/local-apps/matlabhelp/techdoc/visualize/chview3.html)
for an illustration of these two angles (except that this function measures
azimuth from positive X towards positive Y, not negative Y towards positive
X). Note that `Direction3d.xyZ` is a generalized version of `Direction3d.xy`;

    Direction3d.xy angle

is equivalent to

    Direction3d.xyZ angle (Angle.degrees 0)

-}
xyZ : Angle -> Angle -> Direction3d coordinates
xyZ (Quantity theta) (Quantity phi) =
    let
        cosPhi =
            cos phi
    in
    Types.Direction3d
        { x = cosPhi * cos theta
        , y = cosPhi * sin theta
        , z = sin phi
        }


{-| Construct a direction given:

  - Its azimuth angle in the YZ plane, from positive Y towards positive Z
  - Its elevation angle from the YZ plane towards positive X

This is generally not as useful as `Direction3d.xyZ` or `Direction3d.zxY`.

-}
yzX : Angle -> Angle -> Direction3d coordinates
yzX (Quantity theta) (Quantity phi) =
    let
        cosPhi =
            cos phi
    in
    Types.Direction3d
        { x = sin phi
        , y = cosPhi * cos theta
        , z = cosPhi * sin theta
        }


{-| Construct a direction given:

  - Its azimuth angle in the ZX plane, from positive Z towards positive X
  - Its elevation angle from the ZX plane towards positive Y

This may be useful if you like to use positive Y as your global up vector.

-}
zxY : Angle -> Angle -> Direction3d coordinates
zxY (Quantity theta) (Quantity phi) =
    let
        cosPhi =
            cos phi
    in
    Types.Direction3d
        { x = cosPhi * sin theta
        , y = sin phi
        , z = cosPhi * cos theta
        }


{-| Construct a direction given:

  - Its azimuth angle within a given sketch plane, from that sketch plane's
    positive X direction towards its positive Y diretion
  - Its elevation angle out of that sketch plane, in the sketch plane's norma
    direction (the cross product of its X and Y directions)

This is a generalized version of `Direction3d.xyZ`, `Direction3d.yzX` and
`Direction3d.zxY`; for example,

    Direction3d.xyZ azimuth elevation

is equivalent to

    Direction3d.fromAzimuthInAndElevationFrom
        SketchPlane3d.xy
        azimuth
        elevation

-}
fromAzimuthInAndElevationFrom : SketchPlane3d units coordinates3d defines -> Angle -> Angle -> Direction3d coordinates3d
fromAzimuthInAndElevationFrom (Types.SketchPlane3d sketchPlane) (Quantity azimuth) (Quantity elevation) =
    let
        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection

        kx =
            i.y * j.z - i.z * j.y

        ky =
            i.z * j.x - i.x * j.z

        kz =
            i.x * j.y - i.y * j.x

        cosElevation =
            cos elevation

        sketchX =
            cosElevation * cos azimuth

        sketchY =
            cosElevation * sin azimuth

        sketchZ =
            sin elevation
    in
    Types.Direction3d
        { x = sketchX * i.x + sketchY * j.x + sketchZ * kx
        , y = sketchX * i.y + sketchY * j.y + sketchZ * ky
        , z = sketchX * i.z + sketchY * j.z + sketchZ * kz
        }


{-| Attempt to construct the direction from the first given point to the second.
If the two points are coincident, returns `Nothing`.

    point =
        Point3d.meters 1 0 1

    Direction3d.from Point3d.origin point
    --> Just (Direction3d.xz (Angle.degrees 45))

    Direction3d.from point Point3d.origin
    --> Just (Direction3d.xz (Angle.degrees -135))

    Direction3d.from point point
    --> Nothing

-}
from : Point3d units coordinates -> Point3d units coordinates -> Maybe (Direction3d coordinates)
from (Types.Point3d p1) (Types.Point3d p2) =
    let
        deltaX =
            p2.x - p1.x

        deltaY =
            p2.y - p1.y

        deltaZ =
            p2.z - p1.z

        largestComponent =
            max (abs deltaX) (max (abs deltaY) (abs deltaZ))
    in
    if largestComponent == 0 then
        Nothing

    else
        let
            scaledX =
                deltaX / largestComponent

            scaledY =
                deltaY / largestComponent

            scaledZ =
                deltaZ / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ)
        in
        Just <|
            Types.Direction3d
                { x = scaledX / scaledLength
                , y = scaledY / scaledLength
                , z = scaledZ / scaledLength
                }


{-| Construct an arbitrary direction perpendicular to the given direction. The
exact resulting direction is not specified, but it is guaranteed to be
perpendicular to the given direction.

    Direction3d.perpendicularTo Direction3d.x
    --> Direction3d.negativeZ

    Direction3d.perpendicularTo Direction3d.y
    --> Direction3d.positiveZ

    direction =
        Direction3d.xz (Angle.degrees 60)

    Direction3d.perpendicularTo direction
    --> Direction3d.xz (Angle.degrees -30)

-}
perpendicularTo : Direction3d coordinates -> Direction3d coordinates
perpendicularTo (Types.Direction3d d) =
    let
        absX =
            abs d.x

        absY =
            abs d.y

        absZ =
            abs d.z
    in
    if absX <= absY then
        if absX <= absZ then
            let
                scale =
                    sqrt (d.z * d.z + d.y * d.y)
            in
            Types.Direction3d
                { x = 0
                , y = -d.z / scale
                , z = d.y / scale
                }

        else
            let
                scale =
                    sqrt (d.y * d.y + d.x * d.x)
            in
            Types.Direction3d
                { x = -d.y / scale
                , y = d.x / scale
                , z = 0
                }

    else if absY <= absZ then
        let
            scale =
                sqrt (d.z * d.z + d.x * d.x)
        in
        Types.Direction3d
            { x = d.z / scale
            , y = 0
            , z = -d.x / scale
            }

    else
        let
            scale =
                sqrt (d.x * d.x + d.y * d.y)
        in
        Types.Direction3d
            { x = -d.y / scale
            , y = d.x / scale
            , z = 0
            }


{-| Construct a pair of directions that are perpendicular to each other and both
perpendicular to the given direction.

The given direction and the two returned directions will form a
[right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
system (that is, a right-handed `Frame3d` could be constructed by using the
given direction as the X direction and the two returned directions as the Y and
Z directions, or the given direction as the Z direction and the two returned
directions as the X and Y directions).

    Direction3d.perpendicularBasis Direction3d.x
    --> ( Direction3d.negativeZ
    --> , Direction3d.positiveY
    --> )

    Direction3d.perpendicularBasis Direction3d.y
    --> ( Direction3d.positiveZ
    --> , Direction3d.positiveX
    --> )

-}
perpendicularBasis : Direction3d coordinates -> ( Direction3d coordinates, Direction3d coordinates )
perpendicularBasis direction =
    let
        (Types.Direction3d d) =
            direction

        xDirection =
            perpendicularTo direction

        (Types.Direction3d dX) =
            xDirection

        yDirection =
            Types.Direction3d
                { x = d.y * dX.z - d.z * dX.y
                , y = d.z * dX.x - d.x * dX.z
                , z = d.x * dX.y - d.y * dX.x
                }
    in
    ( xDirection, yDirection )


{-| Attempt to form a set of three mutually perpendicular directions from the
three given vectors by performing [Gram-Schmidt normalization](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process):

  - The first returned direction will be equal to the direction of the first
    given vector
  - The second returned direction will be as close as possible to the second
    given vector while being perpendicular to the first returned direction
  - The third returned direction will be as close as possible to the third given
    vector while being perpendicular to the first and second returned directions

If any of the given vectors are zero, any two of them are parallel, or the three
are coplanar, returns `Nothing`.

    Direction3d.orthonormalize
        (Vector3d.meters 3 3 0)
        (Vector3d.meters 0 2 0)
        (Vector3d.meters 1 2 3)
    --> Just
    -->     ( Direction3d.xy (Angle.degrees 45)
    -->     , Direction3d.xy (Angle.degrees 135)
    -->     , Direction3d.z
    -->     )

    -- Three vectors in the XY plane:
    Direction3d.orthonormalize
        (Vector3d.meters 2 0 0)
        (Vector3d.meters 3 1 0)
        (Vector3d.meters 4 2 0)
    --> Nothing

-}
orthonormalize : Vector3d units1 coordinates -> Vector3d units2 coordinates -> Vector3d units3 coordinates -> Maybe ( Direction3d coordinates, Direction3d coordinates, Direction3d coordinates )
orthonormalize xVector xyVector xyzVector =
    Vector3d.direction xVector
        |> Maybe.andThen
            (\xDirection ->
                let
                    yVector =
                        xyVector
                            |> Vector3d.minus
                                (xyVector |> Vector3d.projectionIn xDirection)
                in
                Vector3d.direction yVector
                    |> Maybe.andThen
                        (\yDirection ->
                            let
                                rightHandedZVector =
                                    xVector |> Vector3d.cross xyVector

                                tripleProduct =
                                    rightHandedZVector
                                        |> Vector3d.dot xyzVector

                                zVector =
                                    if tripleProduct |> Quantity.greaterThan Quantity.zero then
                                        rightHandedZVector

                                    else if tripleProduct |> Quantity.lessThan Quantity.zero then
                                        Vector3d.reverse rightHandedZVector

                                    else
                                        Vector3d.zero
                            in
                            Vector3d.direction zVector
                                |> Maybe.map
                                    (\zDirection ->
                                        ( xDirection
                                        , yDirection
                                        , zDirection
                                        )
                                    )
                        )
            )


{-| Attempt to form a set of three mutually perpendicular directions from the
three given directions;

    Direction3d.orthogonalize
        xDirection
        yDirection
        zDirection

is equivalent to

    Direction3d.orthonormalize
        (Direction3d.toVector xDirection)
        (Direction3d.toVector yDirection)
        (Direction3d.toVector zDirection)

-}
orthogonalize : Direction3d coordinates -> Direction3d coordinates -> Direction3d coordinates -> Maybe ( Direction3d coordinates, Direction3d coordinates, Direction3d coordinates )
orthogonalize xDirection yDirection zDirection =
    orthonormalize
        (toVector xDirection)
        (toVector yDirection)
        (toVector zDirection)


{-| Get the X, Y and Z components of a direction as a tuple.

    Direction3d.yz (Angle.degrees 135)
        |> Direction3d.components
    --> ( 0, -0.7071, 0.7071 )

-}
components : Direction3d coordinates -> ( Float, Float, Float )
components (Types.Direction3d d) =
    ( d.x, d.y, d.z )


{-| Get the X component of a direction.

    Direction3d.xComponent Direction3d.x
    --> 1

    Direction3d.xComponent Direction3d.y
    --> 0

-}
xComponent : Direction3d coordinates -> Float
xComponent (Types.Direction3d d) =
    d.x


{-| Get the Y component of a direction.

    Direction3d.yComponent Direction3d.y
    --> 1

    Direction3d.yComponent Direction3d.z
    --> 0

-}
yComponent : Direction3d coordinates -> Float
yComponent (Types.Direction3d d) =
    d.y


{-| Get the Z component of a direction.

    Direction3d.zComponent Direction3d.z
    --> 1

    Direction3d.zComponent Direction3d.x
    --> 0

-}
zComponent : Direction3d coordinates -> Float
zComponent (Types.Direction3d d) =
    d.z


{-| Find the component of one direction in another direction. This is equal to
the cosine of the angle between the directions, or equivalently the dot product
of the two directions converted to unit vectors.

    direction =
        Direction3d.xz (Angle.degrees 60)

    Direction3d.componentIn Direction3d.x direction
    --> 0.5

    Direction3d.componentIn Direction3d.z direction
    --> 0.866

    Direction3d.componentIn direction direction
    --> 1

    direction
        |> Direction3d.componentIn
            (Direction3d.reverse direction)
    --> -1

This is more general and flexible than using `xComponent`, `yComponent` or
`zComponent`, all of which can be expressed in terms of `componentIn`; for
example,

    Direction3d.zComponent direction

is equivalent to

    Direction3d.componentIn Direction3d.z direction

-}
componentIn : Direction3d coordinates -> Direction3d coordinates -> Float
componentIn (Types.Direction3d d2) (Types.Direction3d d1) =
    d1.x * d2.x + d1.y * d2.y + d1.z * d2.z


{-| Get the azimuth of a direction in a given sketch plane (the angle of that
direction projected into the sketch plane), measured from its X axis towards its
Y axis (counterclockwise around the sketch plane's normal axis). The result will
be in the range -180 degrees to 180 degrees.

    Direction3d.x
        |> Direction3d.azimuthIn SketchPlane3d.xy
    --> Angle.degrees 0

    Direction3d.y
        |> Direction3d.azimuthIn SketchPlane3d.xy
    --> Angle.degrees 90

    Direction3d.negativeY
        |> Direction3d.azimuthIn SketchPlane3d.xy
    --> Angle.degrees -90

    Direction3d.negativeX
        |> Direction3d.azimuthIn SketchPlane3d.xy
    --> Angle.degrees 180

Vertical directions are considered to have an azimuth of zero:

    Direction3d.z |> Direction3d.azimuthIn SketchPlane3d.xy
    --> Angle.degrees 0

-}
azimuthIn : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Direction3d coordinates3d -> Angle
azimuthIn (Types.SketchPlane3d sketchPlane) (Types.Direction3d d) =
    let
        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection

        sketchX =
            d.x * i.x + d.y * i.y + d.z * i.z

        sketchY =
            d.x * j.x + d.y * j.y + d.z * j.z
    in
    Quantity (atan2 sketchY sketchX)


{-| Get the elevation angle of a direction from a given sketch plane (the angle
between the direction and the sketch plane, in the direction of the sketch
plane's normal). Directions in (coplanar with) the sketch plane have an
elevation angle of zero. The result will be in the range -90 degrees to 90
degrees.

    Direction3d.x
        |> Direction3d.elevationFrom SketchPlane3d.xy
    --> Angle.degrees 0

    Direction3d.y
        |> Direction3d.elevationFrom SketchPlane3d.xy
    --> Angle.degrees 0

    Direction3d.z
        |> Direction3d.elevationFrom SketchPlane3d.xy
    --> Angle.degrees 90

    Direction3d.negativeZ
        |> Direction3d.elevationFrom SketchPlane3d.xy
    --> Angle.degrees -90

-}
elevationFrom : SketchPlane3d units coordinates3d defines -> Direction3d coordinates3d -> Angle
elevationFrom (Types.SketchPlane3d sketchPlane) (Types.Direction3d d) =
    let
        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection

        kx =
            i.y * j.z - i.z * j.y

        ky =
            i.z * j.x - i.x * j.z

        kz =
            i.x * j.y - i.y * j.x

        sketchX =
            d.x * i.x + d.y * i.y + d.z * i.z

        sketchY =
            d.x * j.x + d.y * j.y + d.z * j.z

        sketchZ =
            d.x * kx + d.y * ky + d.z * kz
    in
    Quantity (atan2 sketchZ (sqrt (sketchX * sketchX + sketchY * sketchY)))


{-| Compare two directions within an angular tolerance. Returns true if the
angle between the two given directions is less than the given tolerance.

    rotatedDirection =
        Direction3d.x
            |> Direction3d.rotateAround Axis3d.z
                (Angle.degrees 2)

    Direction3d.equalWithin (Angle.degrees 5)
        Direction3d.x
        rotatedDirection
    --> True

    Direction3d.equalWithin (Angle.degrees 1)
        Direction3d.x
        rotatedDirection
    --> False

-}
equalWithin : Angle -> Direction3d coordinates -> Direction3d coordinates -> Bool
equalWithin angle firstDirection secondDirection =
    angleFrom firstDirection secondDirection
        |> Quantity.lessThanOrEqualTo angle


{-| Convert a direction to a unitless vector of length 1.

    Direction3d.toVector Direction3d.y
    --> Vector3d.unitless 0 1 0

-}
toVector : Direction3d coordinates -> Vector3d Unitless coordinates
toVector (Types.Direction3d directionComponents) =
    Types.Vector3d directionComponents


{-| Extract the X, Y and Z components of a direction as a record.
-}
unwrap : Direction3d coordinates -> { x : Float, y : Float, z : Float }
unwrap (Types.Direction3d coordinates) =
    coordinates


{-| Find the angle from one direction to another. The result will be in the
range 0 to 180 degrees

    Direction3d.angleFrom Direction3d.x Direction3d.x
    --> Angle.degrees 0

    Direction3d.angleFrom Direction3d.x Direction3d.z
    --> Angle.degrees 90

    Direction3d.angleFrom
        Direction3d.y
        Direction3d.negativeY
    --> Angle.degrees 180

-}
angleFrom : Direction3d coordinates -> Direction3d coordinates -> Angle
angleFrom (Types.Direction3d d1) (Types.Direction3d d2) =
    let
        relativeX =
            d1.x * d2.x + d1.y * d2.y + d1.z * d2.z

        cx =
            d1.y * d2.z - d1.z * d2.y

        cy =
            d1.z * d2.x - d1.x * d2.z

        cz =
            d1.x * d2.y - d1.y * d2.x

        relativeY =
            sqrt (cx * cx + cy * cy + cz * cz)
    in
    Quantity (atan2 relativeY relativeX)


{-| Reverse a direction.
-}
reverse : Direction3d coordinates -> Direction3d coordinates
reverse (Types.Direction3d d) =
    Types.Direction3d
        { x = -d.x
        , y = -d.y
        , z = -d.z
        }


{-| Rotate a direction around an axis by a given angle.

    Direction3d.y
        |> Direction3d.rotateAround Axis3d.x
            (Angle.degrees 90)
    --> Direction3d.z

Note that only the direction of the axis affects the result, not the position of
its origin point, since directions are position-independent:

    offsetAxis =
        Axis3d.withDirection Direction3d.z
            (Point3d.meters 100 200 300)

    Direction3d.x
        |> Direction3d.rotateAround offsetAxis
            (Angle.degrees 90)
    --> Direction3d.y

-}
rotateAround : Axis3d units coordinates -> Angle -> Direction3d coordinates -> Direction3d coordinates
rotateAround (Types.Axis3d axis) (Quantity angle) (Types.Direction3d d) =
    let
        (Types.Direction3d a) =
            axis.direction

        halfAngle =
            0.5 * angle

        sinHalfAngle =
            sin halfAngle

        qx =
            a.x * sinHalfAngle

        qy =
            a.y * sinHalfAngle

        qz =
            a.z * sinHalfAngle

        qw =
            cos halfAngle

        qwx =
            qw * qx

        qwy =
            qw * qy

        qwz =
            qw * qz

        qxx =
            qx * qx

        qxy =
            qx * qy

        qxz =
            qx * qz

        qyy =
            qy * qy

        qyz =
            qy * qz

        qzz =
            qz * qz

        a00 =
            1 - 2 * (qyy + qzz)

        a10 =
            2 * (qxy + qwz)

        a20 =
            2 * (qxz - qwy)

        a01 =
            2 * (qxy - qwz)

        a11 =
            1 - 2 * (qxx + qzz)

        a21 =
            2 * (qyz + qwx)

        a02 =
            2 * (qxz + qwy)

        a12 =
            2 * (qyz - qwx)

        a22 =
            1 - 2 * (qxx + qyy)
    in
    Types.Direction3d
        { x = a00 * d.x + a01 * d.y + a02 * d.z
        , y = a10 * d.x + a11 * d.y + a12 * d.z
        , z = a20 * d.x + a21 * d.y + a22 * d.z
        }


{-| Mirror a direction across a plane.

    direction =
        Direction3d.xyZ (Angle.degrees 30)
            (Angle.degrees 60)

    Direction3d.mirrorAcross Plane3d.xy direction
    --> Direction3d.xyZ (Angle.degrees 30)
    -->     (Angle.degrees -60)

Note that only the normal direction of the plane affects the result, not the
position of its origin point, since directions are position-independent:

    Direction3d.mirrorAcross Plane3d.yz direction
    --> Direction3d.xyZ (Angle.degrees 150)
    -->     (Angle.degrees 60)

    offsetPlane =
        Plane3d.offsetBy 10 Plane3d.yz

    Direction3d.mirrorAcross offsetPlane direction
    --> Direction3d.xyZ (Angle.degrees 150)
    -->     (Angle.degrees 60)

-}
mirrorAcross : Plane3d units coordinates -> Direction3d coordinates -> Direction3d coordinates
mirrorAcross (Types.Plane3d plane) (Types.Direction3d d) =
    let
        (Types.Direction3d n) =
            plane.normalDirection

        a11 =
            1 - 2 * n.x * n.x

        a22 =
            1 - 2 * n.y * n.y

        a33 =
            1 - 2 * n.z * n.z

        a23 =
            -2 * n.y * n.z

        a13 =
            -2 * n.x * n.z

        a12 =
            -2 * n.x * n.y
    in
    Types.Direction3d
        { x = a11 * d.x + a12 * d.y + a13 * d.z
        , y = a12 * d.x + a22 * d.y + a23 * d.z
        , z = a13 * d.x + a23 * d.y + a33 * d.z
        }


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a direction onto a plane (renormalized to have unit length). If the given
direction is exactly perpendicular to the given plane, returns `Nothing`.

    direction =
        Direction3d.xyZ (Angle.degrees -60)
            (Angle.degrees 0)

    Direction3d.projectOnto Plane3d.xy direction
    --> Just direction

    Direction3d.projectOnto Plane3d.xz direction
    --> Just Direction3d.x

    Direction3d.projectOnto Plane3d.yz direction
    --> Just Direction3d.negativeY

    Direction3d.projectOnto Plane3d.xy Direction3d.z
    --> Nothing

-}
projectOnto : Plane3d units coordinates -> Direction3d coordinates -> Maybe (Direction3d coordinates)
projectOnto (Types.Plane3d plane) (Types.Direction3d d) =
    let
        (Types.Direction3d n) =
            plane.normalDirection

        normalProjection =
            d.x * n.x + d.y * n.y + d.z * n.z

        projectedX =
            d.x - normalProjection * n.x

        projectedY =
            d.y - normalProjection * n.y

        projectedZ =
            d.z - normalProjection * n.z

        largestComponent =
            max (abs projectedX) (max (abs projectedY) (abs projectedZ))
    in
    if largestComponent == 0 then
        Nothing

    else
        let
            scaledX =
                projectedX / largestComponent

            scaledY =
                projectedY / largestComponent

            scaledZ =
                projectedZ / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY + scaledZ * scaledZ)
        in
        Just <|
            Types.Direction3d
                { x = scaledX / scaledLength
                , y = scaledY / scaledLength
                , z = scaledZ / scaledLength
                }


{-| Take a direction defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Direction3d.relativeTo rotatedFrame Direction3d.x
    --> Direction3d.xy (Angle.degrees -30)

    Direction3d.relativeTo rotatedFrame Direction3d.y
    --> Direction3d.xy (Angle.degrees 60)

    Direction3d.relativeTo rotatedFrame Direction3d.z
    --> Direction3d.z

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Direction3d globalCoordinates -> Direction3d localCoordinates
relativeTo (Types.Frame3d frame) (Types.Direction3d d) =
    let
        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection
    in
    Types.Direction3d
        { x = d.x * i.x + d.y * i.y + d.z * i.z
        , y = d.x * j.x + d.y * j.y + d.z * j.z
        , z = d.x * k.x + d.y * k.y + d.z * k.z
        }


{-| Take a direction defined in local coordinates relative to a given reference
frame, and return that direction expressed in global coordinates.

    Direction3d.placeIn rotatedFrame Direction3d.x
    --> Direction3d.xy (Angle.degrees 30)

    Direction3d.placeIn rotatedFrame Direction3d.y
    --> Direction3d.xy (Angle.degrees 120)

    Direction3d.placeIn rotatedFrame Direction3d.z
    --> Direction3d.z

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Direction3d localCoordinates -> Direction3d globalCoordinates
placeIn (Types.Frame3d frame) (Types.Direction3d d) =
    let
        (Types.Direction3d i) =
            frame.xDirection

        (Types.Direction3d j) =
            frame.yDirection

        (Types.Direction3d k) =
            frame.zDirection
    in
    Types.Direction3d
        { x = i.x * d.x + j.x * d.y + k.x * d.z
        , y = i.y * d.x + j.y * d.y + k.y * d.z
        , z = i.z * d.x + j.z * d.y + k.z * d.z
        }


{-| Project a direction into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the direction onto the plane, re-normalizes it to have unit length, and then
expresses the projected direction in 2D sketch coordinates.

This is only possible if the direction is not perpendicular to the sketch
plane; if it is perpendicular, `Nothing` is returned.

    direction =
        Direction3d.xy (Angle.degrees -60)

    Direction3d.projectInto SketchPlane3d.xy direction
    --> Just (Direction2d.degrees -60)

    Direction3d.projectInto SketchPlane3d.xz direction
    --> Just Direction2d.x

    Direction3d.projectInto SketchPlane3d.yz direction
    --> Just Direction2d.negativeX

    Direction3d.projectInto SketchPlane3d.xy Direction3d.z
    --> Nothing

-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Direction3d coordinates3d -> Maybe (Direction2d coordinates2d)
projectInto (Types.SketchPlane3d sketchPlane) (Types.Direction3d d) =
    let
        (Types.Direction3d i) =
            sketchPlane.xDirection

        (Types.Direction3d j) =
            sketchPlane.yDirection

        projectedX =
            d.x * i.x + d.y * i.y + d.z * i.z

        projectedY =
            d.x * j.x + d.y * j.y + d.z * j.z

        largestComponent =
            max (abs projectedX) (abs projectedY)
    in
    if largestComponent == 0 then
        Nothing

    else
        let
            scaledX =
                projectedX / largestComponent

            scaledY =
                projectedY / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)
        in
        Just <|
            Types.Direction2d
                { x = scaledX / scaledLength
                , y = scaledY / scaledLength
                }


{-| A [random generator](https://package.elm-lang.org/packages/elm/random/latest/Random)
for 3D directions.
-}
random : Generator (Direction3d coordinates)
random =
    Random.map2 xyZ
        (Random.map Angle.radians (Random.float -pi pi))
        (Random.map Angle.asin (Random.float -1 1))
