--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module OpenSolid.Direction3d
    exposing
        ( Direction3d
        , angleFrom
        , componentIn
        , components
        , equalWithin
        , flip
        , from
        , mirrorAcross
        , negativeX
        , negativeY
        , negativeZ
        , on
        , orthogonalize
        , perpendicularBasis
        , perpendicularTo
        , placeIn
        , positiveX
        , positiveY
        , positiveZ
        , projectInto
        , projectOnto
        , relativeTo
        , rotateAround
        , scaleBy
        , spherical
        , toVector
        , withComponents
        , x
        , xComponent
        , y
        , yComponent
        , z
        , zComponent
        )

{-| <img src="https://opensolid.github.io/images/geometry/icons/direction3d.svg" alt="Direction3d" width="160">

A `Direction3d` represents a direction like 'up' or 'north' or 'forwards'. They
are represented using X, Y and Z components, and can be converted to vectors if
necessary, but should be thought of as conceptually different. Directions have
several uses, such as:

  - Constructing a vector from a length and direction
  - Determining the component of a vector in a particular direction (for
    example, finding the component of velocity in the up direction to get
    vertical speed)
  - Determining the angle between two directions
  - Defining the orientation of an axis, plane or reference frame

@docs Direction3d


# Predefined directions

@docs x, y, z, positiveX, negativeX, positiveY, negativeY, positiveZ, negativeZ


# Constructors

@docs withComponents, on, spherical, from, perpendicularTo, perpendicularBasis, orthogonalize


# Components

@docs components, xComponent, yComponent, zComponent, componentIn


# Comparison

@docs equalWithin


# Vector conversion

@docs toVector


# Angle measurement

@docs angleFrom


# Transformations

@docs flip, scaleBy, rotateAround, mirrorAcross, projectOnto


# Coordinate frames

Functions for transforming directions between local and global coordinates in
different coordinate frames. Like other transformations, coordinate
transformations of directions depend only on the orientations of the relevant
frames, not their positions.

For the examples, assume the following definition of a local coordinate frame,
one that is rotated 30 degrees counterclockwise around the Z axis from the
global XYZ frame:

    rotatedFrame =
        Frame3d.rotateAround Axis3d.z (degrees 30) Frame3d.xyz

@docs relativeTo, placeIn


# Sketch planes

@docs projectInto

-}

import OpenSolid.Bootstrap.SketchPlane3d as SketchPlane3d
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Geometry.Internal as Internal exposing (Axis3d, Frame3d, Plane3d, Point3d, SketchPlane3d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


toDirection : Vector3d -> Direction3d
toDirection vector =
    withComponents (Vector3d.components vector)


{-| A direction in 3D.
-}
type alias Direction3d =
    Internal.Direction3d


{-| Synonym for `Direction3d.positiveX`.
-}
x : Direction3d
x =
    withComponents ( 1, 0, 0 )


{-| Synonym for `Direction3d.positiveY`.
-}
y : Direction3d
y =
    withComponents ( 0, 1, 0 )


{-| Synonym for `Direction3d.positiveZ`.
-}
z : Direction3d
z =
    withComponents ( 0, 0, 1 )


{-| The positive X direction.

    Direction3d.positiveX
    --> Direction3d.withComponents ( 1, 0, 0 )

-}
positiveX : Direction3d
positiveX =
    withComponents ( 1, 0, 0 )


{-| The negative X direction.

    Direction3d.negativeX
    --> Direction3d.withComponents ( -1, 0, 0 )

-}
negativeX : Direction3d
negativeX =
    withComponents ( -1, 0, 0 )


{-| The positive Y direction.

    Direction3d.positiveY
    --> Direction3d.withComponents ( 0, 1, 0 )

-}
positiveY : Direction3d
positiveY =
    withComponents ( 0, 1, 0 )


{-| The negative Y direction.

    Direction3d.negativeY
    --> Direction3d.withComponents ( 0, -1, 0 )

-}
negativeY : Direction3d
negativeY =
    withComponents ( 0, -1, 0 )


{-| The positive Z direction.

    Direction3d.positiveZ
    --> Direction3d.withComponents ( 0, 0, 1 )

-}
positiveZ : Direction3d
positiveZ =
    withComponents ( 0, 0, 1 )


{-| The negative Z direction.

    Direction3d.negativeZ
    --> Direction3d.withComponents ( 0, 0, -1 )

-}
negativeZ : Direction3d
negativeZ =
    withComponents ( 0, 0, -1 )


{-| Construct a direction directly from its X, Y and Z components. Note that
**you must ensure that the sum of the squares of the given components is exactly
one**:

    Direction3d.withComponents ( 1, 0, 0 )
    Direction3d.withComponents ( 0, -1, 0 )
    Direction3d.withComponents ( 0.6, 0, 0.8 )

are all valid but

    Direction3d.withComponents ( 2, 0, 0 )
    Direction3d.withComponents ( 1, 1, 1 )

are not. Instead of using `Direction3d.withComponents`, it may be easier to use
constructors like `Direction3d.on` or `Direction3d.spherical` (which will always
result in a valid direction), or start with existing directions and transform
them as necessary.

-}
withComponents : ( Float, Float, Float ) -> Direction3d
withComponents =
    Internal.Direction3d


{-| Construct a direction on the given sketch plane, given a polar angle from
the sketch plane's X direction towards its Y direction.

    Direction3d.on SketchPlane3d.xy (degrees 45)
    --> Direction3d.withComponents ( 0.7071, 0.7071, 0 )

    Direction3d.on SketchPlane3d.zx (degrees 30)
    --> Direction3d.withComponents ( 0.5, 0, 0.866 )

-}
on : SketchPlane3d -> Float -> Direction3d
on sketchPlane angle =
    Direction2d.fromAngle angle |> Direction2d.placeOnto sketchPlane


{-| Construct a direction using spherical coordinates relative to the given
sketch plane. The azimuth defines the angle on the sketch plane and the
elevation defines the angle from the sketch plane (a positive elevation
corresponds to a direction above the plane, in the same sense as the sketch
plane's `normalDirection`.)

    Direction3d.spherical SketchPlane3d.xy
        { azimuth = degrees 45
        , elevation = degrees 45
        }
    --> Direction3d.withComponents ( 0.5, 0.5, 0.7071 )

    Direction3d.spherical SketchPlane3d.zx
        { azimuth = degrees 90
        , elevation = degrees 30
        }
    --> Direction3d.withComponents ( 0.866, 0.5, 0 )

`on` can be thought of as a special case of `spherical`:

    Direction3d.on sketchPlane angle

is equivalent to

    Direction3d.spherical sketchPlane { azimuth = angle, elevation = 0 }

-}
spherical : SketchPlane3d -> { azimuth : Float, elevation : Float } -> Direction3d
spherical sketchPlane { azimuth, elevation } =
    let
        ( x1, y1, z1 ) =
            components (SketchPlane3d.xDirection sketchPlane)

        ( x2, y2, z2 ) =
            components (SketchPlane3d.yDirection sketchPlane)

        x3 =
            y1 * z2 - z1 * y2

        y3 =
            z1 * x2 - x1 * z2

        z3 =
            x1 * y2 - y1 * x2

        cosElevation =
            cos elevation

        x =
            cosElevation * cos azimuth

        y =
            cosElevation * sin azimuth

        z =
            sin elevation
    in
    withComponents
        ( x * x1 + y * x2 + z * x3
        , x * y1 + y * y2 + z * y3
        , x * z1 + y * z2 + z * z3
        )


{-| Attempt to construct the direction from the first given point to the second.
If the two points are coincident, returns `Nothing`.

    point =
        Point3d.withCoordinates ( 1, 0, 1 )

    Direction3d.from Point3d.origin point
    --> Just (Direction3d.withComponents ( 0.7071, 0, 0.7071 ))

    Direction3d.from point Point3d.origin
    --> Just (Direction3d.withComponents ( -0.7071, 0, -0.7071 ))

    Direction3d.from point point
    --> Nothing

-}
from : Point3d -> Point3d -> Maybe Direction3d
from firstPoint secondPoint =
    Vector3d.direction (Vector3d.from firstPoint secondPoint)


{-| Construct an arbitrary direction perpendicular to the given direction. The
exact resulting direction is not specified, but it is guaranteed to be
perpendicular to the given direction.

    Direction3d.perpendicularTo Direction3d.x
    --> Direction3d.withComponents ( 0, 0, -1 )

    Direction3d.perpendicularTo Direction3d.y
    --> Direction3d.withComponents ( 0, 0, 1 )

    direction =
        Direction3d.withComponents ( 0.6, 0, 0.8 )

    Direction3d.perpendicularTo direction
    --> Direction3d.withComponents ( 0.8, 0, -0.6 )

-}
perpendicularTo : Direction3d -> Direction3d
perpendicularTo direction =
    let
        perpendicularVector =
            Vector3d.perpendicularTo (toVector direction)

        length =
            Vector3d.length perpendicularVector

        normalizedVector =
            Vector3d.scaleBy (1 / length) perpendicularVector
    in
    toDirection normalizedVector


{-| Construct a pair of directions that are perpendicular to each other and both
perpendicular to the given direction.

The given direction and the two returned directions will form a
[right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
system (that is, a right-handed `Frame3d` could be constructed by using the
given direction as the X direction and the two returned directions as the Y and
Z directions, or the given direction as the Z direction and the two returned
directions as the X and Y directions).

    Direction3d.perpendicularBasis Direction3d.x
    --> ( Direction3d.withComponents ( 0, 0, -1 )
    --> , Direction3d.withComponents ( 0, 1, 0 )
    --> )

    Direction3d.perpendicularBasis Direction3d.y
    --> ( Direction3d.withComponents ( 0, 0, 1 )
    --> , Direction3d.withComponents ( 1, 0, 0 )
    --> )

-}
perpendicularBasis : Direction3d -> ( Direction3d, Direction3d )
perpendicularBasis direction =
    let
        xDirection =
            perpendicularTo direction

        yDirection =
            Vector3d.crossProduct (toVector direction) (toVector xDirection)
                |> toDirection
    in
    ( xDirection, yDirection )


{-| Attempt to form a set of three mutually perpendicular directions from the
three given directions by performing [Gram-Schmidt normalization](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process);

    Direction3d.orthogonalize
        ( xDirection
        , yDirection
        , zDirection
        )

is equivalent to

    Vector3d.orthonormalize
        ( Direction3d.toVector xDirection
        , Direction3d.toVector yDirection
        , Direction3d.toVector zDirection
        )

-}
orthogonalize : ( Direction3d, Direction3d, Direction3d ) -> Maybe ( Direction3d, Direction3d, Direction3d )
orthogonalize ( xDirection, yDirection, zDirection ) =
    Vector3d.orthonormalize
        ( toVector xDirection
        , toVector yDirection
        , toVector zDirection
        )


{-| Get the components of a direction as a tuple (the components it would have
as a unit vector, also know as its direction cosines).

    ( x, y, z ) =
        Direction3d.components direction

-}
components : Direction3d -> ( Float, Float, Float )
components (Internal.Direction3d components_) =
    components_


{-| Get the X component of a direction.

    Direction3d.xComponent Direction3d.x
    --> 1

    Direction3d.xComponent Direction3d.y
    --> 0

-}
xComponent : Direction3d -> Float
xComponent (Internal.Direction3d ( x, _, _ )) =
    x


{-| Get the Y component of a direction.

    Direction3d.yComponent Direction3d.y
    --> 1

    Direction3d.yComponent Direction3d.z
    --> 0

-}
yComponent : Direction3d -> Float
yComponent (Internal.Direction3d ( _, y, _ )) =
    y


{-| Get the Z component of a direction.

    Direction3d.zComponent Direction3d.z
    --> 1

    Direction3d.zComponent Direction3d.x
    --> 0

-}
zComponent : Direction3d -> Float
zComponent (Internal.Direction3d ( _, _, z )) =
    z


{-| Find the component of one direction in another direction. This is equal to
the cosine of the angle between the directions, or equivalently the dot product
of the two directions converted to unit vectors.

    direction =
        Direction3d.withComponents ( 0.6, 0.8, 0 )

    Direction3d.componentIn Direction3d.x direction
    --> 0.6

    Direction3d.componentIn Direction3d.z direction
    --> 0

    Direction3d.componentIn direction direction
    --> 1

    Direction3d.componentIn (Direction3d.flip direction) direction
    --> -1

This is more general and flexible than using `xComponent`, `yComponent` or
`zComponent`, all of which can be expressed in terms of `componentIn`; for
example,

    Direction3d.zComponent direction

is equivalent to

    Direction3d.componentIn Direction3d.z direction

-}
componentIn : Direction3d -> Direction3d -> Float
componentIn firstDirection secondDirection =
    Vector3d.componentIn firstDirection (toVector secondDirection)


{-| Compare two directions within a tolerance. Returns true if the angle between
the two given directions is less than the given tolerance.

    direction =
        Direction3d.rotateAround Axis3d.z (degrees 2) Direction3d.x

    Direction3d.equalWithin (degrees 5) Direction3d.x direction
    --> True

    Direction3d.equalWithin (degrees 1) Direction3d.x direction
    --> False

-}
equalWithin : Float -> Direction3d -> Direction3d -> Bool
equalWithin angle firstDirection secondDirection =
    angleFrom firstDirection secondDirection <= angle


{-| Convert a direction to a unit vector.

    Direction3d.toVector Direction3d.y
    --> Vector3d.withComponents ( 0, 1, 0 )

-}
toVector : Direction3d -> Vector3d
toVector direction =
    Vector3d.withComponents (components direction)


{-| Find the angle from one direction to another. The result will be in the
range 0 to π.

    Direction3d.angleFrom Direction3d.x Direction3d.x
    --> degrees 0

    Direction3d.angleFrom Direction3d.x Direction3d.z
    --> degrees 90

    Direction3d.angleFrom Direction3d.y Direction3d.negativeY
    --> degrees 180

-}
angleFrom : Direction3d -> Direction3d -> Float
angleFrom firstDirection secondDirection =
    let
        ( x1, y1, z1 ) =
            components firstDirection

        ( x2, y2, z2 ) =
            components secondDirection

        x =
            x1 * x2 + y1 * y2 + z1 * z2

        cx =
            y1 * z2 - z1 * y2

        cy =
            z1 * x2 - x1 * z2

        cz =
            x1 * y2 - y1 * x2

        y =
            sqrt (cx * cx + cy * cy + cz * cz)
    in
    atan2 y x


{-| Reverse a direction.

    Direction3d.flip Direction3d.y
    --> Direction3d.negativeY

-}
flip : Direction3d -> Direction3d
flip direction =
    let
        ( x, y, z ) =
            components direction
    in
    withComponents ( -x, -y, -z )


{-| Construct a vector of a particular length by treating a direction as a unit
vector and scaling it by the given length. In many cases it may be shorter and
more clear to use the [`Vector3d.in_`](OpenSolid-Vector3d#in_) constructor.

    Direction3d.scaleBy 3 Direction3d.z
    --> Vector3d.withComponents ( 0, 0, 3 )

The length can be negative, in which case the resulting vector will have the
opposite direction.

-}
scaleBy : Float -> Direction3d -> Vector3d
scaleBy scale direction =
    toVector direction |> Vector3d.scaleBy scale


{-| Rotate a direction around an axis by a given angle.

    Direction3d.rotateAround Axis3d.x (degrees 90) Direction3d.y
    --> Direction3d.z

Note that only the direction of the axis affects the result, not the position of
its origin point, since directions are position-independent:

    offsetAxis =
        Axis3d
            { originPoint = Point3d.withCoordinates ( 100, 200, 300 )
            , direction = Direction3d.z
            }

    Direction3d.rotateAround offsetAxis (degrees 90) Direction3d.x
    --> Direction3d.y

-}
rotateAround : Axis3d -> Float -> Direction3d -> Direction3d
rotateAround axis angle direction =
    toVector direction |> Vector3d.rotateAround axis angle |> toDirection


{-| Mirror a direction across a plane.

    direction =
        Direction3d.withComponents ( 0.6, 0, 0.8 )

    Direction3d.mirrorAcross Plane3d.xy direction
    --> Direction3d.withComponents ( 0.6, 0, -0.8 )

Note that only the normal direction of the plane affects the result, not the
position of its origin point, since directions are position-independent:

    offsetPlane =
        Plane3d.offsetBy 10 Plane3d.yz

    Direction3d.mirrorAcross offsetPlane direction
    --> Direction3d.withComponents ( -0.6, 0, 0.8 )

-}
mirrorAcross : Plane3d -> Direction3d -> Direction3d
mirrorAcross plane direction =
    toVector direction |> Vector3d.mirrorAcross plane |> toDirection


{-| Project a direction onto a plane. This is effectively the direction of the
given direction's 'shadow' on the given plane. If the given direction is
exactly perpendicular to the given plane, then `Nothing` is returned.

    direction =
        Direction3d.withComponents ( 0.6, -0.8, 0 )

    Direction3d.projectOnto Plane3d.xy direction
    --> Just (Direction3d.withComponents ( 0.6, -0.8, 0 ))

    Direction3d.projectOnto Plane3d.xz direction
    --> Just (Direction3d.withComponents ( 1, 0, 0 ))

    Direction3d.projectOnto Plane3d.yz direction
    --> Just (Direction3d.withComponents ( 0, -1, 0 ))

    Direction3d.projectOnto Plane3d.xy Direction3d.z
    --> Nothing

-}
projectOnto : Plane3d -> Direction3d -> Maybe Direction3d
projectOnto plane direction =
    toVector direction |> Vector3d.projectOnto plane |> Vector3d.direction


{-| Take a direction defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Direction3d.relativeTo rotatedFrame Direction3d.x
    --> Direction3d.withComponents ( 0.866, -0.5, 0 )

    Direction3d.relativeTo rotatedFrame Direction3d.y
    --> Direction3d.withComponents ( 0.5, 0.866, 0 )

    Direction3d.relativeTo rotatedFrame Direction3d.z
    --> Direction3d.withComponents ( 0, 0, 1 )

-}
relativeTo : Frame3d -> Direction3d -> Direction3d
relativeTo frame direction =
    toVector direction |> Vector3d.relativeTo frame |> toDirection


{-| Take a direction defined in local coordinates relative to a given reference
frame, and return that direction expressed in global coordinates.

    Direction3d.placeIn rotatedFrame Direction3d.x
    --> Direction3d.withComponents ( 0.866, 0.5, 0 )

    Direction3d.placeIn rotatedFrame Direction3d.y
    --> Direction3d.withComponents ( -0.5, 0.866, 0 )

    Direction3d.placeIn rotatedFrame Direction3d.z
    --> Direction3d.withComponents ( 0, 0, 1 )

-}
placeIn : Frame3d -> Direction3d -> Direction3d
placeIn frame direction =
    toVector direction |> Vector3d.placeIn frame |> toDirection


{-| Project a direction into a given sketch plane. Conceptually, this projects
the direction onto the plane and then expresses the projected direction in 2D
sketch coordinates.

This is only possible if the direction is not perpendicular to the sketch
plane; if it is perpendicular, `Nothing` is returned.

    direction =
        Direction3d.withComponents ( 0.6, -0.8, 0 )

    Direction3d.projectInto SketchPlane3d.xy direction
    --> Just (Direction2d.withComponents ( 0.6, -0.8 ))

    Direction3d.projectInto SketchPlane3d.xz direction
    --> Just (Direction2d.withComponents ( 1, 0 ))

    Direction3d.projectInto SketchPlane3d.yz direction
    --> Just (Direction2d.withComponents ( -1, 0 ))

    Direction3d.projectInto SketchPlane3d.xy Direction3d.z
    --> Nothing

-}
projectInto : SketchPlane3d -> Direction3d -> Maybe Direction2d
projectInto sketchPlane direction =
    toVector direction |> Vector3d.projectInto sketchPlane |> Vector2d.direction
