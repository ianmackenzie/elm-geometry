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


module OpenSolid.Vector3d
    exposing
        ( zero
        , perpendicularTo
        , components
        , xComponent
        , yComponent
        , zComponent
        , componentIn
        , equalWithin
        , length
        , squaredLength
        , direction
        , lengthAndDirection
        , negate
        , times
        , plus
        , minus
        , dotProduct
        , crossProduct
        , rotateAround
        , mirrorAcross
        , projectionIn
        , projectOnto
        , relativeTo
        , placeIn
        , projectInto
        )

{-| Various functions for creating and working with `Vector3d` values. A
`Vector3d` is defined by its X, Y and Z components and represents a quantity
like a velocity or displacement in 3D.

The simplest way to create a `Vector3d` is by passing a tuple of X, Y and Z
components to the `Vector3d` constructor, for example

    vector =
        Vector3d ( 2, 1, 3 )

## Reading this documentation

For the examples below, assume that all OpenSolid core types have been imported
using

    import OpenSolid.Geometry.Types exposing (..)

and all other necessary modules have been imported using the following pattern:

    import OpenSolid.Vector3d as Vector3d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Constants

@docs zero

Although there are no predefined constants for `Vector3d ( 1, 0, 0 )`,
`Vector3d ( 0, 1, 0 )` and `Vector3d ( 0, 0, 1 )`, in most cases you will
actually want their `Direction3d` versions `Direction3d.x`, `Direction3d.y` and
`Direction3d.z`.

# Constructors

@docs perpendicularTo

# Components

@docs components, xComponent, yComponent, zComponent, componentIn

# Comparison

@docs equalWithin

# Length and direction

@docs length, squaredLength, direction, lengthAndDirection

# Arithmetic

@docs negate, times, plus, minus, dotProduct, crossProduct

# Transformations

Note that for all transformations, only the orientation of the given axis or
plane is relevant, since vectors are position-independent. Think of transforming
a vector as placing its tail on the relevant axis or plane and then transforming
its tip.

@docs rotateAround, mirrorAcross, projectionIn, projectOnto

# Coordinate frames

Functions for transforming vectors between local and global coordinates in
different coordinate frames. Like other transformations, coordinate
transformations of vectors depend only on the orientations of the relevant
frames, not their positions.

For the examples, assume the following definition of a local coordinate frame,
one that is rotated 30 degrees counterclockwise around the Z axis from the
global XYZ frame:

    rotatedFrame =
        Frame3d.rotateAround Axis3d.z (degrees 30) Frame3d.xyz

    Frame3d.xDirection rotatedFrame ==
        Direction3d ( 0.866, 0.5, 0 )

    Frame3d.yDirection rotatedFrame ==
        Direction3d ( -0.5, 0.866, 0 )

    Frame3d.zDirection rotatedFrame ==
        Direction3d ( 0, 0, 1 )

@docs relativeTo, placeIn

# Sketch planes

Functions for converting vectors between global 3D coordinates and 2D
coordinates within a particular sketch plane.

@docs projectInto
-}

import OpenSolid.Geometry.Types exposing (..)


{-| The zero vector.

    Vector3d.zero ==
        Vector3d ( 0, 0, 0 )
-}
zero : Vector3d
zero =
    Vector3d ( 0, 0, 0 )


{-| Construct an arbitrary vector perpendicular to the given vector. The exact
magnitude and direction of the resulting vector are not specified, but it is
guaranteed to be perpendicular to the given vector and non-zero (unless the
given vector is itself zero).

    Vector3d.perpendicularTo (Vector3d ( 3, 0, 0 )) ==
        Vector3d ( 0, 0, -3 )

    Vector3d.perpendicularTo (Vector3d ( 1, 2, 3 )) ==
        Vector3d ( 0, -3, 2 )

    Vector3d.perpendicularTo Vector3d.zero ==
        Vector3d.zero
-}
perpendicularTo : Vector3d -> Vector3d
perpendicularTo vector =
    let
        ( x, y, z ) =
            components vector

        absX =
            abs x

        absY =
            abs y

        absZ =
            abs z
    in
        if absX <= absY then
            if absX <= absZ then
                Vector3d ( 0, -z, y )
            else
                Vector3d ( -y, x, 0 )
        else if absY <= absZ then
            Vector3d ( z, 0, -x )
        else
            Vector3d ( -y, x, 0 )


{-| Extract the components of a vector.

    Vector3d.components (Vector2d ( 2, 3, 4 )) ==
        ( 2, 3, 4 )

This combined with Elm's built-in tuple destructuring provides a convenient way
to extract the X, Y and Z components of a vector in one line of code:

    ( x, y, z ) =
        Vector3d.components vector
-}
components : Vector3d -> ( Float, Float, Float )
components (Vector3d components') =
    components'


{-| Get the X component of a vector.

    Vector3d.xComponent (Vector3d ( 1, 2, 3 )) ==
        1
-}
xComponent : Vector3d -> Float
xComponent (Vector3d ( x, _, _ )) =
    x


{-| Get the Y component of a vector.

    Vector3d.yComponent (Vector3d ( 1, 2, 3 )) ==
        2
-}
yComponent : Vector3d -> Float
yComponent (Vector3d ( _, y, _ )) =
    y


{-| Get the Z component of a vector.

    Vector3d.zComponent (Vector3d ( 1, 2, 3 )) ==
        3
-}
zComponent : Vector3d -> Float
zComponent (Vector3d ( _, _, z )) =
    z


{-| Find the component of a vector in an arbitrary direction, for example

    verticalSpeed =
        Vector3d.componentIn upDirection velocity

This is more general and flexible than using `xComponent`, `yComponent` or
`zComponent`, all of which can be expressed in terms of `componentIn`:

    Vector3d.zComponent vector ==
        Vector3d.componentIn Direction3d.z vector
-}
componentIn : Direction3d -> Vector3d -> Float
componentIn (Direction3d components) =
    dotProduct (Vector3d components)


{-| Compare two vectors within a tolerance. Returns true if the difference
between the two given vectors has magnitude less than the given tolerance.

    firstVector =
        Vector3d ( 2, 1, 3 )

    secondVector =
        Vector3d ( 2.0002, 0.9999, 3.0001 )

    Vector3d.equalWithin 1e-3 firstVector secondVector ==
        True

    Vector3d.equalWithin 1e-6 firstVector secondVector ==
        False
-}
equalWithin : Float -> Vector3d -> Vector3d -> Bool
equalWithin tolerance firstVector secondVector =
    squaredLength (minus firstVector secondVector) <= tolerance * tolerance


{-| Get the length (magnitude) of a vector.

    Vector3d.length (Vector3d ( 2, 1, 2 )) ==
        3
-}
length : Vector3d -> Float
length =
    squaredLength >> sqrt


{-| Get the squared length of a vector. `squaredLength` is slightly faster than
`length`, so for example

    Vector3d.squaredLength vector > tolerance * tolerance

is equivalent to but slightly more efficient than

    Vector3d.length vector > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `length` is much more
readable!
-}
squaredLength : Vector3d -> Float
squaredLength vector =
    let
        ( x, y, z ) =
            components vector
    in
        x * x + y * y + z * z


{-| Attempt to find the direction of a vector. In the case of a zero vector,
return `Nothing`.

    Vector3d.direction (Vector3d ( 3, 0, 4 )) ==
        Just (Direction3d ( 0.6, 0, 0.8 ))

    Vector3d.direction (Vector3d ( 0, 0, 0 )) ==
        Nothing
-}
direction : Vector3d -> Maybe Direction3d
direction vector =
    if vector == zero then
        Nothing
    else
        let
            normalizedVector =
                times (1 / length vector) vector
        in
            Just (Direction3d (components normalizedVector))


{-| Attempt to find the length and direction of a vector. In the case of a zero
vector, returns `Nothing`.

    vector =
        Vector3d ( 3, 0, 4 )

    Vector3d.lengthAndDirection vector ==
        Just ( 5, Direction3d ( 0.6, 0, 0.8 ) )

    Vector3d.lengthAndDirection Vector3d.zero ==
        Nothing
-}
lengthAndDirection : Vector3d -> Maybe ( Float, Direction3d )
lengthAndDirection vector =
    let
        vectorLength =
            length vector
    in
        if vectorLength == 0.0 then
            Nothing
        else
            let
                normalizedVector =
                    times (1 / vectorLength) vector

                vectorDirection =
                    Direction3d (components normalizedVector)
            in
                Just ( vectorLength, vectorDirection )


{-| Negate a vector.

    Vector3d.negate (Vector3d ( 1, -3, 2 )) ==
        Vector3d ( -1, 3, -2 )
-}
negate : Vector3d -> Vector3d
negate vector =
    let
        ( x, y, z ) =
            components vector
    in
        Vector3d ( -x, -y, -z )


{-| Multiply a vector by a scalar.

    Vector3d.times 3 (Vector3d ( 1, 2, 3 )) ==
        Vector3d ( 3, 6, 9 )
-}
times : Float -> Vector3d -> Vector3d
times scale vector =
    let
        ( x, y, z ) =
            components vector
    in
        Vector3d ( x * scale, y * scale, z * scale )


{-| Add one vector to another.

    firstVector =
        Vector3d ( 1, 2, 3 )

    secondVector =
        Vector3d ( 4, 5, 6 )

    Vector3d.plus firstVector secondVector ==
        Vector3d ( 5, 7, 9 )
-}
plus : Vector3d -> Vector3d -> Vector3d
plus other vector =
    let
        ( x', y', z' ) =
            components other

        ( x, y, z ) =
            components vector
    in
        Vector3d ( x + x', y + y', z + z' )


{-| Subtract one vector from another. The vector to subtract is given first and
the vector to be subtracted from is given second, so

    firstVector =
        Vector3d ( 1, 1, 1 )

    secondVector =
        Vector3d ( 5, 6, 7 )

    Vector3d.minus firstVector secondVector ==
        Vector3d ( 4, 5, 6 )

or more generally, `Vector3d.minus a b` means `b - a`, not `a - b`. Think of
`Vector3d.minus a b` as the operation `minus a` being applied to the vector `b`.
-}
minus : Vector3d -> Vector3d -> Vector3d
minus other vector =
    let
        ( x', y', z' ) =
            components other

        ( x, y, z ) =
            components vector
    in
        Vector3d ( x - x', y - y', z - z' )


{-| Find the dot product of two vectors.

    firstVector =
        Vector3d ( 1, 0, 2 )

    secondVector =
        Vector3d ( 3, 4, 5 )

    Vector3d.dotProduct firstVector secondVector ==
        13
-}
dotProduct : Vector3d -> Vector3d -> Float
dotProduct first second =
    let
        ( x1, y1, z1 ) =
            components first

        ( x2, y2, z2 ) =
            components second
    in
        x1 * x2 + y1 * y2 + z1 * z2


{-| Find the cross product of two vectors.

    firstVector =
        Vector3d ( 2, 0, 0 )

    secondVector =
        Vector3d ( 0, 3, 0 )

    Vector3d.crossProduct firstVector secondVector ==
        Vector3d ( 0, 0, 6 )
-}
crossProduct : Vector3d -> Vector3d -> Vector3d
crossProduct first second =
    let
        ( x1, y1, z1 ) =
            components first

        ( x2, y2, z2 ) =
            components second
    in
        Vector3d
            ( y1 * z2 - z1 * y2
            , z1 * x2 - x1 * z2
            , x1 * y2 - y1 * x2
            )


{-| Rotate a vector around a given axis by a given angle (in radians).

    vector =
        Vector3d ( 2, 0, 1 )

    Vector3d.rotateAround Axis3d.x (degrees 90) vector ==
        Vector3d ( 2, -1, 0 )

    Vector3d.rotateAround Axis3d.z (degrees 45) vector ==
        Vector3d ( 1.4142, 1.4142, 1 )
-}
rotateAround : Axis3d -> Float -> Vector3d -> Vector3d
rotateAround axis angle =
    let
        (Axis3d { originPoint, direction }) =
            axis

        (Direction3d ( dx, dy, dz )) =
            direction

        halfAngle =
            0.5 * angle

        sinHalfAngle =
            sin halfAngle

        x =
            dx * sinHalfAngle

        y =
            dy * sinHalfAngle

        z =
            dz * sinHalfAngle

        w =
            cos halfAngle

        wx =
            w * x

        wy =
            w * y

        wz =
            w * z

        xx =
            x * x

        xy =
            x * y

        xz =
            x * z

        yy =
            y * y

        yz =
            y * z

        zz =
            z * z

        a00 =
            1 - 2 * (yy + zz)

        a10 =
            2 * (xy + wz)

        a20 =
            2 * (xz - wy)

        a01 =
            2 * (xy - wz)

        a11 =
            1 - 2 * (xx + zz)

        a21 =
            2 * (yz + wx)

        a02 =
            2 * (xz + wy)

        a12 =
            2 * (yz - wx)

        a22 =
            1 - 2 * (xx + yy)
    in
        \(Vector3d ( x, y, z )) ->
            Vector3d
                ( a00 * x + a01 * y + a02 * z
                , a10 * x + a11 * y + a12 * z
                , a20 * x + a21 * y + a22 * z
                )


{-| Mirror a vector across a plane.

    vector =
        Vector3d ( 1, 2, 3 )

    Vector3d.mirrorAcross Plane3d.xy vector ==
        Vector3d ( 1, 2, -3 )

    Vector3d.mirrorAcross Plane3d.yz vector ==
        Vector3d ( -1, 2, 3 )
-}
mirrorAcross : Plane3d -> Vector3d -> Vector3d
mirrorAcross plane =
    let
        (Plane3d { originPoint, normalDirection }) =
            plane

        (Direction3d ( dx, dy, dz )) =
            normalDirection

        a =
            1 - 2 * dx * dx

        b =
            1 - 2 * dy * dy

        c =
            1 - 2 * dz * dz

        d =
            -2 * dy * dz

        e =
            -2 * dx * dz

        f =
            -2 * dx * dy
    in
        \(Vector3d ( x, y, z )) ->
            Vector3d
                ( a * x + f * y + e * z
                , f * x + b * y + d * z
                , e * x + d * y + c * z
                )


{-| Find the projection of a vector in a particular direction. Conceptually,
this means splitting the original vector into a portion parallel to the given
direction and a portion perpendicular to it, then returning the parallel
portion.

    vector =
        Vector3d ( 1, 2, 3 )

    Vector3d.projectionIn Direction3d.x vector ==
        Vector3d ( 1, 0, 0 )

    Vector3d.projectionIn Direction3d.z vector ==
        Vector3d ( 0, 0, 3 )
-}
projectionIn : Direction3d -> Vector3d -> Vector3d
projectionIn direction vector =
    let
        (Direction3d directionComponents) =
            direction

        directionVector =
            Vector3d directionComponents
    in
        times (dotProduct vector directionVector) directionVector


{-| Project a vector onto a plane. Conceptually, this means splitting the
original vector into a portion parallel to the plane (perpendicular to the
plane's normal direction) and a portion perpendicular to it (parallel to its
normal direction), then returning the parallel (in-plane) portion.

    vector =
        Vector3d ( 2, 1, 3 )

    Vector3d.projectOnto Plane3d.xy vector ==
        Vector3d ( 2, 1, 0 )

    Vector3d.projectOnto Plane3d.xz vector ==
        Vector3d ( 2, 0, 3 )
-}
projectOnto : Plane3d -> Vector3d -> Vector3d
projectOnto plane vector =
    let
        (Plane3d { originPoint, normalDirection }) =
            plane
    in
        minus (projectionIn normalDirection vector) vector


{-| Take a vector defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    vector =
        Vector3d ( 2, 0, 3 )

    Vector3d.relativeTo rotatedFrame vector ==
        Vector3d ( 1.732, -1, 3 )
-}
relativeTo : Frame3d -> Vector3d -> Vector3d
relativeTo frame vector =
    let
        (Frame3d { originPoint, xDirection, yDirection, zDirection }) =
            frame
    in
        Vector3d
            ( componentIn xDirection vector
            , componentIn yDirection vector
            , componentIn zDirection vector
            )


{-| Take a vector defined in local coordinates relative to a given reference
frame, and return that vector expressed in global coordinates.

    vector =
        Vector3d ( 2, 0, 3 )

    Vector3d.placeIn rotatedFrame vector ==
        Vector3d ( 1.732, 1, 3 )
-}
placeIn : Frame3d -> Vector3d -> Vector3d
placeIn frame =
    let
        (Frame3d { originPoint, xDirection, yDirection, zDirection }) =
            frame

        (Direction3d ( x1, y1, z1 )) =
            xDirection

        (Direction3d ( x2, y2, z2 )) =
            yDirection

        (Direction3d ( x3, y3, z3 )) =
            zDirection
    in
        \(Vector3d ( x, y, z )) ->
            Vector3d
                ( x1 * x + x2 * y + x3 * z
                , y1 * x + y2 * y + y3 * z
                , z1 * x + z2 * y + z3 * z
                )


{-| Project a vector into a given sketch plane. Conceptually, this projects the
vector onto the plane and then expresses the projected vector in 2D sketch
coordinates.

    vector =
        Vector3d ( 2, 1, 3 )

    Vector3d.projectInto SketchPlane3d.xy vector ==
        Vector2d ( 2, 1 )

    Vector3d.projectInto SketchPlane3d.yz vector ==
        Vector2d ( 1, 3 )

    Vector3d.projectInto SketchPlane3d.zx vector ==
        Vector2d ( 3, 2 )
-}
projectInto : SketchPlane3d -> Vector3d -> Vector2d
projectInto sketchPlane vector =
    let
        (SketchPlane3d { originPoint, xDirection, yDirection }) =
            sketchPlane
    in
        Vector2d
            ( componentIn xDirection vector
            , componentIn yDirection vector
            )
