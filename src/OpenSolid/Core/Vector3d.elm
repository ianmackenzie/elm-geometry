{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Vector3d
    exposing
        ( zero
        , in'
        , on
        , relativeTo
        , perpendicularTo
        , components
        , xComponent
        , yComponent
        , zComponent
        , componentIn
        , length
        , squaredLength
        , direction
        , negate
        , times
        , plus
        , minus
        , dotProduct
        , crossProduct
        , rotateAround
        , mirrorAcross
        , projectOntoAxis
        , projectOnto
        , projectInto
        , localizeTo
        , placeIn
        , toRecord
        , fromRecord
        )

{-| Various functions for constructing `Vector3d` values and performing
operations on them. For the examples below, assume that all OpenSolid core types
have been imported using

    import OpenSolid.Core.Types exposing (..)

and all necessary modules have been imported using the following pattern:

    import OpenSolid.Core.Vector3d as Vector3d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Constants

@docs zero

Although there are no predefined constants for `Vector3d ( 1, 0, 0 )`,
`Vector3d ( 0, 1, 0 )` and `Vector3d ( 0, 0, 1 )`, in most cases you will
actually want their `Direction3d` versions `Direction3d.x`, `Direction3d.y` and
`Direction3d.z`.

# Constructors

Since `Vector3d` is not an opaque type, the simplest way to construct one is
directly from its X, Y and Z components, for example `Vector3d ( 2, 3, 4 )`. But
that is not the only way!

@docs in', on, relativeTo, perpendicularTo

# Components

@docs components, xComponent, yComponent, zComponent, componentIn

# Length and direction

@docs length, squaredLength, direction

# Arithmetic

@docs negate, times, plus, minus, dotProduct, crossProduct

# Transformations

@docs rotateAround, mirrorAcross, projectOntoAxis, projectOnto

# Coordinate conversions

Functions for transforming vectors between local and global coordinates in
different coordinate frames. For the examples below, assume the following
definition of a local coordinate frame, one that is rotated 45 degrees
counterclockwise about the Z axis from the global XYZ frame:

    frame =
        Frame3d.rotateAround Axis3d.z (degrees 45) Frame3d.xyz

    frame.xDirection == Direction3d ( 0.7071, 0.7071, 0 )
    frame.yDirection == Direction3d ( -0.7071, 0.7071, 0 )
    frame.zDirection == Direction3d ( 0, 0, 1 )

@docs projectInto, localizeTo, placeIn

# Record conversions

Convert `Vector3d` values to and from Elm records. Primarily useful for
interoperability with other libraries. For example, you could define conversion
functions to and from `elm-linear-algebra`'s `Vec3` type with

    toVec3 : Vector3d -> Math.Vector3.Vec3
    toVec3 =
        Vector3d.toRecord >> Math.Vector3.fromRecord

    fromVec3 : Math.Vector3.Vec3 -> Vector3d
    fromVec3 =
        Math.Vector3.toRecord >> Vector3d.fromRecord

although in this particular case it would likely be simpler and more efficient
to use

    toVec3 =
        Vector3d.components >> Math.Vector3.fromTuple

    fromVec3 =
        Math.Vector3.toTuple >> Vector3d

@docs toRecord, fromRecord
-}

import OpenSolid.Core.Types exposing (..)


{-| The zero vector.

    Vector3d.zero == Vector3d ( 0, 0, 0 )
-}
zero : Vector3d
zero =
    Vector3d ( 0, 0, 0 )


{-| Construct a vector parallel to the given axis, with the given magnitude. The
magnitude may be negative, in which case the vector will have an opposite
direction to the axis.

    Vector3d.in' Direction3d.x 5 == Vector3d ( 5, 0, 0 )
    Vector3d.in' Direction3d.y -3 == Vector3d ( 0, -3, 0 )
-}
in' : Direction3d -> Float -> Vector3d
in' direction magnitude =
    let
        (Direction3d components) =
            direction
    in
        times magnitude (Vector3d components)


{-| Construct a vector which lies on the given plane, with the given local
(planar) components.

    Vector3d.on Plane3d.xy ( 2, 3 ) == Vector3d ( 2, 3, 0 )
    Vector3d.on Plane3d.yz ( 2, 3 ) == Vector3d ( 0, 2, 3 )
    Vector3d.on Plane3d.zy ( 2, 3 ) == Vector3d ( 0, 3, 2 )

    Vector3d.on plane ( x, y ) ==
        Vector3d.plus (Direction3d.times x plane.xDirection)
            (Direction3d.times y plane.yDirection)
-}
on : Plane3d -> ( Float, Float ) -> Vector3d
on plane =
    let
        (Direction3d ( x1, y1, z1 )) =
            plane.xDirection

        (Direction3d ( x2, y2, z2 )) =
            plane.yDirection
    in
        \( x, y ) ->
            Vector3d ( x1 * x + x2 * y, y1 * x + y2 * y, z1 * x + z2 * y )


relativeTo : Frame3d -> ( Float, Float, Float ) -> Vector3d
relativeTo frame =
    let
        (Direction3d ( x1, y1, z1 )) =
            frame.xDirection

        (Direction3d ( x2, y2, z2 )) =
            frame.yDirection

        (Direction3d ( x3, y3, z3 )) =
            frame.zDirection
    in
        \( x, y, z ) ->
            let
                x' =
                    x1 * x + x2 * y + x3 * z

                y' =
                    y1 * x + y2 * y + y3 * z

                z' =
                    z1 * x + z2 * y + z3 * z
            in
                Vector3d ( x', y', z' )


{-| Construct an arbitrary vector perpendicular to the given vector. The exact
magnitude and direction of the resulting vector are not specified, but it is
guaranteed to be perpendicular to the given vector and non-zero (unless the
given vector is itself zero).

    Vector3d.perpendicularTo (Vector3d ( 3, 0, 0 )) == Vector3d ( 0, 0, -3 )
    Vector3d.perpendicularTo (Vector3d ( 1, 2, 3 )) == Vector3d ( 0, -3, 2 )
    Vector3d.perpendicularTo Vector3d.zero == Vector3d.zero
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

    Vector3d.components (Vector2d ( 2, 3, 4 )) == ( 2, 3, 4 )

This combined with Elm's built-in tuple destructuring provides a convenient way
to extract the X, Y and Z components of a vector in one line of code:

    ( x, y, z ) = Vector3d.components vector
-}
components : Vector3d -> ( Float, Float, Float )
components (Vector3d components') =
    components'


{-| Get the X component of a vector.

    Vector3d.xComponent (Vector3d ( 1, 2, 3 )) == 1
-}
xComponent : Vector3d -> Float
xComponent (Vector3d ( x, _, _ )) =
    x


{-| Get the Y component of a vector.

    Vector3d.yComponent (Vector3d ( 1, 2, 3 )) == 2
-}
yComponent : Vector3d -> Float
yComponent (Vector3d ( _, y, _ )) =
    y


{-| Get the Z component of a vector.

    Vector3d.zComponent (Vector3d ( 1, 2, 3 )) == 3
-}
zComponent : Vector3d -> Float
zComponent (Vector3d ( _, _, z )) =
    z


{-| Find the component of a vector in an arbitrary direction, for example

    verticalSpeed = Vector3d.componentIn upDirection velocityVector

This is more general and flexible than using `xComponent`, `yComponent` or
`zComponent`, all of which can be expressed in terms of `componentIn`:

    Vector3d.zComponent vector == Vector3d.componentIn Direction3d.z vector
-}
componentIn : Direction3d -> Vector3d -> Float
componentIn (Direction3d components) =
    dotProduct (Vector3d components)


{-| Get the length of a vector.

    Vector3d.length (Vector3d ( 1, 1, 1 )) == sqrt 3
-}
length : Vector3d -> Float
length =
    squaredLength >> sqrt


{-| Get the squared length of a vector. `squaredLength` is slightly faster than
`length`, so for example

    Vector3d.squaredLength vector > tolerance * tolerance

is equivalent to but slightly more efficient than

    Vector3d.length vector > tolerance

since the latter requires a square root. In many cases, however, the speed
difference will be negligible and using `length` is much more readable!
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

    Vector3d.direction (Vector3d ( 1, 0, 1 )) ==
        Just (Direction3d ( 0.7071, 0, 0.7071 ))

    Vector3d.direction (Vector3d ( 0, 0, 0 )) == Nothing

For instance, given an eye point and a point to look at, the corresponding view
direction could be determined with

    Vector3d.direction (Point3d.vectorFrom eyePoint lookAtPoint)

This would return a `Maybe Direction3d`, with `Nothing` corresponding to the
case where the eye point and point to look at are coincident (in which case the
view direction is not well-defined and some special-case logic is needed).
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


{-| Negate a vector.

    Vector3d.negate (Vector3d ( 1, -3, 2 )) == Vector3d ( -1, 3, -2 )
-}
negate : Vector3d -> Vector3d
negate vector =
    let
        ( x, y, z ) =
            components vector
    in
        Vector3d ( -x, -y, -z )


{-| Multiply a vector by a scalar.

    Vector3d.times 3 (Vector3d ( 1, 2, 3 )) == Vector3d ( 3, 6, 9 )
-}
times : Float -> Vector3d -> Vector3d
times scale vector =
    let
        ( x, y, z ) =
            components vector
    in
        Vector3d ( x * scale, y * scale, z * scale )


{-| Add one vector to another.

    Vector3d.plus (Vector3d ( 1, 2, 3 )) (Vector3d ( 4, 5, 6 )) == Vector3d ( 5, 7, 8 )
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

    Vector3d.minus (Vector3d ( 1, 1, 1 )) (Vector3d ( 5, 6, 7 )) ==
        Vector3d ( 4, 5, 6 )

This means that `minus` can be used more naturally in situations like `map`
functions

    minusVector =
        Vector3d.minus (Vector3d ( 2, 2, 2 ))

    originalVectors =
        [ Vector3d ( 1, 2, 3 ), Vector3d ( 3, 4, 5 ) ]

    List.map minusVector originalVectors ==
        [ Vector3d ( -1, 0, 1 ), Vector3d ( 1, 2, 3 ) ]

or function pipelining

    myFunction =
        Vector3d.minus (Vector3d ( 1, 1, 1 )) >> Vector3d.times 3

    myFunction (Vector3d ( 3, 2, 1 )) == Vector3d ( 6, 3, 0 )

where `myFunction` could be described in pseudo-English as '`minus` by
`Vector3d ( 1, 1, 1 )` and then `times` by 3'.
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

    Vector3d.dotProduct (Vector3d ( 1, 0, 2 )) (Vector3d ( 3, 4, 5 )) == 13
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

    Vector3d.crossProduct (Vector3d ( 2, 0, 0 )) (Vector3d ( 0, 3, 0 )) == Vector3d ( 0, 0, 6 )
-}
crossProduct : Vector3d -> Vector3d -> Vector3d
crossProduct first second =
    let
        ( x1, y1, z1 ) =
            components first

        ( x2, y2, z2 ) =
            components second
    in
        Vector3d ( y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2 )


rotateAround : Axis3d -> Float -> Vector3d -> Vector3d
rotateAround axis angle =
    let
        (Direction3d ( dx, dy, dz )) =
            axis.direction

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
        \vector ->
            let
                ( x, y, z ) =
                    components vector

                x' =
                    a00 * x + a01 * y + a02 * z

                y' =
                    a10 * x + a11 * y + a12 * z

                z' =
                    a20 * x + a21 * y + a22 * z
            in
                Vector3d ( x', y', z' )


mirrorAcross : Plane3d -> Vector3d -> Vector3d
mirrorAcross plane =
    let
        (Direction3d ( dx, dy, dz )) =
            plane.normalDirection

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
        \vector ->
            let
                ( x, y, z ) =
                    components vector

                x' =
                    a * x + f * y + e * z

                y' =
                    f * x + b * y + d * z

                z' =
                    e * x + d * y + c * z
            in
                Vector3d ( x', y', z' )


projectOntoAxis : Axis3d -> Vector3d -> Vector3d
projectOntoAxis axis vector =
    let
        (Direction3d directionComponents) =
            axis.direction

        directionVector =
            Vector3d directionComponents
    in
        times (dotProduct vector directionVector) directionVector


projectOnto : Plane3d -> Vector3d -> Vector3d
projectOnto plane vector =
    let
        normalAxis =
            Axis3d plane.originPoint plane.normalDirection
    in
        minus (projectOntoAxis normalAxis vector) vector


localizeTo : Frame3d -> Vector3d -> Vector3d
localizeTo frame vector =
    let
        x =
            componentIn frame.xDirection vector

        y =
            componentIn frame.yDirection vector

        z =
            componentIn frame.zDirection vector
    in
        Vector3d ( x, y, z )


placeIn : Frame3d -> Vector3d -> Vector3d
placeIn frame =
    components >> relativeTo frame


projectInto : Plane3d -> Vector3d -> Vector2d
projectInto plane vector =
    Vector2d
        ( componentIn plane.xDirection vector
        , componentIn plane.yDirection vector
        )


toRecord : Vector3d -> { x : Float, y : Float, z : Float }
toRecord vector =
    let
        ( x, y, z ) =
            components vector
    in
        { x = x, y = y, z = z }


fromRecord : { x : Float, y : Float, z : Float } -> Vector3d
fromRecord { x, y, z } =
    Vector3d ( x, y, z )
