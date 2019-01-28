--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Vector3d exposing
    ( Vector3d
    , zero
    , fromComponents, fromComponentsIn, from, withLength, on, perpendicularTo, interpolateFrom
    , fromTuple, toTuple, fromRecord, toRecord
    , components, componentsIn, xComponent, yComponent, componentIn, zComponent, length, squaredLength, direction, lengthAndDirection
    , equalWithin, lexicographicComparison
    , plus, minus, dot, cross
    , reverse, normalize, scaleBy, rotateAround, mirrorAcross, projectionIn, projectOnto
    , relativeTo, placeIn, projectInto
    )

{-| A `Vector3d` represents a quantity such as a displacement or velocity in 3D,
and is defined by its X, Y and Z components. This module contains a variety of
vector-related functionality, such as

  - Adding or subtracting vectors
  - Finding the lengths of vectors
  - Rotating vectors
  - Converting vectors between different coordinate systems

Note that unlike in many other geometry packages where vectors are used as a
general-purpose data type, `elm-geometry` has separate data types for vectors,
directions and points. In most code it is actually more common to use `Point3d`
and `Direction3d` than `Vector3d`, and much code can avoid working directly with
`Vector3d` values at all!

@docs Vector3d


# Predefined vectors

@docs zero

Although there are no predefined constants for the vectors with components
(1,&nbsp;0,&nbsp;0), (0,&nbsp;1,&nbsp;0) and (0,&nbsp;0,&nbsp;1), in most cases
you will actually want their `Direction3d` versions [`Direction3d.x`](Direction3d#x),
[`Direction3d.y`](Direction3d#y) and [`Direction3d.z`](Direction3d#z).


# Constructors

@docs fromComponents, fromComponentsIn, from, withLength, on, perpendicularTo, interpolateFrom


# Conversion

@docs fromTuple, toTuple, fromRecord, toRecord


# Properties

@docs components, componentsIn, xComponent, yComponent, componentIn, zComponent, length, squaredLength, direction, lengthAndDirection


# Comparison

@docs equalWithin, lexicographicComparison


# Arithmetic

@docs plus, minus, dot, cross


# Transformations

Note that for all transformations, only the orientation of the given axis or
plane is relevant, since vectors are position-independent. Think of transforming
a vector as placing its tail on the relevant axis or plane and then transforming
its tip.

@docs reverse, normalize, scaleBy, rotateAround, mirrorAcross, projectionIn, projectOnto


# Coordinate conversions

Like other transformations, coordinate transformations of vectors depend only on
the orientations of the relevant frames/sketch planes, not their positions.

For the examples, assume the following definition of a local coordinate frame,
one that is rotated 30 degrees counterclockwise around the Z axis from the
global XYZ frame:

    rotatedFrame =
        Frame3d.atOrigin |> Frame3d.rotateAround Axis3d.z (degrees 30)

@docs relativeTo, placeIn, projectInto

-}

import Angle exposing (Angle)
import Bootstrap.Axis3d as Axis3d
import Bootstrap.Direction3d as Direction3d
import Bootstrap.Frame3d as Frame3d
import Bootstrap.Plane3d as Plane3d
import Bootstrap.Point3d as Point3d
import Bootstrap.SketchPlane3d as SketchPlane3d
import Geometry.Types as Types exposing (Axis3d, Direction3d, Frame3d, Plane3d, Point3d, SketchPlane3d)
import Quantity exposing (Cubed, Product, Quantity, Squared, Unitless)
import Quantity.Extra as Quantity
import Vector2d exposing (Vector2d)


{-| -}
type alias Vector3d units coordinates =
    Types.Vector3d units coordinates


{-| The zero vector.

    Vector3d.zero
    --> Vector3d.fromComponents ( 0, 0, 0 )

-}
zero : Vector3d units coordinates
zero =
    fromComponents ( Quantity.zero, Quantity.zero, Quantity.zero )


{-| Construct a vector from its X, Y and Z components.

    vector =
        Vector3d.fromComponents ( 2, 1, 3 )

-}
fromComponents : ( Quantity Float units, Quantity Float units, Quantity Float units ) -> Vector3d units coordinates
fromComponents givenComponents =
    Types.Vector3d givenComponents


{-| Construct a vector given its local components within a particular frame:

    frame =
        Frame3d.atOrigin
            |> Frame3d.rotateAround Axis3d.z
                (Angle.degrees 45)

    Vector3d.fromComponentsIn frame
        ( Speed.feetPerSecond 1
        , Speed.feetPerSecond 0
        , Speed.feetPerSecond 2
        )
    --> Vector3d.fromComponents
    -->     ( Speed.feetPerSecond 0.7071
    -->     , Speed.feetPerSecond 0.7071
    -->     , Speed.feetPerSecond 2
    -->     )

-}
fromComponentsIn : Frame3d units globalCoordinates localCoordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units ) -> Vector3d units globalCoordinates
fromComponentsIn frame localComponents =
    let
        ( x, y, z ) =
            localComponents

        ( x1, y1, z1 ) =
            Direction3d.components (Frame3d.xDirection frame)

        ( x2, y2, z2 ) =
            Direction3d.components (Frame3d.yDirection frame)

        ( x3, y3, z3 ) =
            Direction3d.components (Frame3d.zDirection frame)
    in
    fromComponents
        ( Quantity.aXbYcZ x1 x x2 y x3 z
        , Quantity.aXbYcZ y1 x y2 y y3 z
        , Quantity.aXbYcZ z1 x z2 y z3 z
        )


{-| Construct a vector from the first given point to the second.

    startPoint =
        Point3d.fromCoordinates ( 1, 1, 1 )

    endPoint =
        Point3d.fromCoordinates ( 4, 5, 6 )

    Vector3d.from startPoint endPoint
    --> Vector3d.fromComponents ( 3, 4, 5 )

-}
from : Point3d units coordinates -> Point3d units coordinates -> Vector3d units coordinates
from firstPoint secondPoint =
    let
        ( x1, y1, z1 ) =
            Point3d.coordinates firstPoint

        ( x2, y2, z2 ) =
            Point3d.coordinates secondPoint
    in
    fromComponents
        ( x2 |> Quantity.minus x1
        , y2 |> Quantity.minus y1
        , z2 |> Quantity.minus z1
        )


{-| Construct a vector with the given length in the given direction.

    Vector3d.withLength 5 Direction3d.y
    --> Vector3d.fromComponents ( 0, 5, 0 )

-}
withLength : Quantity Float units -> Direction3d coordinates -> Vector3d units coordinates
withLength givenLength givenDirection =
    let
        ( dx, dy, dz ) =
            Direction3d.components givenDirection
    in
    fromComponents
        ( Quantity.multiplyBy dx givenLength
        , Quantity.multiplyBy dy givenLength
        , Quantity.multiplyBy dz givenLength
        )


{-| Construct a 3D vector lying _on_ a sketch plane by providing a 2D vector
specified in XY coordinates _within_ the sketch plane.

    vector2d =
        Vector2d.fromComponents ( 2, 3 )

    Vector3d.on SketchPlane3d.xy vector2d
    --> Vector3d.fromComponents ( 2, 3, 0 )

    Vector3d.on SketchPlane3d.yz vector2d
    --> Vector3d.fromComponents ( 0, 2, 3 )

    Vector3d.on SketchPlane3d.zx vector2d
    --> Vector3d.fromComponents ( 3, 0, 2 )

A slightly more complex example:

    tiltedSketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround Axis3d.x
                (degrees 45)

    Vector3d.on tiltedSketchPlane <|
        Vector2d.fromComponents ( 1, 1 )
    --> Vector3d.fromComponents ( 1, 0.7071, 0.7071 )

-}
on : SketchPlane3d units coordinates3d coordinates2d -> Vector2d units coordinates2d -> Vector3d units coordinates3d
on sketchPlane vector2d =
    let
        ( ux, uy, uz ) =
            Direction3d.components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            Direction3d.components (SketchPlane3d.yDirection sketchPlane)

        ( x, y ) =
            Vector2d.components vector2d
    in
    fromComponents
        ( Quantity.aXbY ux x vx y
        , Quantity.aXbY uy x vy y
        , Quantity.aXbY uz x vz y
        )


{-| Construct an arbitrary vector perpendicular to the given vector. The exact
length and direction of the resulting vector are not specified, but it is
guaranteed to be perpendicular to the given vector and non-zero (unless the
given vector is itself zero).

    Vector3d.perpendicularTo
        (Vector3d.fromComponents ( 3, 0, 0 ))
    --> Vector3d.fromComponents ( 0, 0, -3 )

    Vector3d.perpendicularTo
        (Vector3d.fromComponents ( 1, 2, 3 ))
    --> Vector3d.fromComponents ( 0, -3, 2 )

    Vector3d.perpendicularTo Vector3d.zero
    --> Vector3d.zero

-}
perpendicularTo : Vector3d units coordinates -> Vector3d units coordinates
perpendicularTo vector =
    let
        ( x, y, z ) =
            components vector

        absX =
            Quantity.abs x

        absY =
            Quantity.abs y

        absZ =
            Quantity.abs z
    in
    if absX |> Quantity.lessThanOrEqualTo absY then
        if absX |> Quantity.lessThanOrEqualTo absZ then
            fromComponents ( Quantity.zero, Quantity.negate z, y )

        else
            fromComponents ( Quantity.negate y, x, Quantity.zero )

    else if absY |> Quantity.lessThanOrEqualTo absZ then
        fromComponents ( z, Quantity.zero, Quantity.negate x )

    else
        fromComponents ( Quantity.negate y, x, Quantity.zero )


{-| Construct a vector by interpolating from the first given vector to the
second, based on a parameter that ranges from zero to one.

    startVector =
        Vector3d.fromComponents ( 1, 2, 4 )

    endVector =
        Vector3d.fromComponents ( 1, 3, 8 )

    Vector3d.interpolateFrom startVector endVector 0.25
    --> Vector3d.fromComponents ( 1, 2.25, 5 )

Partial application may be useful:

    interpolatedVector : Float -> Vector3d
    interpolatedVector =
        Vector3d.interpolateFrom startVector endVector

    List.map interpolatedVector [ 0, 0.5, 1 ]
    --> [ Vector3d.fromComponents ( 1, 2, 4 )
    --> , Vector3d.fromComponents ( 1, 2, 6 )
    --> , Vector3d.fromComponents ( 1, 2, 8 )
    --> ]

You can pass values less than zero or greater than one to extrapolate:

    interpolatedVector -0.5
    --> Vector3d.fromComponents ( 1, 2, 2 )

    interpolatedVector 1.25
    --> Vector3d.fromComponents ( 1, 2, 9 )

-}
interpolateFrom : Vector3d units coordinates -> Vector3d units coordinates -> Float -> Vector3d units coordinates
interpolateFrom firstVector secondVector givenParameter =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    fromComponents
        ( Quantity.interpolateFrom x1 x2 givenParameter
        , Quantity.interpolateFrom y1 y2 givenParameter
        , Quantity.interpolateFrom z1 z2 givenParameter
        )


fromTuple : ( Float, Float, Float ) -> Vector3d Unitless coordinates
fromTuple ( x, y, z ) =
    fromComponents
        ( Quantity.float x
        , Quantity.float y
        , Quantity.float z
        )


toTuple : Vector3d Unitless coordinates -> ( Float, Float, Float )
toTuple vector =
    ( Quantity.toFloat (xComponent vector)
    , Quantity.toFloat (yComponent vector)
    , Quantity.toFloat (zComponent vector)
    )


fromRecord : { x : Float, y : Float, z : Float } -> Vector3d Unitless coordinates
fromRecord { x, y, z } =
    fromComponents
        ( Quantity.float x
        , Quantity.float y
        , Quantity.float z
        )


toRecord : Vector3d Unitless coordinates -> { x : Float, y : Float, z : Float }
toRecord vector =
    { x = Quantity.toFloat (xComponent vector)
    , y = Quantity.toFloat (yComponent vector)
    , z = Quantity.toFloat (zComponent vector)
    }


{-| Extract the components of a vector.

    Vector3d.fromComponents ( 2, 3, 4 )
        |> Vector3d.components
    --> ( 2, 3, 4 )

This combined with Elm's built-in tuple destructuring provides a convenient way
to extract the X, Y and Z components of a vector in one line of code:

    ( x, y, z ) =
        Vector3d.components vector

-}
components : Vector3d units coordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units )
components (Types.Vector3d vectorComponents) =
    vectorComponents


{-| Find the components of a vector in a given frame;

    Vector3d.componentsIn frame vector

is equivalent to

    ( Vector3d.componentIn (Frame3d.xDirection frame) vector
    , Vector3d.componentIn (Frame3d.yDirection frame) vector
    , Vector3d.componentIn (Frame3d.zDirection frame) vector
    )

-}
componentsIn : Frame3d units globalCoordinates localCoordinates -> Vector3d units globalCoordinates -> ( Quantity Float units, Quantity Float units, Quantity Float units )
componentsIn frame vector =
    ( vector |> componentIn (Frame3d.xDirection frame)
    , vector |> componentIn (Frame3d.yDirection frame)
    , vector |> componentIn (Frame3d.zDirection frame)
    )


{-| Get the X component of a vector.

    Vector3d.fromComponents ( 1, 2, 3 )
        |> Vector3d.xComponent
    --> 1

-}
xComponent : Vector3d units coordinates -> Quantity Float units
xComponent (Types.Vector3d ( x, _, _ )) =
    x


{-| Get the Y component of a vector.

    Vector3d.fromComponents ( 1, 2, 3 )
        |> Vector3d.yComponent
    --> 2

-}
yComponent : Vector3d units coordinates -> Quantity Float units
yComponent (Types.Vector3d ( _, y, _ )) =
    y


{-| Get the Z component of a vector.

    Vector3d.fromComponents ( 1, 2, 3 )
        |> Vector3d.zComponent
    --> 3

-}
zComponent : Vector3d units coordinates -> Quantity Float units
zComponent (Types.Vector3d ( _, _, z )) =
    z


{-| Find the component of a vector in an arbitrary direction, for example

    verticalSpeed =
        Vector3d.componentIn upDirection velocity

This is more general and flexible than using `xComponent`, `yComponent` or
`zComponent`, all of which can be expressed in terms of `componentIn`; for
example,

    Vector3d.zComponent vector

is equivalent to

    Vector3d.componentIn Direction3d.z vector

-}
componentIn : Direction3d coordinates -> Vector3d units coordinates -> Quantity Float units
componentIn givenDirection givenVector =
    let
        ( dx, dy, dz ) =
            Direction3d.components givenDirection

        ( vx, vy, vz ) =
            components givenVector
    in
    Quantity.aXbYcZ dx vx dy vy dz vz


{-| Compare two vectors within a tolerance. Returns true if the difference
between the two given vectors has magnitude less than the given tolerance.

    firstVector =
        Vector3d.fromComponents ( 2, 1, 3 )

    secondVector =
        Vector3d.fromComponents ( 2.0002, 0.9999, 3.0001 )

    Vector3d.equalWithin 1e-3 firstVector secondVector
    --> True

    Vector3d.equalWithin 1e-6 firstVector secondVector
    --> False

-}
equalWithin : Quantity Float units -> Vector3d units coordinates -> Vector3d units coordinates -> Bool
equalWithin givenTolerance firstVector secondVector =
    if givenTolerance |> Quantity.lessThan Quantity.zero then
        False

    else
        let
            difference =
                secondVector |> minus firstVector
        in
        squaredLength difference
            |> Quantity.lessThanOrEqualTo (Quantity.squared givenTolerance)


{-| Compare two `Vector3d` values lexicographically: first by X component, then
by Y, then by Z. Can be used to provide a sort order for `Vector3d` values.
-}
lexicographicComparison : Vector3d units coordinates -> Vector3d units coordinates -> Order
lexicographicComparison firstVector secondVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    if x1 /= x2 then
        Quantity.compare x1 x2

    else if y1 /= y2 then
        Quantity.compare y1 y2

    else
        Quantity.compare z1 z2


{-| Get the length (magnitude) of a vector.

    Vector3d.length (Vector3d.fromComponents ( 2, 1, 2 ))
    --> 3

-}
length : Vector3d units coordinates -> Quantity Float units
length vector =
    Quantity.sqrt (squaredLength vector)


{-| Get the squared length of a vector. `squaredLength` is slightly faster than
`length`, so for example

    Vector3d.squaredLength vector > tolerance * tolerance

is equivalent to but slightly more efficient than

    Vector3d.length vector > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `length` is much more
readable!

-}
squaredLength : Vector3d units coordinates -> Quantity Float (Squared units)
squaredLength vector =
    let
        ( x, y, z ) =
            components vector
    in
    Quantity.squared x
        |> Quantity.plus (Quantity.squared y)
        |> Quantity.plus (Quantity.squared z)


{-| Attempt to find the direction of a vector. In the case of a zero vector,
returns `Nothing`.

    Vector3d.fromComponents ( 3, 0, 3 )
        |> Vector3d.direction
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees 0)
    -->         (degrees 45)
    -->     )

    Vector3d.direction Vector3d.zero
    --> Nothing

-}
direction : Vector3d units coordinates -> Maybe (Direction3d coordinates)
direction givenVector =
    let
        vectorLength =
            length givenVector
    in
    if vectorLength == Quantity.zero then
        Nothing

    else
        let
            ( vx, vy, vz ) =
                components givenVector
        in
        Just <|
            Direction3d.unsafeFromComponents
                ( Quantity.ratio vx vectorLength
                , Quantity.ratio vy vectorLength
                , Quantity.ratio vz vectorLength
                )


{-| Attempt to find the length and direction of a vector. In the case of a zero
vector, returns `Nothing`.

    vector =
        Vector3d.fromComponents ( 3, 0, 3 )

    Vector3d.lengthAndDirection vector
    --> Just
    -->     ( 4.2426
    -->     , Direction3d.fromAzimuthAndElevation
    -->         (degrees 0)
    -->         (degrees 45)
    -->     )

    Vector3d.lengthAndDirection Vector3d.zero
    --> Nothing

-}
lengthAndDirection : Vector3d units coordinates -> Maybe ( Quantity Float units, Direction3d coordinates )
lengthAndDirection givenVector =
    let
        vectorLength =
            length givenVector
    in
    if vectorLength == Quantity.zero then
        Nothing

    else
        let
            ( vx, vy, vz ) =
                components givenVector

            vectorDirection =
                Direction3d.unsafeFromComponents
                    ( Quantity.ratio vx vectorLength
                    , Quantity.ratio vy vectorLength
                    , Quantity.ratio vz vectorLength
                    )
        in
        Just ( vectorLength, vectorDirection )


{-| Normalize a vector to have a length of one. Zero vectors are left as-is.

    vector =
        Vector3d.fromComponents ( 3, 0, 4 )

    Vector3d.normalize vector
    --> Vector3d.fromComponents ( 0.6, 0, 0.8 )

    Vector3d.normalize Vector3d.zero
    --> Vector3d.zero

**Warning**: `Vector3d.direction` is safer since it forces you to explicitly
consider the case where the given vector is zero. `Vector3d.normalize` is
primarily useful for cases like generating WebGL meshes, where defaulting to a
zero vector for degenerate cases is acceptable, and the overhead of something
like

    Vector3d.direction vector
        |> Maybe.map Direction3d.toVector
        |> Maybe.withDefault Vector3d.zero

(which is functionally equivalent to `Vector3d.normalize vector`) is too high.

-}
normalize : Vector3d units coordinates -> Vector3d Unitless coordinates
normalize givenVector =
    let
        vectorLength =
            length givenVector
    in
    if vectorLength == Quantity.zero then
        zero

    else
        let
            ( vx, vy, vz ) =
                components givenVector
        in
        fromComponents
            ( Quantity.float (Quantity.ratio vx vectorLength)
            , Quantity.float (Quantity.ratio vy vectorLength)
            , Quantity.float (Quantity.ratio vz vectorLength)
            )


{-| Find the sum of two vectors.

    firstVector =
        Vector3d.fromComponents ( 1, 2, 3 )

    secondVector =
        Vector3d.fromComponents ( 4, 5, 6 )

    Vector3d.sum firstVector secondVector
    --> Vector3d.fromComponents ( 5, 7, 9 )

-}
plus : Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
plus secondVector firstVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    fromComponents
        ( x1 |> Quantity.plus x2
        , y1 |> Quantity.plus y2
        , z1 |> Quantity.plus z2
        )


{-| Find the difference between two vectors (the first vector minus the second).

    firstVector =
        Vector3d.fromComponents ( 5, 6, 7 )

    secondVector =
        Vector3d.fromComponents ( 1, 1, 1 )

    Vector3d.difference firstVector secondVector
    --> Vector3d.fromComponents ( 4, 5, 6 )

-}
minus : Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
minus secondVector firstVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    fromComponents
        ( x1 |> Quantity.minus x2
        , y1 |> Quantity.minus y2
        , z1 |> Quantity.minus z2
        )


{-| Find the dot product of two vectors.

    firstVector =
        Vector3d.fromComponents
            ( Length.meters 1
            , Length.meters 0
            , Length.meters 2
            )

    secondVector =
        Vector3d.fromComponents
            ( Length.meters 3
            , Length.meters 4
            , Length.meters 5
            )

    firstVector |> Vector3d.dot secondVector
    --> Area.squareMeters 13

-}
dot : Vector3d units2 coordinates -> Vector3d units1 coordinates -> Quantity Float (Product units1 units2)
dot secondVector firstVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    (x1 |> Quantity.times x2)
        |> Quantity.plus (y1 |> Quantity.times y2)
        |> Quantity.plus (z1 |> Quantity.times z2)


{-| Find the cross product of two vectors.

    firstVector =
        Vector3d.fromComponents
            ( Length.meters 2
            , Length.meters 0
            , Length.meters 0
            )

    secondVector =
        Vector3d.fromComponents
            ( Length.meters 0
            , Length.meters 3
            , Length.meters 0
            )

    firstVector |> Vector3d.cross secondVector
    --> Vector3d.fromComponents
    -->     ( Quantity.zero
    -->     , Quantity.zero
    -->     , Area.squareMeters 6
    -->     )

Note the argument order - `v1 x v2` would be written as

    v1 |> Vector3d.cross v2

which is the same as

    Vector3d.cross v2 v1

but the _opposite_ of

    Vector3d.cross v1 v2

-}
cross : Vector3d units2 coordinates -> Vector3d units1 coordinates -> Vector3d (Product units1 units2) coordinates
cross secondVector firstVector =
    let
        ( x1, y1, z1 ) =
            components firstVector

        ( x2, y2, z2 ) =
            components secondVector
    in
    fromComponents
        ( (y1 |> Quantity.times z2) |> Quantity.minus (z1 |> Quantity.times y2)
        , (z1 |> Quantity.times x2) |> Quantity.minus (x1 |> Quantity.times z2)
        , (x1 |> Quantity.times y2) |> Quantity.minus (y1 |> Quantity.times x2)
        )


{-| Reverse the direction of a vector, negating its components.

    Vector3d.reverse (Vector3d.fromComponents ( 1, -3, 2 ))
    --> Vector3d.fromComponents ( -1, 3, -2 )

(This could have been called `negate`, but `reverse` is more consistent with
the naming used in other modules.)

-}
reverse : Vector3d units coordinates -> Vector3d units coordinates
reverse vector =
    let
        ( x, y, z ) =
            components vector
    in
    fromComponents ( Quantity.negate x, Quantity.negate y, Quantity.negate z )


{-| Scale the length of a vector by a given scale.

    Vector3d.fromComponents ( 1, 2, 3 )
        |> Vector3d.scaleBy 3
    --> Vector3d.fromComponents ( 3, 6, 9 )

(This could have been called `multiply` or `times`, but `scaleBy` was chosen as
a more geometrically meaningful name and to be consistent with the `scaleAbout`
name used in other modules.)

-}
scaleBy : Float -> Vector3d units coordinates -> Vector3d units coordinates
scaleBy scale vector =
    let
        ( x, y, z ) =
            components vector
    in
    fromComponents
        ( Quantity.multiplyBy scale x
        , Quantity.multiplyBy scale y
        , Quantity.multiplyBy scale z
        )


{-| Rotate a vector around a given axis by a given angle (in radians).

    vector =
        Vector3d.fromComponents ( 2, 0, 1 )

    Vector3d.rotateAround Axis3d.x (degrees 90) vector
    --> Vector3d.fromComponents ( 2, -1, 0 )

    Vector3d.rotateAround Axis3d.z (degrees 45) vector
    --> Vector3d.fromComponents ( 1.4142, 1.4142, 1 )

-}
rotateAround : Axis3d units coordinates -> Angle -> Vector3d units coordinates -> Vector3d units coordinates
rotateAround axis angle =
    let
        ( ax, ay, az ) =
            Direction3d.components (Axis3d.direction axis)

        halfAngle =
            Quantity.multiplyBy 0.5 angle

        sinHalfAngle =
            Angle.sin halfAngle

        qx =
            ax * sinHalfAngle

        qy =
            ay * sinHalfAngle

        qz =
            az * sinHalfAngle

        qw =
            Angle.cos halfAngle

        wx =
            qw * qx

        wy =
            qw * qy

        wz =
            qw * qz

        xx =
            qx * qx

        xy =
            qx * qy

        xz =
            qx * qz

        yy =
            qy * qy

        yz =
            qy * qz

        zz =
            qz * qz

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
        in
        fromComponents
            ( Quantity.aXbYcZ a00 x a01 y a02 z
            , Quantity.aXbYcZ a10 x a11 y a12 z
            , Quantity.aXbYcZ a20 x a21 y a22 z
            )


{-| Mirror a vector across a plane.

    vector =
        Vector3d.fromComponents ( 1, 2, 3 )

    Vector3d.mirrorAcross Plane3d.xy vector
    --> Vector3d.fromComponents ( 1, 2, -3 )

    Vector3d.mirrorAcross Plane3d.yz vector
    --> Vector3d.fromComponents ( -1, 2, 3 )

-}
mirrorAcross : Plane3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
mirrorAcross plane =
    let
        ( dx, dy, dz ) =
            Direction3d.components (Plane3d.normalDirection plane)

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
        in
        fromComponents
            ( Quantity.aXbYcZ a x f y e z
            , Quantity.aXbYcZ f x b y d z
            , Quantity.aXbYcZ e x d y c z
            )


{-| Find the projection of a vector in a particular direction. Conceptually,
this means splitting the original vector into a portion parallel to the given
direction and a portion perpendicular to it, then returning the parallel
portion.

    vector =
        Vector3d.fromComponents ( 1, 2, 3 )

    Vector3d.projectionIn Direction3d.x vector
    --> Vector3d.fromComponents ( 1, 0, 0 )

    Vector3d.projectionIn Direction3d.z vector
    --> Vector3d.fromComponents ( 0, 0, 3 )

-}
projectionIn : Direction3d coordinates -> Vector3d units coordinates -> Vector3d units coordinates
projectionIn givenDirection givenVector =
    givenDirection |> withLength (givenVector |> componentIn givenDirection)


{-| Project a vector [orthographically](https://en.wikipedia.org/wiki/Orthographic_projection)
onto a plane. Conceptually, this means splitting the original vector into a
portion parallel to the plane (perpendicular to the plane's normal direction)
and a portion perpendicular to it (parallel to its normal direction), then
returning the parallel (in-plane) portion.

    vector =
        Vector3d.fromComponents ( 2, 1, 3 )

    Vector3d.projectOnto Plane3d.xy vector
    --> Vector3d.fromComponents ( 2, 1, 0 )

    Vector3d.projectOnto Plane3d.xz vector
    --> Vector3d.fromComponents ( 2, 0, 3 )

-}
projectOnto : Plane3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates
projectOnto givenPlane givenVector =
    givenVector
        |> minus
            (projectionIn (Plane3d.normalDirection givenPlane) givenVector)


{-| Take a vector defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    vector =
        Vector3d.fromComponents ( 2, 0, 3 )

    Vector3d.relativeTo rotatedFrame vector
    --> Vector3d.fromComponents ( 1.732, -1, 3 )

-}
relativeTo : Frame3d units globalCoordinates localCoordinates -> Vector3d units globalCoordinates -> Vector3d units localCoordinates
relativeTo frame vector =
    fromComponents
        ( componentIn (Frame3d.xDirection frame) vector
        , componentIn (Frame3d.yDirection frame) vector
        , componentIn (Frame3d.zDirection frame) vector
        )


{-| Take a vector defined in local coordinates relative to a given reference
frame, and return that vector expressed in global coordinates.

    vector =
        Vector3d.fromComponents ( 2, 0, 3 )

    Vector3d.placeIn rotatedFrame vector
    --> Vector3d.fromComponents ( 1.732, 1, 3 )

-}
placeIn : Frame3d units globalCoordinates localCoordinates -> Vector3d units localCoordinates -> Vector3d units globalCoordinates
placeIn frame vector =
    let
        ( x1, y1, z1 ) =
            Direction3d.components (Frame3d.xDirection frame)

        ( x2, y2, z2 ) =
            Direction3d.components (Frame3d.yDirection frame)

        ( x3, y3, z3 ) =
            Direction3d.components (Frame3d.zDirection frame)

        ( x, y, z ) =
            components vector
    in
    fromComponents
        ( Quantity.aXbYcZ x1 x x2 y x3 z
        , Quantity.aXbYcZ y1 x y2 y y3 z
        , Quantity.aXbYcZ z1 x z2 y z3 z
        )


{-| Project a vector into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the vector onto the plane and then expresses the projected vector in 2D
sketch coordinates.

    vector =
        Vector3d.fromComponents ( 2, 1, 3 )

    Vector3d.projectInto SketchPlane3d.xy vector
    --> Vector2d.fromComponents ( 2, 1 )

    Vector3d.projectInto SketchPlane3d.yz vector
    --> Vector2d.fromComponents ( 1, 3 )

    Vector3d.projectInto SketchPlane3d.zx vector
    --> Vector2d.fromComponents ( 3, 2 )

-}
projectInto : SketchPlane3d units coordinates coordinates2d -> Vector3d units coordinates -> Vector2d units coordinates2d
projectInto sketchPlane vector =
    Vector2d.fromComponents
        ( componentIn (SketchPlane3d.xDirection sketchPlane) vector
        , componentIn (SketchPlane3d.yDirection sketchPlane) vector
        )
