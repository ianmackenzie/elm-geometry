{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Vector2d
    exposing
        ( zero
        , perpendicularTo
        , components
        , xComponent
        , yComponent
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
        , rotateBy
        , mirrorAcross
        , projectionIn
        , projectOnto
        , relativeTo
        , placeIn
        , placeOnto
        )

{-| Various functions for creating and working with `Vector2d` values. For the
examples below, assume that all OpenSolid core types have been imported using

    import OpenSolid.Geometry.Types exposing (..)

and all other necessary modules have been imported using the following pattern:

    import OpenSolid.Vector2d as Vector2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Constants

@docs zero

Although there are no predefined constants for `Vector2d ( 1, 0 )` and
`Vector2d ( 0, 1 )`, in most cases you will actually want their `Direction2d`
versions `Direction2d.x` and `Direction2d.y`.

# Constructors

The simplest way to construct a `Vector2d` value is by passing a tuple of X and
Y components to the `Vector2d` constructor, for example `Vector2d ( 2, 3 )`.

There are no specific functions to create vectors from polar components, but you
can use Elm's built-in `fromPolar` function:

    vector =
        Vector2d (fromPolar ( radius, angle ))

@docs perpendicularTo

# Components

@docs components, xComponent, yComponent, componentIn

# Comparison

@docs equalWithin

# Length and direction

@docs length, squaredLength, direction, lengthAndDirection

# Arithmetic

@docs negate, times, plus, minus, dotProduct, crossProduct

# Transformations

Note that for `mirrorAcross` and `projectOnto`, only the direction of the axis
affects the result, since vectors are position-independent. Think of
mirroring/projecting a vector across/onto an axis as moving the vector so its
tail is on the axis, then mirroring/projecting its tip across/onto the axis.

@docs rotateBy, mirrorAcross, projectionIn, projectOnto

# Coordinate frames

Functions for transforming vectors between local and global coordinates in
different coordinate frames. Like other transformations, coordinate conversions
of vectors depend only on the orientations of the relevant frames, not the
positions of their origin points.

For the examples, assume the following frames have been defined:

    upsideDownFrame =
        Frame2d
            { originPoint = Point2d.origin
            , xDirection = Direction2d.x
            , yDirection = Direction2d.negate Direction2d.y
            }

    rotatedFrame =
        Frame2d.rotateBy (degrees 30) Frame2d.xy

@docs relativeTo, placeIn

# Sketch planes

@docs placeOnto
-}

import OpenSolid.Geometry.Types exposing (..)


{-| The zero vector.

    Vector2d.zero ==
        Vector2d ( 0, 0 )
-}
zero : Vector2d
zero =
    Vector2d ( 0, 0 )


{-| Construct a vector perpendicular to the given vector, by rotating the given
vector 90 degrees counterclockwise. The constructed vector will have the same
length as the given vector.

    Vector2d.perpendicularTo (Vector2d ( 1, 0 )) ==
        Vector2d ( 0, 1 )

    Vector2d.perpendicularTo (Vector2d ( 0, 2 )) ==
        Vector2d ( -2, 0 )

    Vector2d.perpendicularTo (Vector2d ( 3, 1 )) ==
        Vector2d ( -1, 3 )

    Vector2d.perpendicularTo Vector2d.zero ==
        Vector2d.zero
-}
perpendicularTo : Vector2d -> Vector2d
perpendicularTo vector =
    let
        ( x, y ) =
            components vector
    in
        Vector2d ( -y, x )


{-| Extract the components of a vector.

    Vector2d.components (Vector2d ( 2, 3 )) ==
        ( 2, 3 )

This combined with Elm's built-in tuple destructuring provides a convenient way
to extract both the X and Y components of a vector in one line of code:

    ( x, y ) =
        Vector2d.components vector

To get the polar components of a vector, you can use Elm's built in `toPolar`
function:

    ( radius, angle ) =
        toPolar (Vector2d.components vector)
-}
components : Vector2d -> ( Float, Float )
components (Vector2d components') =
    components'


{-| Get the X component of a vector.

    Vector2d.xComponent (Vector2d ( 2, 3 )) ==
        2
-}
xComponent : Vector2d -> Float
xComponent =
    components >> fst


{-| Get the Y component of a vector.

    Vector2d.yComponent (Vector2d ( 2, 3 )) ==
        3
-}
yComponent : Vector2d -> Float
yComponent =
    components >> snd


{-| Find the component of a vector in an arbitrary direction, for example

    forwardSpeed =
        Vector2d.componentIn forwardDirection velocity

This is more general and flexible than using `xComponent` or `yComponent`, both
of which can be expressed in terms of `componentIn`:

    Vector2d.xComponent vector ==
        Vector2d.componentIn Direction2d.x vector
-}
componentIn : Direction2d -> Vector2d -> Float
componentIn (Direction2d ( dx, dy )) (Vector2d ( x, y )) =
    x * dx + y * dy


{-| Compare two vectors within a tolerance. Returns true if the difference
between the two given vectors has length less than the given tolerance.

    firstVector =
        Vector2d ( 1, 2 )

    secondVector =
        Vector2d ( 0.9999, 2.0002 )

    Vector2d.equalWithin 1e-3 firstVector secondVector ==
        True

    Vector2d.equalWithin 1e-6 firstVector secondVector ==
        False
-}
equalWithin : Float -> Vector2d -> Vector2d -> Bool
equalWithin tolerance firstVector secondVector =
    squaredLength (minus firstVector secondVector) <= tolerance * tolerance


{-| Get the length of a vector.

    Vector2d.length (Vector2d ( 3, 4 )) ==
        5
-}
length : Vector2d -> Float
length =
    squaredLength >> sqrt


{-| Get the squared length of a vector. `squaredLength` is slightly faster than
`length`, so for example

    Vector2d.squaredLength vector > tolerance * tolerance

is equivalent to but slightly more efficient than

    Vector2d.length vector > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `length` is much more
readable!
-}
squaredLength : Vector2d -> Float
squaredLength (Vector2d ( x, y )) =
    x * x + y * y


{-| Attempt to find the direction of a vector. In the case of a zero vector,
return `Nothing`.

    Vector2d.direction (Vector2d ( 3, 4 )) ==
        Just (Direction2d ( 0.6, 0.8 ))

    Vector2d.direction (Vector2d ( 0, 0 )) ==
        Nothing
-}
direction : Vector2d -> Maybe Direction2d
direction vector =
    if vector == zero then
        Nothing
    else
        let
            normalizedVector =
                times (1 / length vector) vector
        in
            Just (Direction2d (components normalizedVector))


{-| Attempt to find the length and direction of a vector. In the case of a zero
vector, returns `Nothing`.

    vector =
        Vector2d ( 3, 4 )

    Vector2d.lengthAndDirection vector ==
        Just ( 5, Direction2d ( 0.6, 0.8 ) )

    Vector2d.lengthAndDirection Vector2d.zero ==
        Nothing
-}
lengthAndDirection : Vector2d -> Maybe ( Float, Direction2d )
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
                    Direction2d (components normalizedVector)
            in
                Just ( vectorLength, vectorDirection )


{-| Negate a vector.

    Vector2d.negate (Vector2d ( -1, 2 )) ==
        Vector2d ( 1, -2 )
-}
negate : Vector2d -> Vector2d
negate (Vector2d ( x, y )) =
    Vector2d ( -x, -y )


{-| Multiply a vector by a scalar.

    Vector2d.times 3 (Vector2d ( 1, 2 )) ==
        Vector2d ( 3, 6 )
-}
times : Float -> Vector2d -> Vector2d
times scale (Vector2d ( x, y )) =
    Vector2d ( x * scale, y * scale )


{-| Add one vector to another.

    firstVector =
        Vector2d ( 1, 2 )

    secondVector =
        Vector2d ( 3, 4 )

    Vector2d.plus firstVector secondVector ==
        Vector2d ( 4, 6 )
-}
plus : Vector2d -> Vector2d -> Vector2d
plus (Vector2d ( x2, y2 )) (Vector2d ( x1, y1 )) =
    Vector2d ( x1 + x2, y1 + y2 )


{-| Subtract one vector from another. The vector to subtract is given first and
the vector to be subtracted from is given second, so

    firstVector =
        Vector2d ( 1, 2 )

    secondVector =
        Vector2d ( 5, 6 )

    Vector2d.minus firstVector secondVector ==
        Vector2d ( 4, 4 )

or more generally, `Vector2d.minus a b` means `b - a`, not `a - b`. Think of
`Vector2d.minus a b` as the operation `minus a` being applied to the vector `b`.
-}
minus : Vector2d -> Vector2d -> Vector2d
minus (Vector2d ( x2, y2 )) (Vector2d ( x1, y1 )) =
    Vector2d ( x1 - x2, y1 - y2 )


{-| Find the dot product of two vectors.

    firstVector =
        Vector2d ( 1, 2 )

    secondVector =
        Vector2d ( 3, 4 )

    Vector2d.dotProduct firstVector secondVector ==
        11
-}
dotProduct : Vector2d -> Vector2d -> Float
dotProduct (Vector2d ( x1, y1 )) (Vector2d ( x2, y2 )) =
    x1 * x2 + y1 * y2


{-| Find the scalar 'cross product' of two vectors in 2D. This is defined as

    crossProduct (Vector2d ( x1, y1 )) (Vector2d ( x2, y2 )) =
        x1 * y2 - y1 * x2

and is useful in many of the same ways as the 3D cross product:

  - Its magnitude is equal to the product of the lengths of the two given
    vectors and the sine of the angle between them, so it can be used as a
    metric to determine if two vectors are nearly parallel.
  - The sign of the result indicates the direction of rotation from the first
    vector to the second (positive indicates a counterclockwise rotation and
    negative indicates a clockwise rotation), similar to how the direction of
    the 3D cross product indicates the direction of rotation.

Some examples:

    firstVector =
        Vector2d ( 2, 0 )

    secondVector =
        Vector2d ( 0, 3 )

    Vector2d.crossProduct firstVector secondVector ==
        6

    Vector2d.crossProduct secondVector firstVector ==
        -6

    Vector2d.crossProduct firstVector firstVector ==
        0
-}
crossProduct : Vector2d -> Vector2d -> Float
crossProduct (Vector2d ( x1, y1 )) (Vector2d ( x2, y2 )) =
    x1 * y2 - y1 * x2


{-| Rotate a vector counterclockwise by a given angle (in radians).

    Vector2d.rotateBy (degrees 45) (Vector2d ( 1, 1 )) ==
        Vector2d ( 0, 1.4142 )

    Vector2d.rotateBy pi (Vector2d ( 1, 0 )) ==
        Vector2d ( -1, 0 )
-}
rotateBy : Float -> Vector2d -> Vector2d
rotateBy angle =
    let
        cosine =
            cos angle

        sine =
            sin angle
    in
        \(Vector2d ( x, y )) ->
            Vector2d ( x * cosine - y * sine, y * cosine + x * sine )


{-| Mirror a vector across a particular axis.

    vector =
        Vector2d ( 2, 3 )

    Vector2d.mirrorAcross Axis2d.y vector ==
        Vector2d ( -2, 3 )

    horizontalAxis =
        Axis2d
            { originPoint = Point2d ( 100, 200 )
            , direction = Direction2d.x
            }

    Vector2d.mirrorAcross horizontalAxis vector ==
        Vector2d ( 2, -3 )
-}
mirrorAcross : Axis2d -> Vector2d -> Vector2d
mirrorAcross axis =
    let
        (Axis2d { originPoint, direction }) =
            axis

        (Direction2d ( dx, dy )) =
            direction

        a =
            1 - 2 * dy * dy

        b =
            2 * dx * dy

        c =
            1 - 2 * dx * dx
    in
        \(Vector2d ( vx, vy )) -> Vector2d ( a * vx + b * vy, c * vy + b * vx )


{-| Find the projection of a vector in a particular direction. Conceptually,
this means splitting the original vector into a portion parallel to the given
direction and a portion perpendicular to it, then returning the parallel
portion.

    vector =
        Vector2d ( 2, 3 )

    Vector2d.projectionIn Direction2d.x vector ==
        Vector2d ( 2, 0 )

    Vector2d.projectionIn Direction2d.y vector ==
        Vector2d ( 0, 3 )
-}
projectionIn : Direction2d -> Vector2d -> Vector2d
projectionIn direction vector =
    let
        (Direction2d directionComponents) =
            direction

        directionVector =
            Vector2d directionComponents
    in
        times (dotProduct vector directionVector) directionVector


{-| Project a vector onto an axis.

    Vector2d.projectOnto Axis2d.y (Vector2d ( 3, 4 )) ==
        Vector2d ( 0, 4 )

    Vector2d.projectOnto Axis2d.x (Vector2d ( -1, 2 )) ==
        Vector2d ( -1, 0 )

This is equivalent to finding the projection in the axis' direction.
-}
projectOnto : Axis2d -> Vector2d -> Vector2d
projectOnto axis =
    let
        (Axis2d { originPoint, direction }) =
            axis
    in
        projectionIn direction


{-| Take a vector defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Vector2d.relativeTo upsideDownFrame (Vector2d ( 2, 3 )) ==
        Vector2d ( 2, -3 )

    Vector2d.relativeTo rotatedFrame (Vector2d ( 2, 0 )) ==
        Vector2d ( 1.732, -1 )
-}
relativeTo : Frame2d -> Vector2d -> Vector2d
relativeTo frame vector =
    let
        (Frame2d { originPoint, xDirection, yDirection }) =
            frame
    in
        Vector2d
            ( componentIn xDirection vector
            , componentIn yDirection vector
            )


{-| Take a vector defined in local coordinates relative to a given reference
frame, and return that vector expressed in global coordinates.

    Vector2d.placeIn upsideDownFrame (Vector2d ( 2, 3 )) ==
        Vector2d ( 2, -3 )

    Vector2d.placeIn rotatedFrame (Vector2d ( 2, 0 )) ==
        Vector2d ( 1.732, 1 )
-}
placeIn : Frame2d -> Vector2d -> Vector2d
placeIn frame =
    let
        (Frame2d { originPoint, xDirection, yDirection }) =
            frame

        (Direction2d ( x1, y1 )) =
            xDirection

        (Direction2d ( x2, y2 )) =
            yDirection
    in
        \(Vector2d ( x, y )) -> Vector2d ( x1 * x + x2 * y, y1 * x + y2 * y )


{-| Take a vector defined in 2D coordinates within a particular sketch plane and
return the corresponding vector in 3D.

    vector =
        Vector2d ( 2, 3 )

    Vector2d.placeOnto SketchPlane3d.xy vector ==
        Vector3d ( 2, 3, 0 )

    Vector2d.placeOnto SketchPlane3d.yz vector ==
        Vector3d ( 0, 2, 3 )

    Vector2d.placeOnto SketchPlane3d.zx vector ==
        Vector3d ( 3, 0, 2 )

A slightly more complex example:

    tiltedSketchPlane =
        SketchPlane3d.xy
            |> SketchPlane3d.rotateAround Axis3d.x (degrees 45)

    Vector2d.placeOnto tiltedSketchPlane (Vector2d ( 1, 1 )) ==
        Vector3d ( 1, 0.7071, 0.7071 )
-}
placeOnto : SketchPlane3d -> Vector2d -> Vector3d
placeOnto sketchPlane =
    let
        (SketchPlane3d { originPoint, xDirection, yDirection }) =
            sketchPlane

        (Direction3d ( ux, uy, uz )) =
            xDirection

        (Direction3d ( vx, vy, vz )) =
            yDirection
    in
        \(Vector2d ( x, y )) ->
            Vector3d
                ( x * ux + y * vx
                , x * uy + y * vy
                , x * uz + y * vz
                )
