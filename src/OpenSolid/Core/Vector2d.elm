{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Vector2d
    exposing
        ( zero
        , polar
        , along
        , components
        , fromComponents
        , polarComponents
        , fromPolarComponents
        , toRecord
        , fromRecord
        , xComponent
        , yComponent
        , componentIn
        , length
        , squaredLength
        , negate
        , plus
        , minus
        , times
        , dotProduct
        , crossProduct
        , rotateBy
        , mirrorAcross
        , projectOnto
        , perpendicularTo
        , toLocalIn
        , fromLocalIn
        )

{-| The functions in this module provide various ways of constructing `Vector2d`
values and performing operations on them. For the examples below, assume the
following imports:

    import OpenSolid.Core.Types exposing (..)
    import OpenSolid.Core.Vector2d as Vector2d
    import OpenSolid.Core.Direction2d as Direction2d
    import OpenSolid.Core.Point2d as Point2d
    import OpenSolid.Core.Frame2d as Frame2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Constants

@docs zero

Although there are no predefined constants for `Vector2d 1 0` and
`Vector2d 0 1`, in most cases you will actually want their `Direction2d`
versions `Direction2d.x` and `Direction2d.y`.

# Constructors

Since `Vector2d` is not an opaque type, the simplest way to construct one is
directly from its X and Y components, for example `Vector2d 2 3`. But that is
not the only way!

@docs polar, along

# Conversions

Various ways to convert to and from plain tuples and records. Primarily useful
for interoperability with other libraries. For example, you could define
conversion functions to and from `elm-linear-algebra`'s `Vec2` type with

    toVec2 : Vector2d -> Math.Vector2.Vec2
    toVec2 =
        Vector2d.components >> Math.Vector2.fromTuple

    fromVec2 : Math.Vector2.Vec2 -> Vector2d
    fromVec2 =
        Math.Vector2.toTuple >> Vector2d.fromComponents

@docs components, fromComponents, polarComponents, fromPolarComponents, toRecord, fromRecord

# Components

@docs xComponent, yComponent, componentIn

# Length

@docs length, squaredLength

# Arithmetic

@docs negate, plus, minus, times, dotProduct, crossProduct

# Transformations

@docs rotateBy, mirrorAcross, projectOnto, perpendicularTo

# Local coordinates

Functions for transforming vectors between local and global coordinates in
different coordinate frames. For the examples below, assume the following
definition of a local coordinate frame, one that is rotated 45 degrees
counterclockwise from the global XY frame:

    frame = Frame2d.rotateBy (degrees 45) Frame2d.xy
    frame.xDirection == Direction2d (Vector2d 0.7071 0.7071)
    frame.yDirection == Direction2d (Vector2d -0.7071 0.7071)

@docs toLocalIn, fromLocalIn
-}

import OpenSolid.Core.Types exposing (..)


{-| The zero vector.

    Vector2d.zero == Vector2d 0 0
-}
zero : Vector2d
zero =
    Vector2d 0 0


{-| Construct a vector with the given radius and angle. Angles must be given in
radians (Elm's built-in `degrees` and `turns` functions may be useful).

    Vector2d.polar 1 (degrees 45) == Vector2d 0.7071 0.7071
-}
polar : Float -> Float -> Vector2d
polar radius angle =
    fromPolarComponents ( radius, angle )


{-| Construct a vector parallel to the given axis, with the given magnitude.

    Vector2d.along Axis2d.x 5 == Vector2d 5 0
    Vector2d.along Axis2d.y -3 == Vector2d 0 -3
-}
along : Axis2d -> Float -> Vector2d
along axis magnitude =
    let
        (Direction2d directionVector) =
            axis.direction
    in
        times magnitude directionVector


{-| Get the (x, y) components of a vector.

    Vector2d.components (Vector2d x y) == ( x, y )

Note that you could use this to extract the X and Y components of a vector using
tuple pattern matching, for example

    ( x, y ) = Vector2d.components vector

but it's simpler and more efficient (although perhaps slightly cryptic) to use
pattern matching on the vector directly:

    (Vector2d x y) = vector
-}
components : Vector2d -> ( Float, Float )
components (Vector2d x y) =
    ( x, y )


{-| Construct a vector from (x, y) components.

    Vector2d.fromComponents ( x, y ) == Vector2d x y
-}
fromComponents : ( Float, Float ) -> Vector2d
fromComponents ( x, y ) =
    Vector2d x y


{-| Get the polar (radius, angle) components of a vector. Angles will be
returned in radians.

    Vector2d.polarComponents (Vector2d 1 1) == ( sqrt 2, degrees 45 )
-}
polarComponents : Vector2d -> ( Float, Float )
polarComponents =
    components >> toPolar


{-| Construct a vector from polar (radius, angle) components.

    Vector2d.fromPolarComponents ( radius, angle ) == Vector2d.polar radius angle
-}
fromPolarComponents : ( Float, Float ) -> Vector2d
fromPolarComponents =
    fromPolar >> fromComponents


{-| Convert a vector to a record with `x` and `y` fields.

    Vector2d.toRecord (Vector2d 2 3) == { x = 2, y = 3 }
-}
toRecord : Vector2d -> { x : Float, y : Float }
toRecord (Vector2d x y) =
    { x = x, y = y }


{-| Construct a vector from a record with `x` and `y` fields.

    Vector2d.fromRecord { x = 2, y = 3 } == Vector2d 2 3
-}
fromRecord : { x : Float, y : Float } -> Vector2d
fromRecord { x, y } =
    Vector2d x y


{-| Get the X component of a vector.
-}
xComponent : Vector2d -> Float
xComponent (Vector2d x _) =
    x


{-| Get the Y component of a vector.
-}
yComponent : Vector2d -> Float
yComponent (Vector2d _ y) =
    y


{-| Although `xComponent` and `yComponent` are provided for convenience, in many
cases it is  better to use `componentIn`, which finds the component of a vector
in an arbitrary direction (equivalent to taking a dot product of the vector and
a unit vector in the given direction). For example, instead of using

    Vector2d.yComponent vector

you could define a constant such as

    verticalDirection =
        Direction2d.y

and then use

    Vector2d.componentIn verticalDirection vector

or even define a convenience function

    verticalComponent : Vector2d -> Float
    verticalComponent =
        Vector2d.componentIn verticalDirection

which you could then use as

    verticalComponent vector

This sort of approach is more flexible, although not quite as efficient (since
behind the scenes it requires a dot product instead of a simple component
access).
-}
componentIn : Direction2d -> Vector2d -> Float
componentIn (Direction2d vector) =
    dotProduct vector


{-| Get the length of a vector.
-}
length : Vector2d -> Float
length =
    squaredLength >> sqrt


{-| Get the squared length of a vector. `squaredLength` is slightly faster than
`length`, so for example

    Vector2d.squaredLength vector > tolerance * tolerance

is equivalent to but slightly more efficient than

    Vector2d.length vector > tolerance

since the latter requires a square root. In many cases, however, the speed
difference will be negligible and using `length` is much more readable!
-}
squaredLength : Vector2d -> Float
squaredLength (Vector2d x y) =
    x * x + y * y


{-| Negate a vector.

    Vector2d.negate (Vector2d -1 2) == Vector2d 1 -2
-}
negate : Vector2d -> Vector2d
negate (Vector2d x y) =
    Vector2d (-x) (-y)


{-| Add one vector to another.

    Vector2d.plus (Vector2d 1 2) (Vector2d 3 4) == Vector2d 4 6
-}
plus : Vector2d -> Vector2d -> Vector2d
plus (Vector2d x2 y2) (Vector2d x1 y1) =
    Vector2d (x1 + x2) (y1 + y2)


{-| Subtract one vector from another. The vector to subtract is given first and
the vector to be subtracted from is given second, so

    Vector2d.minus (Vector2d 1 2) (Vector2d 5 6) == Vector2d 4 4

This means that `minus` can be used more naturally in situations like `map`
functions

    minusVector =
        Vector2d.minus (Vector2d 2 0)

    originalVectors =
        [ Vector2d 1 2, Vector2d 3 4 ]

    List.map minusVector originalVectors == [ Vector2d -1 2, Vector2d 1 4 ]

or function pipelining

    myFunction =
        Vector2d.minus (Vector2d 0 1) >> Vector2d.times 3

    myFunction (Vector2d 2 1) == Vector2d 6 0

where `myFunction` could be described in pseudo-English as '`minus` by
`Vector2d 0 1` and then `times` by 3'.
-}
minus : Vector2d -> Vector2d -> Vector2d
minus (Vector2d x2 y2) (Vector2d x1 y1) =
    Vector2d (x1 - x2) (y1 - y2)


{-| Multiply a vector by a scalar.

    Vector2d.times 3 (Vector2d 1 2) == Vector2d 3 6
-}
times : Float -> Vector2d -> Vector2d
times scale (Vector2d x y) =
    Vector2d (x * scale) (y * scale)


{-| Find the dot product of two vectors.

    Vector2d.dotProduct (Vector2d 1 2) (Vector2d 3 4) == 1 * 3 + 2 * 4 == 11
-}
dotProduct : Vector2d -> Vector2d -> Float
dotProduct (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * x2 + y1 * y2


{-| Find the scalar 'cross product' of two vectors in 2D. This is defined as

    crossProduct (Vector2d x1 y1) (Vector2d x2 y2) =
        x1 * y2 - y1 * x2

and is useful in many of the same ways as the 3D cross product. Its magnitude is
equal to the product of the lengths of the two given vectors and the sine of the
angle between them, so it can be used as a metric to determine if two vectors
are nearly parallel. The sign of the result indicates the direction of rotation
from the first vector to the second (positive indicates a counterclockwise
rotation and negative indicates a clockwise rotation), similar to how the
direction of the 3D cross product indicates the direction of rotation.
-}
crossProduct : Vector2d -> Vector2d -> Float
crossProduct (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * y2 - y1 * x2


{-| Rotate a vector counterclockwise by a given angle (in radians).

    Vector2d.rotateBy (degrees 45) (Vector2d 1 1) == Vector2d 0 1.4142
    Vector2d.rotateBy pi (Vector2d 1 0) == Vector2d -1 0

Rotating a list of vectors by 90 degrees:

    vectors =
        [ v1, v2, v3 ]

    angle =
        degrees 90

    rotatedVectors =
        List.map (Vector2d.rotateBy angle) vectors
-}
rotateBy : Float -> Vector2d -> Vector2d
rotateBy angle =
    let
        cosine =
            cos angle

        sine =
            sin angle
    in
        \(Vector2d x y) ->
            Vector2d (x * cosine - y * sine) (y * cosine + x * sine)


{-| Mirror a vector across a particular axis. Note that only the direction of
the axis affects the result.

    Vector2d.mirrorAcross Axis2d.x (Vector2d 2 3) == Vector2d 2 -3
    Vector2d.mirrorAcross Axis2d.y (Vector2d 2 3) == Vector2d -2 3
-}
mirrorAcross : Axis2d -> Vector2d -> Vector2d
mirrorAcross axis =
    let
        (Direction2d (Vector2d dx dy)) =
            axis.direction

        a =
            1 - 2 * dy * dy

        b =
            2 * dx * dy

        c =
            1 - 2 * dx * dx
    in
        \(Vector2d vx vy) -> Vector2d (a * vx + b * vy) (c * vy + b * vx)


{-| Project a vector onto an axis. This will result in a vector parallel to the
given axis.

    Vector2d.projectOnto Axis2d.y (Vector2d 3 4) == Vector2d 0 4
    Vector2d.projectOnto Axis2d.x (Vector2d -1 2) == Vector2d -1 0
-}
projectOnto : Axis2d -> Vector2d -> Vector2d
projectOnto axis vector =
    let
        (Direction2d directionVector) =
            axis.direction
    in
        times (dotProduct vector directionVector) directionVector


{-| Construct a vector perpendicular to the given vector, by rotating the given
vector 90 degrees in a counterclockwise direction. The perpendicular vector will
have the same length as the given vector.

    Vector2d.perpendicularTo (Vector2d 3 1) == Vector2d -1 3
-}
perpendicularTo : Vector2d -> Vector2d
perpendicularTo (Vector2d x y) =
    Vector2d (-y) x


{-| Convert a vector from global coordinates to local coordinates within a given
frame. The result will be the given vector expressed relative to the given
frame.

The vector `Vector2d 1 0` (in global coordinates), relative to the rotated
frame defined above, is a vector of length 1 with an angle of -45 degrees.
Therefore,

    Vector2d.toLocalIn frame (Vector2d 1 0) == Vector2d 0.7071 -0.7071

The vector `Vector2d 1 1`, on the other hand, is parallel to the X axis of the
rotated frame so only has an X component when expressed in local coordinates
relative to that frame:

    Vector2d.toLocalIn frame (Vector2d 1 1) == Vector2d 1.4142 0
-}
toLocalIn : Frame2d -> Vector2d -> Vector2d
toLocalIn frame vector =
    Vector2d (componentIn frame.xDirection vector)
        (componentIn frame.yDirection vector)


{-| Convert a vector from local coordinates within a given frame to global
coordinates.

The vector `Vector2d 1 0` (in local coordinates with respect to the rotated
frame defined above) is a vector of length 1 along the frame's X axis, which is
itself at a 45 degree angle in global coordinates. Therefore,

    Vector2d.fromLocalIn frame (Vector2d 1 0) == Vector2d 0.7071 0.7071

The vector `Vector2d 1 1` in local coordinates, on the other hand, is at a 45
degree angle from the X axis of the rotated frame and so is straight up in
global coordinates:

    Vector2d.fromLocalIn frame (Vector2d 1 1) == Vector2d 0 1.4142
-}
fromLocalIn : Frame2d -> Vector2d -> Vector2d
fromLocalIn frame =
    let
        (Direction2d (Vector2d x1 y1)) =
            frame.xDirection

        (Direction2d (Vector2d x2 y2)) =
            frame.yDirection
    in
        \(Vector2d x y) -> Vector2d (x1 * x + x2 * y) (y1 * x + y2 * y)
