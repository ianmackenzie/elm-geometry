{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Vector2d
    exposing
        ( zero
        , relativeTo
        , fromComponents
        , fromPolarComponents
        , xComponent
        , yComponent
        , components
        , polarComponents
        , componentIn
        , squaredLength
        , length
        , perpendicularVector
        , rotateBy
        , mirrorAbout
        , toLocalIn
        , toGlobalFrom
        , projectionIn
        , projectOnto
        , placeOnto
        , negate
        , sum
        , difference
        , plus
        , minus
        , times
        , dotProduct
        , crossProduct
        , toRecord
        , fromRecord
        )

{-| Functions for working with `Vector2d` values. Vectors can be constructed
directly from their X and Y components if desired:

    v1 = Vector2d 2 3
    v2 = Vector2d (4 + 5) (sqrt 2)

The functions in this module provide various other ways of constructing vectors
and performing operations on them. For the examples below, assume the following
imports:

    import OpenSolid.Core.Types exposing (..)
    import OpenSolid.Vector2d as Vector2d
    import OpenSolid.Direction2d as Direction2d
    import OpenSolid.Point2d as Point2d
    import OpenSolid.Frame2d as Frame2d

# Constants

Although there are no predefined constants for `Vector2d 1 0` and
`Vector2d 0 1`, in most cases you will actually want their `Direction2d`
equivalents, which are available as `Direction2d.x` and `Direction2d.y`.

@docs zero

# Constructors

@docs fromComponents, fromPolarComponents

# Components

Although `xComponent` and `yComponent` are provided for convenience, in many
cases it is  better to use `componentIn`. For instance, instead of using
`Vector2d.yComponent someVector`, you could define a constant such as

    verticalDirection : Direction2d
    verticalDirection =
        Direction2d.y

and then use `Vector2d.componentIn verticalDirection someVector`. This is more
flexible and self-documenting, although not quite as efficient (since behind the
scenes it requires a dot product instead of a simple component access).

@docs xComponent, yComponent, componentIn, components, polarComponents

# Arithmetic

@docs plus, minus, times, dotProduct, crossProduct

# Length and direction

@docs squaredLength, length, perpendicularVector

# Transformations

@docs rotateBy, mirrorAbout, projectionIn, projectOnto, placeOnto

# Coordinate conversions

For the examples for these functions, assuming the following definition of a
local coordinate frame, one that is rotated 45 degrees counterclockwise from the
global XY frame:

    frame = Frame2d.rotateAround (Point2d.origin) (degrees 45) Frame2d.xy
    frame.xDirection == Direction2d (Vector2d 0.7071 0.7071)
    frame.yDirection == Direction2d (Vector2d -0.7071 0.7071)

@docs toLocalIn, toGlobalFrom

# Record conversions

@docs toRecord, fromRecord
-}

import OpenSolid.Core.Types exposing (..)


{-| The zero vector.

    Vector2d.zero == Vector2d 0 0
-}
zero : Vector2d
zero =
    Vector2d 0 0


relativeTo : Frame2d -> Float -> Float -> Vector2d
relativeTo frame =
    let
        (Direction2d (Vector2d x1 y1)) =
            frame.xDirection

        (Direction2d (Vector2d x2 y2)) =
            frame.yDirection
    in
        \x y -> Vector2d (x * x1 + y * x2) (x * y1 + y * y2)


{-| Construct a vector from a pair of X and Y components.

    Vector2d.fromComponents ( 2, 3 ) == Vector2d 2 3
-}
fromComponents : ( Float, Float ) -> Vector2d
fromComponents ( x, y ) =
    Vector2d x y


{-| Construct a vector from (radius, angle) components. Angles must be given in
radians (Elm's built-in `degrees` and `turns` functions may be useful).

    Vector2d.fromPolarComponents ( 1, degrees 45 ) == Vector2d 0.7071 0.7071
-}
fromPolarComponents : ( Float, Float ) -> Vector2d
fromPolarComponents =
    fromPolar >> fromComponents


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


{-| Get the X and Y components of a vector as a tuple.
-}
components : Vector2d -> ( Float, Float )
components (Vector2d x y) =
    ( x, y )


{-| Convert a vector to polar (radius, angle) components. Angles will be
returned in radians.

    Vector2d.polarComponents (Vector2d 1 1) == ( sqrt 2, pi / 4 )
-}
polarComponents : Vector2d -> ( Float, Float )
polarComponents =
    components >> toPolar


{-| Get the component of a vector in a particular direction. For example,
`Vector2d.componentIn Direction2d.x` is equivalent to `Vector2d.xComponent`.

The result is signed value; for a vector version, see `projectionIn`.
-}
componentIn : Direction2d -> Vector2d -> Float
componentIn (Direction2d vector) =
    dotProduct vector


{-| Get the squared length of a vector. This is slightly more efficient than
calling `Vector2d.length`.
-}
squaredLength : Vector2d -> Float
squaredLength (Vector2d x y) =
    x * x + y * y


{-| Get the length of a vector. Using `Vector2d.squaredLength` is slightly
more efficient, so for instance

    Vector2d.squaredLength vector > tolerance * tolerance

is equivalent to but slightly faster than

    Vector2d.length vector > tolerance

In many cases, however, the speed difference will be negligible and using
`Vector2d.length` is much more readable!
-}
length : Vector2d -> Float
length =
    squaredLength >> sqrt


{-| Construct a vector perpendicular to the given vector but with the same
length, by rotating the given vector 90 degrees in a counterclockwise direction.

    Vector2d.perpendicularVector (Vector2d 3 1) == Vector2d -1 3
-}
perpendicularVector : Vector2d -> Vector2d
perpendicularVector (Vector2d x y) =
    Vector2d (-y) x


{-| Rotate a vector counterclockwise by a given angle (in radians).

    Vector2d.rotateBy (degrees 45) (Vector2d 1 1) == Vector2d 0 1.4142
    Vector2d.rotateBy pi (Vector2d 1 0) == Vector2d -1 0

Rotating a list of vectors by 90 degrees:

    vectors = [ v1, v2, v3 ]
    angle = degrees 90
    rotatedVectors = List.map (Vector2d.rotateBy angle) vectors
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


{-| Mirror a vector about a particular direction. This can be thought of as
mirroring the vector across an axis that has the given direction.

    Vector2d.mirrorAbout Direction2d.x (Vector2d 2 3) == Vector2d 2 -3
    Vector2d.mirrorAbout Direction2d.y (Vector2d 2 3) == Vector2d -2 3
-}
mirrorAbout : Direction2d -> Vector2d -> Vector2d
mirrorAbout direction =
    let
        (Direction2d (Vector2d dx dy)) =
            direction

        a =
            1 - 2 * dy * dy

        b =
            2 * dx * dy

        c =
            1 - 2 * dx * dx
    in
        \(Vector2d vx vy) -> Vector2d (a * vx + b * vy) (c * vy + b * vx)


{-| Convert a vector from global coordinates to local coordinates within a given
frame. The result will be the given vector expressed relative to the given
frame.

The vector `Vector2d 1 0` (in global coordinates), relative to the rotated
frame, is a vector of length 1 with an angle of -45 degrees. Therefore,

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
frame) is a vector of length 1 along the frame's X axis. Therefore,

    Vector2d.toGlobalFrom frame (Vector2d 1 0) == Vector2d 0.7071 0.7071

The vector `Vector2d 1 1` in local coordinates, on the other hand, is at a 45
degree angle from the X axis of the rotated frame and so is straight up in
global coordinates:

    Vector2d.toGlobalFrom frame (Vector2d 1 1) == Vector2d 0 1.4142
-}
toGlobalFrom : Frame2d -> Vector2d -> Vector2d
toGlobalFrom frame =
    let
        (Direction2d (Vector2d x1 y1)) =
            frame.xDirection

        (Direction2d (Vector2d x2 y2)) =
            frame.yDirection
    in
        \(Vector2d x y) -> Vector2d (x1 * x + x2 * y) (y1 * x + y2 * y)


{-| Find the portion of a vector parallel to a particular direction. This can be
thought of as projecting the vector onto an axis that has the given direction.

See also `componentIn`.
`Vector2d.length (Vector2d.projectionIn direction vector)` is equivalent to
`abs (Vector2d.componentIn direction vector)`
-}
projectionIn : Direction2d -> Vector2d -> Vector2d
projectionIn ((Direction2d directionVector) as direction) vector =
    times (componentIn direction vector) directionVector


projectOnto : Axis2d -> Vector2d -> Vector2d
projectOnto axis =
    projectionIn axis.direction


placeOnto : Plane3d -> Vector2d -> Vector3d
placeOnto plane =
    let
        (Direction3d (Vector3d x1 y1 z1)) =
            plane.xDirection

        (Direction3d (Vector3d x2 y2 z2)) =
            plane.yDirection
    in
        \(Vector2d x y) ->
            Vector3d (x1 * x + x2 * y) (y1 * x + y2 * y) (z1 * x + z2 * y)


negate : Vector2d -> Vector2d
negate (Vector2d x y) =
    Vector2d (-x) (-y)


sum : Vector2d -> Vector2d -> Vector2d
sum (Vector2d x1 y1) (Vector2d x2 y2) =
    Vector2d (x1 + x2) (y1 + y2)


difference : Vector2d -> Vector2d -> Vector2d
difference (Vector2d x1 y1) (Vector2d x2 y2) =
    Vector2d (x1 - x2) (y1 - y2)


plus : Vector2d -> Vector2d -> Vector2d
plus (Vector2d x2 y2) (Vector2d x1 y1) =
    Vector2d (x1 + x2) (y1 + y2)


minus : Vector2d -> Vector2d -> Vector2d
minus (Vector2d x2 y2) (Vector2d x1 y1) =
    Vector2d (x1 - x2) (y1 - y2)


times : Float -> Vector2d -> Vector2d
times scale (Vector2d x y) =
    Vector2d (x * scale) (y * scale)


dotProduct : Vector2d -> Vector2d -> Float
dotProduct (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * x2 + y1 * y2


crossProduct : Vector2d -> Vector2d -> Float
crossProduct (Vector2d x1 y1) (Vector2d x2 y2) =
    x1 * y2 - y1 * x2


toRecord : Vector2d -> { x : Float, y : Float }
toRecord (Vector2d x y) =
    { x = x, y = y }


fromRecord : { x : Float, y : Float } -> Vector2d
fromRecord { x, y } =
    Vector2d x y
