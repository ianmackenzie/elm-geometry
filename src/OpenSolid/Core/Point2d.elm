{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Point2d
    exposing
        ( origin
        , polar
        , alongAxis
        , midpoint
        , interpolate
        , xCoordinate
        , yCoordinate
        , vectorFrom
        , vectorTo
        , distanceFrom
        , squaredDistanceFrom
        , distanceAlong
        , distanceFromAxis
        , plus
        , minus
        , scaleAbout
        , rotateAround
        , translateAlong
        , mirrorAcross
        , projectOnto
        , toLocalIn
        , fromLocalIn
        , coordinates
        , fromCoordinates
        , polarCoordinates
        , fromPolarCoordinates
        , toRecord
        , fromRecord
        )

{-| Various functions for constructing `Point2d` values and performing
operations on them. For the examples below, assume the following imports:

    import OpenSolid.Core.Types exposing (..)
    import OpenSolid.Core.Point2d as Point2d
    import OpenSolid.Core.Frame2d as Frame2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Constants

@docs origin

# Constructors

Since `Point2d` is not an opaque type, the simplest way to construct one is
directly from its X and Y coordinates, for example `Point2d 2 3`. But that is
not the only way!

@docs polar, alongAxis, midpoint, interpolate

# Coordinates

@docs xCoordinate, yCoordinate

# Displacement and distance

@docs vectorFrom, vectorTo, distanceFrom, squaredDistanceFrom, distanceAlong, distanceFromAxis

# Arithmetic

@docs plus, minus

# Transformations

@docs scaleAbout, rotateAround, translateAlong, mirrorAcross, projectOnto

# Local coordinates

Functions for transforming points between local and global coordinates in
different coordinate frames. Although these examples use a simple offset
frame, these functions can be used to convert to and from local coordinates in
arbitrarily transformed (translated, rotated, mirrored) frames.

@docs toLocalIn, fromLocalIn

# Conversions

Various ways to convert to and from plain tuples and records. Primarily useful
for interoperability with other libraries. For example, you could define
conversion functions to and from `elm-linear-algebra`'s `Vec2` type with

    toVec2 : Point2d -> Math.Vector2.Vec2
    toVec2 =
        Point2d.coordinates >> Math.Vector2.fromTuple

    fromVec2 : Math.Vector2.Vec2 -> Point2d
    fromVec2 =
        Math.Vector2.toTuple >> Point2d.fromCoordinates

@docs coordinates, fromCoordinates, polarCoordinates, fromPolarCoordinates, toRecord, fromRecord
-}

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d


{-| The point (0, 0).
-}
origin : Point2d
origin =
    Point2d 0 0


{-| Construct a point given its radius from the origin and its angle
counterclockwise from the positive X axis. Angles must be given in radians
(Elm's built-in `degrees` and `turns` functions may be useful).

    Point2d.polar 1 (degrees 45) == Point2d 0.7071 0.7071
-}
polar : Float -> Float -> Point2d
polar radius angle =
    fromPolarCoordinates ( radius, angle )


{-| Construct a point along an axis, at a particular distance from the axis'
origin point.

    horizontalAxis =
        Axis2d (Point2d 1 1) Direction2d.x

    Point2d.alongAxis horizontalAxis 3 == Point2d 4 1
    Point2d.alongAxis horizontalAxis -3 == Point2d -2 1
-}
alongAxis : Axis2d -> Float -> Point2d
alongAxis axis distance =
    plus (Vector2d.alongAxis axis distance) axis.originPoint


{-| Construct a point halfway between two other points.

    Point2d.midpoint (Point2d 1 1) (Point2d 3 7) == Point2d 2 4
-}
midpoint : Point2d -> Point2d -> Point2d
midpoint firstPoint secondPoint =
    interpolate firstPoint secondPoint 0.5


{-| Construct a point by interpolating between two other points based on a
parameter that ranges from zero to one.

    interpolate : Float -> Point2d
    interpolate parameter =
        Point2d.interpolate Point2d.origin (Point2d 8 8) parameter

    interpolate 0 == Point2d 0 0
    interpolate 0.25 == Point2d 2 2
    interpolate 0.75 == Point2d 6 6
    interpolate 1 == Point2d 8 8

You can also pass values less than zero or greater than one to extrapolate:

    interpolate 1.25 == Point2d 10 10
-}
interpolate : Point2d -> Point2d -> Float -> Point2d
interpolate startPoint endPoint =
    let
        displacement =
            vectorFrom startPoint endPoint
    in
        \t -> plus (Vector2d.times t displacement) startPoint


{-| Get the X coordinate of a point.

    Point2d.xCoordinate (Point2d 2 3) == 2
-}
xCoordinate : Point2d -> Float
xCoordinate (Point2d x _) =
    x


{-| Get the Y coordinate of a point.

    Point2d.yCoordinate (Point2d 2 3) == 3
-}
yCoordinate : Point2d -> Float
yCoordinate (Point2d _ y) =
    y


{-| Find the vector from one point to another.

    Point2d.vectorFrom (Point2d 1 1) (Point2d 4 5) == Vector2d 3 4
-}
vectorFrom : Point2d -> Point2d -> Vector2d
vectorFrom (Point2d x2 y2) (Point2d x1 y1) =
    Vector2d (x1 - x2) (y1 - y2)


{-| Flipped version of `vectorFrom`, where the end point is given first.

    Point2d.vectorTo (Point2d 1 1) (Point2d 4 5) == Vector2d -3 -4
-}
vectorTo : Point2d -> Point2d -> Vector2d
vectorTo (Point2d x2 y2) (Point2d x1 y1) =
    Vector2d (x2 - x1) (y2 - y1)


{-| Find the distance between two points.

    Point2d.distanceFrom (Point2d 1 1) (Point2d 2 2) == sqrt 2

Partial application can be useful:

    points =
        [ Point2d 3 4, Point2d 10 0, Point2d -1 2 ]

    sortedPoints =
        List.sortBy (Point2d.distanceFrom Point2d.origin) points

    sortedPoints == [ Point2d -1 2, Point2d 3 4, Point2d 10 10 ]
-}
distanceFrom : Point2d -> Point2d -> Float
distanceFrom other =
    squaredDistanceFrom other >> sqrt


{-| Find the square of the distance from one point to another.
`squaredDistanceFrom` is slightly faster than `distanceFrom`, so for example

    Point2d.squaredDistanceFrom firstPoint secondPoint > tolerance * tolerance

is equivalent to but slightly more efficient than

    Point2d.distanceFrom firstPoint secondPoint > tolerance

since the latter requires a square root. In many cases, however, the speed
difference will be negligible and using `distanceFrom` is much more readable!
-}
squaredDistanceFrom : Point2d -> Point2d -> Float
squaredDistanceFrom other =
    vectorFrom other >> Vector2d.squaredLength


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result may be
negative if the projected point is 'behind' the axis' origin point.

    axis =
        Axis2d (Point2d 1 2) Direction2d.x

    Point2d.distanceAlong axis (Point2d 3 3) == 2
    Point2d.distanceAlong axis Point2d.origin == -1
-}
distanceAlong : Axis2d -> Point2d -> Float
distanceAlong axis =
    vectorFrom axis.originPoint >> Vector2d.componentIn axis.direction


{-| Determine the perpendicular (or neareast) distance of a point from an axis.
Note that this is an unsigned value - it does not matter which side of the axis
the point is on.

    axis =
        Axis2d (Point2d 1 2) Direction2d.x

    Point2d.distanceFrom axis (Point2d 3 3) == 1 -- one unit above
    Point2d.distanceFrom axis Point2d.origin == 2 -- two units below

If you need a signed value, you could construct a perpendicular axis and measure
distance along it (using the `axis` value from above):

    perpendicularAxis =
        Axis2d.perpendicularTo axis

    Point2d.distanceAlong perpendicularAxis (Point2d 3 3) == 1
    Point2d.distanceAlong perpendicularAxis Point2d.origin == -2
-}
distanceFromAxis : Axis2d -> Point2d -> Float
distanceFromAxis axis =
    vectorFrom axis.originPoint
        >> Vector2d.crossProduct (Direction2d.asVector axis.direction)
        >> abs


{-| Add a vector to a point (translate the point by that vector).

    Point2d.plus (Vector2d 1 2) (Point2d 3 4) == Point2d 4 6
-}
plus : Vector2d -> Point2d -> Point2d
plus (Vector2d vx vy) (Point2d px py) =
    Point2d (px + vx) (py + vy)


{-| Subtract a vector from a point (translate by the negation of that vector).

    Point2d.minus (Vector2d 1 2) (Point2d 3 4) == Point2d 2 2
-}
minus : Vector2d -> Point2d -> Point2d
minus (Vector2d vx vy) (Point2d px py) =
    Point2d (px - vx) (py - vy)


addTo (Point2d px py) (Vector2d vx vy) =
    Point2d (px + vx) (py + vy)


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 does
nothing, and scaling by a factor of 0 collapses all points to the center point.

    Point2d.scaleAbout Point2d.origin 3 (Point2d 1 2) == Point2d 3 6
    Point2d.scaleAbout (Point2d 1 1) 0.5 (Point2d 5 1) == Point2d 3 1
    Point2d.scaleAbout (Point2d 1 1) 2 (Point2d 1 2) == Point2d 1 3
    Point2d.scaleAbout (Point2d 1 1) 10 (Point2d 1 1) == Point2d 1 1

Do not scale by a negative scaling factor - while this will sometimes do what
you want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.
-}
scaleAbout : Point2d -> Float -> Point2d -> Point2d
scaleAbout centerPoint scale =
    vectorFrom centerPoint >> Vector2d.times scale >> addTo centerPoint


{-| Rotate around a given center point counterclockwise by a given angle (in
radians). The point to rotate around is given first and the point to rotate is
given last.

    Point2d.rotateAround Point2d.origin (degrees 45) (Point2d 1 0) == Point2d 0.7071 0.7071
    Point2d.rotateAround (Point2d 2 1) (degrees 90) (Point2d 3 1) == Point2d 2 2
-}
rotateAround : Point2d -> Float -> Point2d -> Point2d
rotateAround centerPoint angle =
    vectorFrom centerPoint >> Vector2d.rotateBy angle >> addTo centerPoint


{-| Translate parallel to a given axis by a given distance.

    Point2d.translateAlong Axis2d.x 3 (Point2d 1 2) == Point2d 4 2
    Point2d.translateAlong Axis2d.y -4 (Point2d 1 1) == Point2d 1 -3
-}
translateAlong : Axis2d -> Float -> Point2d -> Point2d
translateAlong axis distance =
    plus (Vector2d.alongAxis axis distance)


{-| Mirror a point across an axis.

    Point2d.mirrorAcross Axis2d.x (Point2d 2 3) == Point2d -2 3

Angled axes work as well:

    diagonalAxis =
        Axis2d Point2d.origin (Direction2d.fromAngle (degrees 45))

    Point2d.mirrorAcross diagonalAxis (Point2d 3 0) == Point2d 0 3
-}
mirrorAcross : Axis2d -> Point2d -> Point2d
mirrorAcross axis =
    vectorFrom axis.originPoint
        >> Vector2d.mirrorAcross axis
        >> addTo axis.originPoint


{-| Project a point perpendicularly onto an axis.

    Point2d.projectOnto Axis2d.x (Point2d 2 3) == Point2d 2 0
    Point2d.projectOnto Axis2d.y (Point2d 2 3) == Point2d 0 3

    offsetYAxis =
        Axis2d (Point2d 1 0) Direction2d.y

    Point2d.projectOnto offsetYAxis (Point2d 2 3) == Point2d 1 3
-}
projectOnto : Axis2d -> Point2d -> Point2d
projectOnto axis =
    vectorFrom axis.originPoint
        >> Vector2d.projectOnto axis
        >> addTo axis.originPoint


{-| Convert a point from global coordinates to local coordinates within a given
frame. The result will be the given point expressed relative to the given
frame.

    localFrame =
        Frame2d.atPoint (Point2d 1 2)

    Point2d.toLocalIn localFrame (Point2d 4 5) == Point2d 3 3
    Point2d.toLocalIn localFrame (Point2d 1 0) == Point2d 0 -2
-}
toLocalIn : Frame2d -> Point2d -> Point2d
toLocalIn frame =
    vectorFrom frame.originPoint
        >> Vector2d.toLocalIn frame
        >> (\(Vector2d x y) -> Point2d x y)


{-| Convert a point from local coordinates within a given frame to global
coordinates. Inverse of `toLocalIn`.

    localFrame =
        Frame2d.atPoint (Point2d 1 2)

    Point2d.fromLocalIn localFrame (Point2d 3 3) == Point2d 4 5
    Point2d.fromLocalIn localFrame (Point2d 0 -2) == Point2d 1 0
-}
fromLocalIn : Frame2d -> Point2d -> Point2d
fromLocalIn frame =
    (\(Point2d x y) -> Vector2d x y)
        >> Vector2d.fromLocalIn frame
        >> addTo frame.originPoint


{-| Get the (x, y) coordinates of a point.

    Point2d.cooordinates (Point2d x y) == ( x, y )

Note that you could use this to extract the X and Y coordinates of a point using
tuple pattern matching, for example

    ( x, y ) = Point2d.coordinates point

but it's simpler and more efficient to use pattern matching on the point
directly:

    (Point2d x y) = point
-}
coordinates : Point2d -> ( Float, Float )
coordinates (Point2d x y) =
    ( x, y )


{-| Construct a point from (x, y) coordinates.

    Point2d.fromCoordinates ( x, y ) == Point2d x y
-}
fromCoordinates : ( Float, Float ) -> Point2d
fromCoordinates ( x, y ) =
    Point2d x y


{-| Get the polar (radius, angle) coordinates of a point. Angles will be
returned in radians.

    Point2d.polarCoordinates (Point2d 1 1) == ( sqrt 2, degrees 45 )
-}
polarCoordinates : Point2d -> ( Float, Float )
polarCoordinates =
    coordinates >> toPolar


{-| Construct a point from polar (radius, angle) coordinates.

    Point2d.fromPolarCoordinates ( radius, angle ) == Point2d.polar radius angle
-}
fromPolarCoordinates : ( Float, Float ) -> Point2d
fromPolarCoordinates =
    fromPolar >> fromCoordinates


{-| Convert a point to a record with `x` and `y` fields.

    Point2d.toRecord (Point2d 2 3) == { x = 2, y = 3 }
-}
toRecord : Point2d -> { x : Float, y : Float }
toRecord (Point2d x y) =
    { x = x, y = y }


{-| Construct a point from a record with `x` and `y` fields.

    Point2d.fromRecord { x = 2, y = 3 } == Point2d 2 3
-}
fromRecord : { x : Float, y : Float } -> Point2d
fromRecord { x, y } =
    Point2d x y
