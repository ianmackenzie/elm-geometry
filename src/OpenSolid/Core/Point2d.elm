{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Point2d
    exposing
        ( origin
        , along
        , midpoint
        , interpolate
        , coordinates
        , xCoordinate
        , yCoordinate
        , vectorFrom
        , vectorTo
        , distanceFrom
        , squaredDistanceFrom
        , signedDistanceFrom
        , signedDistanceAlong
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , projectOnto
        , localizeTo
        , placeIn
        , placeIn3d
        , toRecord
        , fromRecord
        )

{-| Various functions for working with `Point2d` values. For the examples below,
assume that all OpenSolid core types have been imported using

    import OpenSolid.Core.Types exposing (..)

and all necessary modules have been imported using the following pattern:

    import OpenSolid.Core.Point2d as Point2d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Constants

@docs origin

# Constructors

Since `Point2d` is not an opaque type, the simplest way to construct one is
directly from its X and Y coordinates, for example `Point2d ( 2, 3 )`. But that
is not the only way!

There are no specific functions to create points from polar components, but you
can use Elm's built-in `fromPolar` function, for example
`Point2d (fromPolar ( radius, angle ))`.

@docs along, midpoint, interpolate

# Coordinates

@docs coordinates, xCoordinate, yCoordinate

# Displacement

@docs vectorFrom, vectorTo

# Distance

@docs distanceFrom, squaredDistanceFrom, signedDistanceFrom, signedDistanceAlong

# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOnto

# Coordinate conversions

Functions for transforming points between local and global coordinates in
different coordinate systems. Although these examples use a simple offset
frame, these functions can be used to convert to and from local coordinates in
arbitrarily transformed (translated, rotated, mirrored) frames.

@docs localizeTo, placeIn, placeIn3d

# Record conversions

Convert `Point2d` values to and from Elm records. Primarily useful for
interoperability with other libraries. For example, you could define conversion
functions to and from `elm-linear-algebra`'s `Vec2` type with

    toVec2 : Point2d -> Math.Vector2.Vec2
    toVec2 =
        Point2d.toRecord >> Math.Vector2.fromRecord

    fromVec2 : Math.Vector2.Vec2 -> Point2d
    fromVec2 =
        Math.Vector2.toRecord >> Point2d.fromRecord

although in this particular case it would likely be simpler and more efficient
to use

    toVec2 =
        Point2d.coordinates >> Math.Vector2.fromTuple

    fromVec2 =
        Math.Vector2.toTuple >> Point2d

@docs toRecord, fromRecord
-}

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d


addTo : Point2d -> Vector2d -> Point2d
addTo =
    flip translateBy


{-| The point (0, 0).
-}
origin : Point2d
origin =
    Point2d ( 0, 0 )


{-| Construct a point along an axis at a particular distance from the axis'
origin point. Positive and negative distances will be interpreted relative to
the direction of the axis.

    horizontalAxis =
        Axis2d { originPoint = Point2d ( 1, 1 ), direction = Direction2d.x }

    Point2d.along horizontalAxis 3 == Point2d ( 4, 1 )
    Point2d.along horizontalAxis -3 == Point2d ( -2, 1 )
    Point2d.along (Axis2d.flip horizontalAxis) 3 == Point2d ( -2, 1 )
-}
along : Axis2d -> Float -> Point2d
along (Axis2d { originPoint, direction }) distance =
    translateBy (Direction2d.times distance direction) originPoint


{-| Construct a point halfway between two other points.

    Point2d.midpoint (Point2d ( 1, 1 )) (Point2d ( 3, 7 )) == Point2d ( 2, 4 )
-}
midpoint : Point2d -> Point2d -> Point2d
midpoint firstPoint secondPoint =
    interpolate firstPoint secondPoint 0.5


{-| Construct a point by interpolating between two other points based on a
parameter that ranges from zero to one.

    interpolatedPoint : Float -> Point2d
    interpolatedPoint parameter =
        Point2d.interpolate Point2d.origin (Point2d ( 8, 8 )) parameter

    interpolatedPoint 0 == Point2d ( 0, 0 )
    interpolatedPoint 0.25 == Point2d ( 2, 2 )
    interpolatedPoint 0.75 == Point2d ( 6, 6 )
    interpolatedPoint 1 == Point2d ( 8, 8 )

You can actually pass values less than zero or greater than one to extrapolate:

    interpolatedPoint 1.25 == Point2d ( 10, 10 )
-}
interpolate : Point2d -> Point2d -> Float -> Point2d
interpolate startPoint endPoint =
    let
        displacement =
            vectorFrom startPoint endPoint
    in
        \t -> translateBy (Vector2d.times t displacement) startPoint


{-| Get the (x, y) coordinates of a point as a tuple.

    ( x, y ) = Point2d.coordinates point

To get the polar coordinates of a point, you can use Elm's built in `toPolar`
function:

    ( radius, angle ) = toPolar (Point2d.coordinates point)
-}
coordinates : Point2d -> ( Float, Float )
coordinates (Point2d coordinates') =
    coordinates'


{-| Get the X coordinate of a point.

    Point2d.xCoordinate (Point2d ( 2, 3 )) == 2
-}
xCoordinate : Point2d -> Float
xCoordinate =
    coordinates >> fst


{-| Get the Y coordinate of a point.

    Point2d.yCoordinate (Point2d ( 2, 3 )) == 3
-}
yCoordinate : Point2d -> Float
yCoordinate =
    coordinates >> snd


{-| Find the vector from one point to another.

    Point2d.vectorFrom (Point2d ( 1, 1 )) (Point2d ( 4, 5 )) ==
        Vector2d ( 3, 4 )
-}
vectorFrom : Point2d -> Point2d -> Vector2d
vectorFrom other point =
    let
        ( x', y' ) =
            coordinates other

        ( x, y ) =
            coordinates point
    in
        Vector2d ( x - x', y - y' )


{-| Flipped version of `vectorFrom`, where the end point is given first.

    Point2d.vectorTo (Point2d ( 1, 1 )) (Point2d ( 4, 5 )) ==
        Vector2d ( -3, -4 )
-}
vectorTo : Point2d -> Point2d -> Vector2d
vectorTo =
    flip vectorFrom


{-| Find the distance between two points.

    Point2d.distanceFrom (Point2d ( 1, 1 )) (Point2d ( 2, 2 )) == sqrt 2

Partial application can be useful:

    points =
        [ Point2d ( 3, 4 ), Point2d ( 10, 0 ), Point2d ( -1, 2 ) ]

    List.sortBy (Point2d.distanceFrom Point2d.origin) points ==
        [ Point2d ( -1, 2 ), Point2d ( 3, 4 ), Point2d ( 10, 0 ) ]
-}
distanceFrom : Point2d -> Point2d -> Float
distanceFrom other =
    squaredDistanceFrom other >> sqrt


{-| Find the square of the distance from one point to another.
`squaredDistanceFrom` is slightly faster than `distanceFrom`, so for example

    Point2d.squaredDistanceFrom firstPoint secondPoint > tolerance * tolerance

is equivalent to but slightly more efficient than

    Point2d.distanceFrom firstPoint secondPoint > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `distanceFrom` is much more
readable!
-}
squaredDistanceFrom : Point2d -> Point2d -> Float
squaredDistanceFrom other =
    vectorFrom other >> Vector2d.squaredLength


{-| Determine the perpendicular distance of a point from an axis. The result
will be positive if the point is to the left of the axis and negative if it is
to the right, with the forwards direction defined by the direction of the axis.
This means that flipping an axis (reversing its direction) will also flip the
sign of the result of this function.

    axis =
        Axis2d { originPoint = Point2d ( 1, 2 ), direction = Direction2d.x }

    Point2d.distanceFrom axis (Point2d ( 3, 3 )) == 1
    Point2d.distanceFrom axis Point2d.origin == -2

    Point2d.distanceFrom (Axis2d.flip axis) (Point2d ( 3, 3 )) == -1
    Point2d.distanceFrom (Axis2d.flip axis) Point2d.origin == 2
-}
signedDistanceFrom : Axis2d -> Point2d -> Float
signedDistanceFrom axis =
    let
        (Axis2d { originPoint, direction }) =
            axis

        directionVector =
            Vector2d (Direction2d.components direction)
    in
        vectorFrom originPoint >> Vector2d.crossProduct directionVector


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis2d { originPoint = Point2d ( 1, 2 ), direction = Direction2d.x }

    Point2d.signedDistanceAlong axis (Point2d ( 3, 3 )) == 2
    Point2d.signedDistanceAlong axis Point2d.origin == -1
-}
signedDistanceAlong : Axis2d -> Point2d -> Float
signedDistanceAlong axis =
    let
        (Axis2d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint >> Vector2d.componentIn direction


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    Point2d.scaleAbout Point2d.origin 3 (Point2d ( 1, 2 )) ==
        Point2d ( 3, 6 )

    Point2d.scaleAbout (Point2d ( 1, 1 )) 0.5 (Point2d ( 5, 1 )) ==
        Point2d ( 3, 1 )

    Point2d.scaleAbout (Point2d ( 1, 1 )) 2 (Point2d ( 1, 2 )) ==
        Point2d ( 1, 3 )

    Point2d.scaleAbout (Point2d ( 1, 1 )) 10 (Point2d ( 1, 1 )) ==
        Point2d ( 1, 1 )

Do not scale by a negative scaling factor - while this may sometimes do what you
want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.
-}
scaleAbout : Point2d -> Float -> Point2d -> Point2d
scaleAbout centerPoint scale =
    vectorFrom centerPoint >> Vector2d.times scale >> addTo centerPoint


{-| Rotate around a given center point counterclockwise by a given angle (in
radians). The point to rotate around is given first and the point to rotate is
given last.

    Point2d.rotateAround Point2d.origin (degrees 45) (Point2d ( 1, 0 )) ==
        Point2d ( 0.7071, 0.7071 )

    Point2d.rotateAround (Point2d ( 2, 1 )) (degrees 90) (Point2d ( 3, 1 )) ==
        Point2d ( 2, 2 )
-}
rotateAround : Point2d -> Float -> Point2d -> Point2d
rotateAround centerPoint angle =
    vectorFrom centerPoint >> Vector2d.rotateBy angle >> addTo centerPoint


{-| Translate a point by a given displacement. You can think of this as 'plus'.

    Point2d.translateBy (Vector2d ( 1, 2 )) (Point2d ( 3, 4 )) ==
        Point2d ( 4, 6 )
-}
translateBy : Vector2d -> Point2d -> Point2d
translateBy vector point =
    let
        ( vx, vy ) =
            Vector2d.components vector

        ( px, py ) =
            coordinates point
    in
        Point2d ( px + vx, py + vy )


{-| Mirror a point across an axis.

    Point2d.mirrorAcross Axis2d.x (Point2d ( 2, 3 )) == Point2d ( -2, 3 )
-}
mirrorAcross : Axis2d -> Point2d -> Point2d
mirrorAcross axis =
    let
        (Axis2d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint
            >> Vector2d.mirrorAcross axis
            >> addTo originPoint


{-| Project a point perpendicularly onto an axis.

    Point2d.projectOnto Axis2d.x (Point2d ( 2, 3 )) == Point2d ( 2, 0 )
    Point2d.projectOnto Axis2d.y (Point2d ( 2, 3 )) == Point2d ( 0, 3 )

    offsetYAxis =
        Axis2d { originPoint = Point2d ( 1, 0 ), direction = Direction2d.y }

    Point2d.projectOnto offsetYAxis (Point2d ( 2, 3 )) == Point2d ( 1, 3 )
-}
projectOnto : Axis2d -> Point2d -> Point2d
projectOnto axis =
    let
        (Axis2d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint
            >> Vector2d.projectOnto axis
            >> addTo originPoint


{-| Convert a point from global coordinates to local coordinates within a given
frame. The result will be the given point expressed relative to the given
frame.

    localFrame =
        Frame2d.translateTo (Point2d ( 1, 2 )) Frame2d.xy

    Point2d.localizeTo localFrame (Point2d ( 4, 5 )) == Point2d ( 3, 3 )
    Point2d.localizeTo localFrame (Point2d ( 1, 0 )) == Point2d ( 0, -2 )
-}
localizeTo : Frame2d -> Point2d -> Point2d
localizeTo frame =
    let
        (Frame2d { originPoint, xDirection, yDirection }) =
            frame
    in
        vectorFrom originPoint
            >> Vector2d.localizeTo frame
            >> Vector2d.components
            >> Point2d


{-| Convert a point from local coordinates within a given frame to global
coordinates. Inverse of `localizeTo`.

    localFrame =
        Frame2d.translateTo (Point2d ( 1, 2 )) Frame2d.xy

    Point2d.placeIn localFrame (Point2d ( 3, 3 )) == Point2d ( 4, 5 )
    Point2d.placeIn localFrame (Point2d ( 0, -2 )) == Point2d ( 1, 0 )
-}
placeIn : Frame2d -> Point2d -> Point2d
placeIn frame =
    let
        (Frame2d { originPoint, xDirection, yDirection }) =
            frame
    in
        coordinates >> Vector2d >> Vector2d.placeIn frame >> addTo originPoint


{-| Take a point defined by 2D coordinates within a particular planar frame and
convert it to global 3D coordinates.

    Point2d.placeIn3d PlanarFrame3d.xz (Point2d ( 2, 1 )) == Point3d ( 2, 0, 1 )

    rotatedFrame =
        PlanarFrame3d.rotateAround Axis3d.x (degrees 45) PlanarFrame3d.xy

    Point2d.placeIn3d rotatedFrame (Point2d ( 2, 1 )) ==
        Point3d ( 2, 0.7071, 0.7071 )
-}
placeIn3d : PlanarFrame3d -> Point2d -> Point3d
placeIn3d planarFrame point =
    let
        (PlanarFrame3d { originPoint, xDirection, yDirection }) =
            planarFrame

        (Point3d ( px, py, pz )) =
            originPoint

        (Vector3d ( vx, vy, vz )) =
            Vector2d.placeIn3d planarFrame (Vector2d (coordinates point))
    in
        Point3d ( px + vx, py + vy, pz + vz )


{-| Convert a point to a record with `x` and `y` fields.

    Point2d.toRecord (Point2d ( 2, 3 )) == { x = 2, y = 3 }
-}
toRecord : Point2d -> { x : Float, y : Float }
toRecord (Point2d ( x, y )) =
    { x = x, y = y }


{-| Construct a point from a record with `x` and `y` fields.

    Point2d.fromRecord { x = 2, y = 3 } == Point2d ( 2, 3 )
-}
fromRecord : { x : Float, y : Float } -> Point2d
fromRecord { x, y } =
    Point2d ( x, y )
