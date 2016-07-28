{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Point3d
    exposing
        ( origin
        , midpoint
        , interpolate
        , coordinates
        , xCoordinate
        , yCoordinate
        , zCoordinate
        , vectorFrom
        , vectorTo
        , distanceFrom
        , squaredDistanceFrom
        , distanceFromAxis
        , squaredDistanceFromAxis
        , signedDistanceAlong
        , signedDistanceFrom
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , projectOntoAxis
        , projectOnto
        , projectInto2d
        , localizeTo
        , placeIn
        , toRecord
        , fromRecord
        )

{-| Various functions for working with `Point3d` values. For the examples below,
assume that all OpenSolid core types have been imported using

    import OpenSolid.Core.Types exposing (..)

and all necessary modules have been imported using the following pattern:

    import OpenSolid.Core.Point3d as Point3d

Examples use `==` to indicate that two expressions are equivalent, even if (due
to numerical roundoff) they might not be exactly equal.

# Constants

@docs origin

# Constructors

Since `Point3d` is not an opaque type, the simplest way to construct one is
directly from its X, Y and Z coordinates, for example `Point2d ( 2, 1, 3 )`. But
that is not the only way!

@docs along, midpoint, interpolate

# Coordinates

@docs coordinates, xCoordinate, yCoordinate, zCoordinate

# Displacement

@docs vectorFrom, vectorTo

# Distance

@docs distanceFrom, squaredDistanceFrom, distanceFromAxis, squaredDistanceFromAxis, signedDistanceAlong, signedDistanceFrom

# Transformations

@docs scaleAbout, rotateAround, translateBy, mirrorAcross, projectOntoAxis, projectOnto

# Coordinate conversions

Functions for transforming points between local and global coordinates in
different coordinate systems. Although these examples use a simple offset
frame, these functions can be used to convert to and from local coordinates in
arbitrarily transformed (translated, rotated, mirrored) frames.

@docs projectInto2d, localizeTo, placeIn

# Record conversions

Convert `Point3d` values to and from Elm records. Primarily useful for
interoperability with other libraries. For example, you could define conversion
functions to and from `elm-linear-algebra`'s `Vec3` type with

    toVec3 : Point3d -> Math.Vector3.Vec3
    toVec3 =
        Point3d.toRecord >> Math.Vector3.fromRecord

    fromVec3 : Math.Vector3.Vec3 -> Point3d
    fromVec3 =
        Math.Vector3.toRecord >> Point3d.fromRecord

although in this particular case it would likely be simpler and more efficient
to use

    toVec3 =
        Point3d.coordinates >> Math.Vector3.fromTuple

    fromVec3 =
        Math.Vector3.toTuple >> Point3d

@docs toRecord, fromRecord
-}

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


addTo : Point3d -> Vector3d -> Point3d
addTo =
    flip translateBy


{-| The point (0, 0, 0).
-}
origin : Point3d
origin =
    Point3d ( 0, 0, 0 )


{-| Construct a point along an axis at a particular distance from the axis'
origin point. Positive and negative distances will be interpreted relative to
the direction of the axis.

    horizontalAxis =
        Axis3d { originPoint = Point2d ( 1, 1, 1 ), direction = Direction3d.x }

    Point3d.along horizontalAxis 3 == Point3d ( 4, 1, 1 )
    Point3d.along horizontalAxis -3 == Point3d ( -2, 1, 1 )
    Point3d.along (Axis3d.flip horizontalAxis) 3 == Point3d ( -2, 1, 1 )
-}
along : Axis3d -> Float -> Point3d
along (Axis3d { originPoint, direction }) distance =
    translateBy (Direction3d.times distance direction) originPoint


{-| Construct a point halfway between two other points.

    Point3d.midpoint (Point3d ( 1, 1, 1 )) (Point3d ( 3, 7, 9 )) ==
        Point3d ( 2, 4, 5 )
-}
midpoint : Point3d -> Point3d -> Point3d
midpoint firstPoint secondPoint =
    interpolate firstPoint secondPoint 0.5


{-| Construct a point by interpolating between two other points based on a
parameter that ranges from zero to one.

    interpolatedPoint : Float -> Point3d
    interpolatedPoint =
        Point3d.interpolate Point3d.origin (Point3d ( 4, 8, 12 ))

    interpolatedPoint 0 == Point3d ( 0, 0, 0 )
    interpolatedPoint 0.25 == Point3d ( 1, 2, 3 )
    interpolatedPoint 0.75 == Point3d ( 3, 6, 9 )
    interpolatedPoint 1 == Point3d ( 4, 8, 12 )

You can actually pass values less than zero or greater than one to extrapolate:

    interpolatedPoint 1.25 == Point3d ( 5, 10, 15 )
-}
interpolate : Point3d -> Point3d -> Float -> Point3d
interpolate startPoint endPoint =
    let
        displacement =
            vectorFrom startPoint endPoint
    in
        \t -> translateBy (Vector3d.times t displacement) startPoint


{-| Get the (x, y, z) coordinates of a point as a tuple.

    ( x, y, z ) = Point3d.coordinates point
-}
coordinates : Point3d -> ( Float, Float, Float )
coordinates (Point3d coordinates') =
    coordinates'


{-| Get the X coordinate of a point.

    Point3d.xCoordinate (Point2d ( 2, 1, 3 )) == 2
-}
xCoordinate : Point3d -> Float
xCoordinate (Point3d ( x, _, _ )) =
    x


{-| Get the Y coordinate of a point.

    Point3d.yCoordinate (Point2d ( 2, 1, 3 )) == 1
-}
yCoordinate : Point3d -> Float
yCoordinate (Point3d ( _, y, _ )) =
    y


{-| Get the Z coordinate of a point.

    Point3d.zCoordinate (Point2d ( 2, 1, 3 )) == 3
-}
zCoordinate : Point3d -> Float
zCoordinate (Point3d ( _, _, z )) =
    z


{-| Find the vector from one point to another.

    Point3d.vectorFrom (Point3d ( 1, 1, 1 )) (Point3d ( 4, 5, 6 )) ==
        Vector3d ( 3, 4, 5 )
-}
vectorFrom : Point3d -> Point3d -> Vector3d
vectorFrom other point =
    let
        ( x', y', z' ) =
            coordinates other

        ( x, y, z ) =
            coordinates point
    in
        Vector3d ( x - x', y - y', z - z' )


{-| Flipped version of `vectorFrom`, where the end point is given first.

    Point2d.vectorTo (Point2d ( 1, 1, 1 )) (Point2d ( 4, 5, 6 )) ==
        Vector2d ( -3, -4, -5 )
-}
vectorTo : Point3d -> Point3d -> Vector3d
vectorTo =
    flip vectorFrom


{-| Find the distance between two points.

    Point3d.distanceFrom (Point3d ( 1, 1, 1 )) (Point3d ( 2, 2, 2 )) == sqrt 3

Partial application can be useful:

    points =
        [ Point3d ( 3, 4, 5 ), Point3d ( 10, 10, 10 ), Point3d ( -1, 2, -3 ) ]

    List.sortBy (Point3d.distanceFrom Point3d.origin) points ==
        [ Point3d ( -1, 2, -3 ), Point3d ( 3, 4, 5 ), Point3d ( 10, 10, 10 ) ]
-}
distanceFrom : Point3d -> Point3d -> Float
distanceFrom other =
    squaredDistanceFrom other >> sqrt


{-| Find the square of the distance from one point to another.
`squaredDistanceFrom` is slightly faster than `distanceFrom`, so for example

    Point3d.squaredDistanceFrom firstPoint secondPoint > tolerance * tolerance

is equivalent to but slightly more efficient than

    Point3d.distanceFrom firstPoint secondPoint > tolerance

since the latter requires a square root under the hood. In many cases, however,
the speed difference will be negligible and using `distanceFrom` is much more
readable!
-}
squaredDistanceFrom : Point3d -> Point3d -> Float
squaredDistanceFrom other =
    vectorFrom other >> Vector3d.squaredLength


{-| Find the perpendicular (nearest) distance of a point from an axis.

    Point3d.distanceFromAxis Axis3d.x (Point3d ( -1, 2, 0 )) == 2
    Point3d.distanceFromAxis Axis3d.y (Point3d ( -1, 2, 0 )) == 1
    Point3d.distanceFromAxis Axis3d.z (Point3d ( -1, 2, 0 )) == sqrt 5

Note that unlike in 2D, the result is always positive (unsigned) since there is
no such thing as the left or right side of an axis in 3D.
-}
distanceFromAxis : Axis3d -> Point3d -> Float
distanceFromAxis axis =
    squaredDistanceFromAxis axis >> sqrt


{-| Find the square of the perpendicular distance of a point from an axis. As
with `distanceFrom`/`squaredDistanceFrom` this is slightly more efficient than
`distanceFromAxis` since it avoids a square root.
-}
squaredDistanceFromAxis : Axis3d -> Point3d -> Float
squaredDistanceFromAxis axis =
    let
        (Axis3d { originPoint, direction }) =
            axis

        directionVector =
            Direction3d.toVector direction
    in
        vectorFrom originPoint
            >> Vector3d.crossProduct directionVector
            >> Vector3d.squaredLength


{-| Determine how far along an axis a particular point lies. Conceptually, the
point is projected perpendicularly onto the axis, and then the distance of this
projected point from the axis' origin point is measured. The result will be
positive if the projected point is ahead the axis' origin point and negative if
it is behind, with 'ahead' and 'behind' defined by the direction of the axis.

    axis =
        Axis3d { originPoint = Point3d ( 1, 0, 0 ), direction = Direction3d.x }

    Point3d.signedDistanceAlong axis (Point3d ( 3, 3, 3 )) == 2
    Point3d.signedDistanceAlong axis Point3d.origin == -1
-}
signedDistanceAlong : Axis3d -> Point3d -> Float
signedDistanceAlong axis =
    let
        (Axis3d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint >> Vector3d.componentIn direction


{-| Find the perpendicular distance of a point from a plane. The result will be
positive if the point is 'above' the plane and negative if it is 'below', with
'up' defined by the normal direction of the plane. This means that flipping a
plane (reversing its normal direction) will also flip the sign of the result of
this function.

    plane =
        Plane3d
            { originPoint = Point2d ( 1, 2, 3 )
            , normalDirection = Direction2d.y
            }

    Point3d.signedDistanceFrom plane (Point3d ( 3, 3, 3 )) == 1
    Point3d.signedDistanceFrom plane Point3d.origin == -2

    Point3d.signedDistanceFrom (Plane3d.flip plane) (Point3d ( 3, 3, 3 )) == -1
    Point3d.signedDistanceFrom (Plane3d.flip plane) Point3d.origin == 2
-}
signedDistanceFrom : Plane3d -> Point3d -> Float
signedDistanceFrom plane =
    let
        (Plane3d { originPoint, normalDirection }) =
            plane
    in
        vectorFrom originPoint >> Vector3d.componentIn normalDirection


{-| Perform a uniform scaling about the given center point. The center point is
given first and the point to transform is given last. Points will contract or
expand about the center point by the given scale. Scaling by a factor of 1 is a
no-op, and scaling by a factor of 0 collapses all points to the center point.

    Point3d.scaleAbout Point3d.origin 3 (Point3d ( 1, 2, 3 )) ==
        Point3d ( 3, 6, 9 )

    Point3d.scaleAbout (Point3d ( 1, 1, 1 )) 2 (Point3d ( 1, 2, 3 )) ==
        Point3d ( 1, 3, 5 )

    Point3d.scaleAbout (Point3d ( 1, 1, 1 )) 0.5 (Point3d ( 1, 2, 3 )) ==
        Point3d ( 1, 1.5, 2 )

    Point3d.scaleAbout (Point3d ( 1, 1, 1 )) 10 (Point3d ( 1, 1, 1 )) ==
        Point3d ( 1, 1, 1 )

Do not scale by a negative scaling factor - while this may sometimes do what you
want it is confusing and error prone. Try a combination of mirror and/or
rotation operations instead.
-}
scaleAbout : Point3d -> Float -> Point3d -> Point3d
scaleAbout centerPoint scale =
    vectorFrom centerPoint >> Vector3d.times scale >> addTo centerPoint


{-| Rotate a point around an axis by a given angle (in radians).

    Point3d.rotateAround Axis3d.x (degrees 90) (Point3d ( 3, 1, 0 )) ==
        Point3d ( 3, 0, 1 )

    Point3d.rotateAround Axis3d.z (degrees 45) (Point3d ( 1, 0, 0 )) ==
        Point3d ( 0.7071, 0.7071, 0 )

Rotation direction is given by the right-hand rule, counterclockwise about the
direction of the axis.
-}
rotateAround : Axis3d -> Float -> Point3d -> Point3d
rotateAround axis angle =
    let
        (Axis3d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint
            >> Vector3d.rotateAround axis angle
            >> addTo originPoint


{-| Translate a point by a given displacement. You can think of this as 'plus'.

    Point3d.translateBy (Vector3d ( 1, 2, 3 )) (Point3d ( 3, 4, 5 )) ==
        Point3d ( 4, 6, 8 )
-}
translateBy : Vector3d -> Point3d -> Point3d
translateBy vector point =
    let
        ( vx, vy, vz ) =
            Vector3d.components vector

        ( px, py, pz ) =
            coordinates point
    in
        Point3d ( px + vx, py + vy, pz + vz )


{-| Mirror a point across a plane. The result will be the same distance from the
plane but on the opposite side.

    Point3d.mirrorAcross Plane3d.xy (Point3d ( 1, 2, 3 )) ==
        Point3d ( 1, 2, -3 )

    Point3d.mirrorAcross Plane3d.yz (Point3d ( 1, 2, 3 )) ==
        Point3d ( -1, 2, 3 )

    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    -- The origin point is 1 unit below the offset
    -- plane, so its mirrored copy is one unit above
    Point3d.mirrorAcross offsetPlane Point3d.origin ==
        Point3d ( 0, 0, 2 )
-}
mirrorAcross : Plane3d -> Point3d -> Point3d
mirrorAcross plane =
    let
        (Plane3d { originPoint, normalDirection }) =
            plane
    in
        vectorFrom originPoint
            >> Vector3d.mirrorAcross plane
            >> addTo originPoint


{-| Project a point perpendicularly onto a plane.

    Point3d.projectOnto Plane3d.xy (Point3d ( 1, 2, 3 )) ==
        Point3d ( 1, 2, 0 )

    Point3d.projectOnto Plane3d.yz (Point3d ( 1, 2, 3 )) ==
        Point3d ( 0, 2, 3 )

    offsetPlane =
        Plane3d.offsetBy 1 Plane3d.xy

    Point3d.projectOnto offsetPlane (Point3d ( 1, 2, 3 )) ==
        Point3d ( 1, 2, 1 )
-}
projectOnto : Plane3d -> Point3d -> Point3d
projectOnto plane point =
    let
        (Plane3d { originPoint, normalDirection }) =
            plane

        signedDistance =
            signedDistanceFrom plane point

        displacement =
            Direction3d.times -signedDistance normalDirection
    in
        translateBy displacement point


{-| Project a point perpendicularly onto an axis.

    Point3d.projectOntoAxis Axis2d.x (Point3d ( 1, 2, 3 )) ==
        Point2d ( 1, 0, 0 )

    Point3d.projectOntoAxis Axis2d.y (Point3d ( 1, 2, 3 )) ==
        Point2d ( 0, 2, 0 )

    verticalAxis =
        Axis3d { originPoint = Point3d ( 1, 1, 1 ), direction = Direction3d.z }

    Point3d.projectOntoAxis verticalAxis (Point3d ( 10, 10, 10 )) ==
        Point3d ( 1, 1, 10 )
-}
projectOntoAxis : Axis3d -> Point3d -> Point3d
projectOntoAxis axis =
    let
        (Axis3d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint
            >> Vector3d.projectOntoAxis axis
            >> addTo originPoint


projectInto2d : PlanarFrame3d -> Point3d -> Point2d
projectInto2d planarFrame =
    let
        (PlanarFrame3d { originPoint, xDirection, yDirection }) =
            planarFrame
    in
        vectorFrom originPoint
            >> Vector3d.projectInto2d planarFrame
            >> (\(Vector2d components) -> Point2d components)


localizeTo : Frame3d -> Point3d -> Point3d
localizeTo frame =
    let
        (Frame3d { originPoint, xDirection, yDirection, zDirection }) =
            frame
    in
        vectorFrom originPoint
            >> Vector3d.localizeTo frame
            >> (\(Vector3d components) -> Point3d components)


placeIn : Frame3d -> Point3d -> Point3d
placeIn frame =
    let
        (Frame3d { originPoint, xDirection, yDirection, zDirection }) =
            frame
    in
        coordinates >> Vector3d >> Vector3d.placeIn frame >> addTo originPoint


toRecord : Point3d -> { x : Float, y : Float, z : Float }
toRecord (Point3d ( x, y, z )) =
    { x = x, y = y, z = z }


fromRecord : { x : Float, y : Float, z : Float } -> Point3d
fromRecord { x, y, z } =
    Point3d ( x, y, z )
