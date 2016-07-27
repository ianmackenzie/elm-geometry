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


midpoint : Point3d -> Point3d -> Point3d
midpoint firstPoint secondPoint =
    interpolate firstPoint secondPoint 0.5


interpolate : Point3d -> Point3d -> Float -> Point3d
interpolate startPoint endPoint =
    let
        displacement =
            vectorFrom startPoint endPoint
    in
        \t -> translateBy (Vector3d.times t displacement) startPoint


coordinates : Point3d -> ( Float, Float, Float )
coordinates (Point3d coordinates') =
    coordinates'


xCoordinate : Point3d -> Float
xCoordinate (Point3d ( x, _, _ )) =
    x


yCoordinate : Point3d -> Float
yCoordinate (Point3d ( _, y, _ )) =
    y


zCoordinate : Point3d -> Float
zCoordinate (Point3d ( _, _, z )) =
    z


squaredDistanceFrom : Point3d -> Point3d -> Float
squaredDistanceFrom other =
    vectorFrom other >> Vector3d.squaredLength


distanceFrom : Point3d -> Point3d -> Float
distanceFrom other =
    squaredDistanceFrom other >> sqrt


vectorFrom : Point3d -> Point3d -> Vector3d
vectorFrom other point =
    let
        ( x', y', z' ) =
            coordinates other

        ( x, y, z ) =
            coordinates point
    in
        Vector3d ( x - x', y - y', z - z' )


vectorTo : Point3d -> Point3d -> Vector3d
vectorTo =
    flip vectorFrom


distanceFromAxis : Axis3d -> Point3d -> Float
distanceFromAxis axis =
    squaredDistanceFromAxis axis >> sqrt


squaredDistanceFromAxis : Axis3d -> Point3d -> Float
squaredDistanceFromAxis axis =
    let
        (Axis3d { originPoint, direction }) =
            axis

        directionVector =
            Vector3d (Direction3d.components direction)
    in
        vectorFrom originPoint
            >> Vector3d.crossProduct directionVector
            >> Vector3d.squaredLength


signedDistanceAlong : Axis3d -> Point3d -> Float
signedDistanceAlong axis =
    let
        (Axis3d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint >> Vector3d.componentIn direction


signedDistanceFrom : Plane3d -> Point3d -> Float
signedDistanceFrom plane =
    let
        (Plane3d { originPoint, normalDirection }) =
            plane
    in
        vectorFrom originPoint >> Vector3d.componentIn normalDirection


scaleAbout : Point3d -> Float -> Point3d -> Point3d
scaleAbout centerPoint scale =
    vectorFrom centerPoint >> Vector3d.times scale >> addTo centerPoint


rotateAround : Axis3d -> Float -> Point3d -> Point3d
rotateAround axis angle =
    let
        (Axis3d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint
            >> Vector3d.rotateAround axis angle
            >> addTo originPoint


translateBy : Vector3d -> Point3d -> Point3d
translateBy vector point =
    let
        ( vx, vy, vz ) =
            Vector3d.components vector

        ( px, py, pz ) =
            coordinates point
    in
        Point3d ( px + vx, py + vy, pz + vz )


mirrorAcross : Plane3d -> Point3d -> Point3d
mirrorAcross plane =
    let
        (Plane3d { originPoint, normalDirection }) =
            plane
    in
        vectorFrom originPoint
            >> Vector3d.mirrorAcross plane
            >> addTo originPoint


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


projectOntoAxis : Axis3d -> Point3d -> Point3d
projectOntoAxis axis =
    let
        (Axis3d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint
            >> Vector3d.projectOntoAxis axis
            >> addTo originPoint


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


projectInto2d : PlanarFrame3d -> Point3d -> Point2d
projectInto2d planarFrame =
    let
        (PlanarFrame3d { originPoint, xDirection, yDirection }) =
            planarFrame
    in
        vectorFrom originPoint
            >> Vector3d.projectInto2d planarFrame
            >> (\(Vector2d components) -> Point2d components)


toRecord : Point3d -> { x : Float, y : Float, z : Float }
toRecord (Point3d ( x, y, z )) =
    { x = x, y = y, z = z }


fromRecord : { x : Float, y : Float, z : Float } -> Point3d
fromRecord { x, y, z } =
    Point3d ( x, y, z )
