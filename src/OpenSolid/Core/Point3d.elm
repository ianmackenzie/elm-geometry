{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Point3d
    exposing
        ( origin
        , alongAxis
        , onPlane
        , relativeTo
        , interpolate
        , midpoint
        , xCoordinate
        , yCoordinate
        , zCoordinate
        , coordinates
        , squaredDistanceFrom
        , distanceFrom
        , vectorTo
        , vectorFrom
        , distanceAlong
        , squaredDistanceFromAxis
        , distanceFromAxis
        , distanceFromPlane
        , signedDistanceFromPlane
        , scaleAbout
        , rotateAround
        , translateAlong
        , mirrorAcross
        , localizeTo
        , placeIn
        , projectOntoAxis
        , projectOnto
        , projectInto
        , plus
        , minus
        , toRecord
        , fromRecord
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


origin : Point3d
origin =
    Point3d ( 0, 0, 0 )


alongAxis : Axis3d -> Float -> Point3d
alongAxis axis =
    Vector3d.alongAxis axis >> addTo axis.originPoint


onPlane : Plane3d -> ( Float, Float ) -> Point3d
onPlane plane =
    Vector3d.onPlane plane >> addTo plane.originPoint


relativeTo : Frame3d -> ( Float, Float, Float ) -> Point3d
relativeTo frame =
    Vector3d.relativeTo frame >> addTo frame.originPoint


interpolate : Point3d -> Point3d -> Float -> Point3d
interpolate startPoint endPoint =
    let
        displacement =
            vectorFrom startPoint endPoint
    in
        \t -> plus (Vector3d.times t displacement) startPoint


midpoint : Point3d -> Point3d -> Point3d
midpoint firstPoint secondPoint =
    interpolate firstPoint secondPoint 0.5


xCoordinate : Point3d -> Float
xCoordinate (Point3d ( x, _, _ )) =
    x


yCoordinate : Point3d -> Float
yCoordinate (Point3d ( _, y, _ )) =
    y


zCoordinate : Point3d -> Float
zCoordinate (Point3d ( _, _, z )) =
    z


coordinates : Point3d -> ( Float, Float, Float )
coordinates (Point3d coordinates') =
    coordinates'


squaredDistanceFrom : Point3d -> Point3d -> Float
squaredDistanceFrom other =
    vectorFrom other >> Vector3d.squaredLength


distanceFrom : Point3d -> Point3d -> Float
distanceFrom other =
    squaredDistanceFrom other >> sqrt


vectorTo : Point3d -> Point3d -> Vector3d
vectorTo other point =
    let
        ( x', y', z' ) =
            coordinates other

        ( x, y, z ) =
            coordinates point
    in
        Vector3d ( x' - x, y' - y, z' - z )


vectorFrom : Point3d -> Point3d -> Vector3d
vectorFrom other point =
    let
        ( x', y', z' ) =
            coordinates other

        ( x, y, z ) =
            coordinates point
    in
        Vector3d ( x - x', y - y', z - z' )


distanceAlong : Axis3d -> Point3d -> Float
distanceAlong axis =
    vectorFrom axis.originPoint >> Vector3d.componentIn axis.direction


squaredDistanceFromAxis : Axis3d -> Point3d -> Float
squaredDistanceFromAxis axis =
    let
        directionVector =
            Vector3d (Direction3d.components axis.direction)
    in
        vectorFrom axis.originPoint
            >> Vector3d.crossProduct directionVector
            >> Vector3d.squaredLength


distanceFromAxis : Axis3d -> Point3d -> Float
distanceFromAxis axis =
    squaredDistanceFromAxis axis >> sqrt


distanceFromPlane : Plane3d -> Point3d -> Float
distanceFromPlane plane =
    signedDistanceFromPlane plane >> abs


signedDistanceFromPlane : Plane3d -> Point3d -> Float
signedDistanceFromPlane plane =
    vectorFrom plane.originPoint >> Vector3d.componentIn plane.normalDirection


addTo point vector =
    let
        ( px, py, pz ) =
            coordinates point

        ( vx, vy, vz ) =
            Vector3d.components vector
    in
        Point3d ( px + vx, py + vy, pz + vz )


scaleAbout : Point3d -> Float -> Point3d -> Point3d
scaleAbout centerPoint scale =
    vectorFrom centerPoint >> Vector3d.times scale >> addTo centerPoint


rotateAround : Axis3d -> Float -> Point3d -> Point3d
rotateAround axis angle =
    vectorFrom axis.originPoint
        >> Vector3d.rotateAround axis angle
        >> addTo axis.originPoint


translateAlong : Axis3d -> Float -> Point3d -> Point3d
translateAlong axis distance =
    plus (Vector3d.alongAxis axis distance)


mirrorAcross : Plane3d -> Point3d -> Point3d
mirrorAcross plane =
    vectorFrom plane.originPoint
        >> Vector3d.mirrorAcross plane
        >> addTo plane.originPoint


localizeTo : Frame3d -> Point3d -> Point3d
localizeTo frame =
    vectorFrom frame.originPoint
        >> Vector3d.localizeTo frame
        >> (\(Vector3d components) -> Point3d components)


placeIn : Frame3d -> Point3d -> Point3d
placeIn frame =
    coordinates >> relativeTo frame


projectOntoAxis : Axis3d -> Point3d -> Point3d
projectOntoAxis axis =
    vectorFrom axis.originPoint
        >> Vector3d.projectOntoAxis axis
        >> addTo axis.originPoint


projectOnto : Plane3d -> Point3d -> Point3d
projectOnto plane point =
    let
        signedDistance =
            signedDistanceFromPlane plane point

        displacement =
            Direction3d.times signedDistance plane.normalDirection
    in
        minus displacement point


projectInto : Plane3d -> Point3d -> Point2d
projectInto plane =
    vectorFrom plane.originPoint
        >> Vector3d.projectInto plane
        >> (\(Vector2d components) -> Point2d components)


plus : Vector3d -> Point3d -> Point3d
plus vector point =
    let
        ( vx, vy, vz ) =
            Vector3d.components vector

        ( px, py, pz ) =
            coordinates point
    in
        Point3d ( px + vx, py + vy, pz + vz )


minus : Vector3d -> Point3d -> Point3d
minus vector point =
    let
        ( vx, vy, vz ) =
            Vector3d.components vector

        ( px, py, pz ) =
            coordinates point
    in
        Point3d ( px - vx, py - vy, pz - vz )


toRecord : Point3d -> { x : Float, y : Float, z : Float }
toRecord point =
    let
        ( x, y, z ) =
            coordinates point
    in
        { x = x, y = y, z = z }


fromRecord : { x : Float, y : Float, z : Float } -> Point3d
fromRecord { x, y, z } =
    Point3d ( x, y, z )
