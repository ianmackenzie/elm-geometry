{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Point3d
    exposing
        ( origin
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
        , signedDistanceFrom
        , scaleAbout
        , rotateAround
        , translateBy
        , translateIn
        , mirrorAcross
        , localizeTo
        , placeIn
        , projectOntoAxis
        , projectOnto
        , projectInto
        , toRecord
        , fromRecord
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


addTo =
    flip translateBy


origin : Point3d
origin =
    Point3d ( 0, 0, 0 )


interpolate : Point3d -> Point3d -> Float -> Point3d
interpolate startPoint endPoint =
    let
        displacement =
            vectorFrom startPoint endPoint
    in
        \t -> translateBy (Vector3d.times t displacement) startPoint


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


distanceAlong : Axis3d -> Point3d -> Float
distanceAlong axis =
    let
        (Axis3d { originPoint, direction }) =
            axis
    in
        vectorFrom originPoint >> Vector3d.componentIn direction


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


distanceFromAxis : Axis3d -> Point3d -> Float
distanceFromAxis axis =
    squaredDistanceFromAxis axis >> sqrt


distanceFromPlane : Plane3d -> Point3d -> Float
distanceFromPlane plane =
    signedDistanceFrom plane >> abs


signedDistanceFrom : Plane3d -> Point3d -> Float
signedDistanceFrom plane =
    let
        (Plane3d { originPoint, xDirection, yDirection, normalDirection }) =
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


translateIn : Direction3d -> Float -> Point3d -> Point3d
translateIn direction =
    translateBy << Vector3d.inDirection direction


mirrorAcross : Plane3d -> Point3d -> Point3d
mirrorAcross plane =
    let
        (Plane3d { originPoint, xDirection, yDirection, normalDirection }) =
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
        (Plane3d { originPoint, xDirection, yDirection, normalDirection }) =
            plane

        signedDistance =
            signedDistanceFrom plane point

        displacement =
            Vector3d.inDirection normalDirection -signedDistance
    in
        translateBy displacement point


projectInto : Plane3d -> Point3d -> Point2d
projectInto plane =
    let
        (Plane3d { originPoint, xDirection, yDirection, normalDirection }) =
            plane
    in
        vectorFrom originPoint
            >> Vector3d.projectInto plane
            >> (\(Vector2d components) -> Point2d components)


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
