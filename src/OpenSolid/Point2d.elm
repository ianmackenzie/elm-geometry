{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Point2d
    exposing
        ( origin
        , relativeTo
        , along
        , fromCoordinates
        , fromPolarCoordinates
        , xCoordinate
        , yCoordinate
        , coordinates
        , polarCoordinates
        , squaredDistanceTo
        , distanceTo
        , vectorTo
        , vectorFrom
        , distanceAlong
        , distanceFromAxis
        , scaleAbout
        , rotateAround
        , translateAlong
        , mirrorAcross
        , toLocalIn
        , toGlobalFrom
        , projectOnto
        , placeOnto
        , plus
        , minus
        , toRecord
        , fromRecord
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d


origin : Point2d
origin =
    Point2d 0 0


relativeTo : Frame2d -> Float -> Float -> Point2d
relativeTo frame =
    let
        relativeVector =
            Vector2d.relativeTo frame
    in
        \x y -> plus (relativeVector x y) frame.originPoint


along : Axis2d -> Float -> Point2d
along axis distance =
    plus (Vector2d.along axis distance) axis.originPoint


fromCoordinates : ( Float, Float ) -> Point2d
fromCoordinates ( x, y ) =
    Point2d x y


fromPolarCoordinates : ( Float, Float ) -> Point2d
fromPolarCoordinates =
    fromPolar >> fromCoordinates


xCoordinate : Point2d -> Float
xCoordinate (Point2d x _) =
    x


yCoordinate : Point2d -> Float
yCoordinate (Point2d _ y) =
    y


coordinates : Point2d -> ( Float, Float )
coordinates (Point2d x y) =
    ( x, y )


polarCoordinates : Point2d -> ( Float, Float )
polarCoordinates =
    coordinates >> toPolar


squaredDistanceTo : Point2d -> Point2d -> Float
squaredDistanceTo other =
    vectorTo other >> Vector2d.squaredLength


distanceTo : Point2d -> Point2d -> Float
distanceTo other =
    squaredDistanceTo other >> sqrt


vectorTo : Point2d -> Point2d -> Vector2d
vectorTo (Point2d x2 y2) (Point2d x1 y1) =
    Vector2d (x2 - x1) (y2 - y1)


vectorFrom : Point2d -> Point2d -> Vector2d
vectorFrom (Point2d x2 y2) (Point2d x1 y1) =
    Vector2d (x1 - x2) (y1 - y2)


distanceAlong : Axis2d -> Point2d -> Float
distanceAlong axis =
    vectorFrom axis.originPoint >> Vector2d.componentIn axis.direction


distanceFromAxis : Axis2d -> Point2d -> Float
distanceFromAxis axis =
    vectorFrom axis.originPoint
        >> Vector2d.crossProduct (Direction2d.vector axis.direction)
        >> abs


addTo (Point2d px py) (Vector2d vx vy) =
    Point2d (px + vx) (py + vy)


scaleAbout : Point2d -> Float -> Point2d -> Point2d
scaleAbout centerPoint scale =
    vectorFrom centerPoint >> Vector2d.times scale >> addTo centerPoint


rotateAround : Point2d -> Float -> Point2d -> Point2d
rotateAround centerPoint angle =
    let
        rotateVector =
            Vector2d.rotateBy angle
    in
        vectorFrom centerPoint >> rotateVector >> addTo centerPoint


translateAlong : Axis2d -> Float -> Point2d -> Point2d
translateAlong axis distance =
    plus (Vector2d.along axis distance)


mirrorAcross : Axis2d -> Point2d -> Point2d
mirrorAcross axis =
    let
        mirrorVector =
            Vector2d.mirrorAbout axis.direction
    in
        vectorFrom axis.originPoint >> mirrorVector >> addTo axis.originPoint


toLocalIn : Frame2d -> Point2d -> Point2d
toLocalIn frame =
    let
        localizeVector =
            Vector2d.toLocalIn frame
    in
        vectorFrom frame.originPoint
            >> localizeVector
            >> (\(Vector2d x y) -> Point2d x y)


toGlobalFrom : Frame2d -> Point2d -> Point2d
toGlobalFrom frame =
    let
        globalizeVector =
            Vector2d.toGlobalFrom frame
    in
        (\(Point2d x y) -> Vector2d x y)
            >> globalizeVector
            >> addTo frame.originPoint


projectOnto : Axis2d -> Point2d -> Point2d
projectOnto axis =
    vectorFrom axis.originPoint
        >> Vector2d.projectOnto axis
        >> addTo axis.originPoint


placeOnto : Plane3d -> Point2d -> Point3d
placeOnto plane (Point2d x y) =
    let
        (Vector3d vx vy vz) =
            Vector2d.placeOnto plane (Vector2d x y)

        (Point3d px py pz) =
            plane.originPoint
    in
        Point3d (px + vx) (py + vy) (pz + vz)


plus : Vector2d -> Point2d -> Point2d
plus (Vector2d vx vy) (Point2d px py) =
    Point2d (px + vx) (py + vy)


minus : Vector2d -> Point2d -> Point2d
minus (Vector2d vx vy) (Point2d px py) =
    Point2d (px - vx) (py - vy)


toRecord : Point2d -> { x : Float, y : Float }
toRecord (Point2d x y) =
    { x = x, y = y }


fromRecord : { x : Float, y : Float } -> Point2d
fromRecord { x, y } =
    Point2d x y
