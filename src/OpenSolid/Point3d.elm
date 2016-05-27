{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Point3d
    exposing
        ( origin
        , fromComponents
        , xComponent
        , yComponent
        , zComponent
        , components
        , squaredDistanceTo
        , distanceTo
        , vectorTo
        , vectorFrom
        , distanceAlong
        , squaredDistanceToAxis
        , distanceToAxis
        , signedDistanceFrom
        , scaleAbout
        , rotateAbout
        , mirrorAbout
        , relativeTo
        , placeIn
        , projectOntoAxis
        , projectOnto
        , projectInto
        , plus
        , minus
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d


origin : Point3d
origin =
    Point3d 0 0 0


fromComponents : ( Float, Float, Float ) -> Point3d
fromComponents ( x, y, z ) =
    Point3d x y z


xComponent : Point3d -> Float
xComponent (Point3d x _ _) =
    x


yComponent : Point3d -> Float
yComponent (Point3d _ y _) =
    y


zComponent : Point3d -> Float
zComponent (Point3d _ _ z) =
    z


components : Point3d -> ( Float, Float, Float )
components (Point3d x y z) =
    ( x, y, z )


squaredDistanceTo : Point3d -> Point3d -> Float
squaredDistanceTo other =
    vectorTo other >> Vector3d.squaredLength


distanceTo : Point3d -> Point3d -> Float
distanceTo other =
    squaredDistanceTo other >> sqrt


vectorTo : Point3d -> Point3d -> Vector3d
vectorTo (Point3d x2 y2 z2) (Point3d x1 y1 z1) =
    Vector3d (x2 - x1) (y2 - y1) (z2 - z1)


vectorFrom : Point3d -> Point3d -> Vector3d
vectorFrom (Point3d x2 y2 z2) (Point3d x1 y1 z1) =
    Vector3d (x1 - x2) (y1 - y2) (z1 - z2)


distanceAlong : Axis3d -> Point3d -> Float
distanceAlong axis =
    vectorFrom axis.originPoint >> Vector3d.componentIn axis.direction


squaredDistanceToAxis : Axis3d -> Point3d -> Float
squaredDistanceToAxis axis =
    let
        directionVector =
            Direction3d.vector axis.direction
    in
        vectorFrom axis.originPoint
            >> Vector3d.cross directionVector
            >> Vector3d.squaredLength


distanceToAxis : Axis3d -> Point3d -> Float
distanceToAxis axis =
    squaredDistanceToAxis axis >> sqrt


signedDistanceFrom : Plane3d -> Point3d -> Float
signedDistanceFrom plane =
    vectorFrom plane.originPoint >> Vector3d.componentIn plane.normalDirection


scaleAbout : Point3d -> Float -> Point3d -> Point3d
scaleAbout centerPoint scale =
    vectorFrom centerPoint >> Vector3d.times scale >> Vector3d.addTo centerPoint


rotateAbout : Axis3d -> Float -> Point3d -> Point3d
rotateAbout axis angle =
    let
        rotateVector =
            Vector3d.rotateAbout axis.direction angle
    in
        vectorFrom axis.originPoint
            >> rotateVector
            >> Vector3d.addTo axis.originPoint


mirrorAbout : Plane3d -> Point3d -> Point3d
mirrorAbout plane =
    let
        mirrorVector =
            Vector3d.mirrorAlong plane.normalDirection
    in
        vectorFrom plane.originPoint
            >> mirrorVector
            >> Vector3d.addTo plane.originPoint


relativeTo : Frame3d -> Point3d -> Point3d
relativeTo frame =
    let
        localizeVector =
            Vector3d.relativeTo frame
    in
        vectorFrom frame.originPoint
            >> localizeVector
            >> (\(Vector3d x y z) -> Point3d x y z)


placeIn : Frame3d -> Point3d -> Point3d
placeIn frame =
    let
        globalizeVector =
            Vector3d.placeIn frame
    in
        (\(Point3d x y z) -> Vector3d x y z)
            >> globalizeVector
            >> Vector3d.addTo frame.originPoint


projectOntoAxis : Axis3d -> Point3d -> Point3d
projectOntoAxis axis =
    vectorFrom axis.originPoint
        >> Vector3d.projectionIn axis.direction
        >> Vector3d.addTo axis.originPoint


projectOnto : Plane3d -> Point3d -> Point3d
projectOnto plane point =
    let
        distance =
            signedDistanceFrom plane point

        displacement =
            Direction3d.times distance plane.normalDirection
    in
        minus displacement point


projectInto : Plane3d -> Point3d -> Point2d
projectInto plane =
    vectorFrom plane.originPoint
        >> Vector3d.projectInto plane
        >> (\(Vector2d x y) -> Point2d x y)


plus : Vector3d -> Point3d -> Point3d
plus (Vector3d vx vy vz) (Point3d px py pz) =
    Point3d (px + vx) (py + vy) (pz + vz)


minus : Vector3d -> Point3d -> Point3d
minus (Vector3d vx vy vz) (Point3d px py pz) =
    Point3d (px - vx) (py - vy) (pz - vz)
