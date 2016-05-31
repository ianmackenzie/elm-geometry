{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Axis3d
    exposing
        ( x
        , y
        , z
        , point
        , normalDirection
        , normalPlane
        , reverse
        , scaleAbout
        , rotateAbout
        , translateBy
        , mirrorAbout
        , toLocalIn
        , toGlobalFrom
        , projectOnto
        , projectInto
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Plane3d as Plane3d


x : Axis3d
x =
    Axis3d Point3d.origin Direction3d.x


y : Axis3d
y =
    Axis3d Point3d.origin Direction3d.y


z : Axis3d
z =
    Axis3d Point3d.origin Direction3d.z


point : Float -> Axis3d -> Point3d
point distance axis =
    Point3d.plus (Direction3d.times distance axis.direction) axis.originPoint


normalDirection : Axis3d -> Direction3d
normalDirection axis =
    Direction3d.perpendicularDirection axis.direction


normalPlane : Axis3d -> Plane3d
normalPlane axis =
    Plane3d.fromPointAndNormal axis.originPoint axis.direction


reverse : Axis3d -> Axis3d
reverse axis =
    Axis3d axis.originPoint (Direction3d.negate axis.direction)


scaleAbout : Point3d -> Float -> Axis3d -> Axis3d
scaleAbout centerPoint scale axis =
    let
        scalePoint =
            Point3d.scaleAbout centerPoint scale
    in
        Axis3d (scalePoint axis.originPoint) axis.direction


rotateAbout : Axis3d -> Float -> Axis3d -> Axis3d
rotateAbout otherAxis angle =
    let
        rotatePoint =
            Point3d.rotateAbout otherAxis angle

        rotateDirection =
            Direction3d.rotateAbout otherAxis.direction angle
    in
        \axis ->
            Axis3d (rotatePoint axis.originPoint)
                (rotateDirection axis.direction)


translateBy : Vector3d -> Axis3d -> Axis3d
translateBy vector axis =
    Axis3d (Point3d.plus vector axis.originPoint) axis.direction


mirrorAbout : Plane3d -> Axis3d -> Axis3d
mirrorAbout plane =
    let
        mirrorPoint =
            Point3d.mirrorAbout plane

        mirrorDirection =
            Direction3d.mirrorAlong plane.normalDirection
    in
        \axis ->
            Axis3d (mirrorPoint axis.originPoint)
                (mirrorDirection axis.direction)


toLocalIn : Frame3d -> Axis3d -> Axis3d
toLocalIn frame =
    let
        localizePoint =
            Point3d.toLocalIn frame

        localizeDirection =
            Direction3d.toLocalIn frame
    in
        \axis ->
            Axis3d (localizePoint axis.originPoint)
                (localizeDirection axis.direction)


toGlobalFrom : Frame3d -> Axis3d -> Axis3d
toGlobalFrom frame =
    let
        globalizePoint =
            Point3d.toGlobalFrom frame

        globalizeDirection =
            Direction3d.toGlobalFrom frame
    in
        \axis ->
            Axis3d (globalizePoint axis.originPoint)
                (globalizeDirection axis.direction)


projectOnto : Plane3d -> Axis3d -> Maybe Axis3d
projectOnto plane axis =
    let
        projectedOrigin =
            Point3d.projectOnto plane axis.originPoint
    in
        Maybe.map (Axis3d projectedOrigin)
            (Direction3d.projectOnto plane axis.direction)


projectInto : Plane3d -> Axis3d -> Maybe Axis2d
projectInto plane axis =
    let
        projectedOrigin =
            Point3d.projectInto plane axis.originPoint
    in
        Maybe.map (Axis2d projectedOrigin)
            (Direction3d.projectInto plane axis.direction)
