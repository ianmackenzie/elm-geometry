{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Plane3d
    exposing
        ( xy
        , xz
        , yx
        , yz
        , zx
        , zy
        , fromPointAndNormal
        , offsetBy
        , flip
        , xAxis
        , yAxis
        , normalAxis
        , scaleAbout
        , rotateAround
        , translateBy
        , translateAlong
        , mirrorAcross
        , toLocalIn
        , fromLocalIn
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d


xy : Plane3d
xy =
    Plane3d Point3d.origin
        Direction3d.x
        Direction3d.y
        Direction3d.z


xz : Plane3d
xz =
    Plane3d Point3d.origin
        Direction3d.x
        Direction3d.z
        (Direction3d.negate Direction3d.y)


yx : Plane3d
yx =
    Plane3d Point3d.origin
        Direction3d.y
        Direction3d.x
        (Direction3d.negate Direction3d.z)


yz : Plane3d
yz =
    Plane3d Point3d.origin
        Direction3d.y
        Direction3d.z
        Direction3d.x


zx : Plane3d
zx =
    Plane3d Point3d.origin
        Direction3d.z
        Direction3d.x
        Direction3d.y


zy : Plane3d
zy =
    Plane3d Point3d.origin
        Direction3d.z
        Direction3d.y
        (Direction3d.negate Direction3d.x)


fromPointAndNormal : Point3d -> Direction3d -> Plane3d
fromPointAndNormal originPoint normalDirection =
    let
        ( xDirection, yDirection ) =
            Direction3d.normalBasis normalDirection
    in
        Plane3d originPoint xDirection yDirection normalDirection


offsetBy : Float -> Plane3d -> Plane3d
offsetBy distance plane =
    translateBy (Direction3d.times distance plane.normalDirection) plane


flip : Plane3d -> Plane3d
flip plane =
    { plane | normalDirection = Direction3d.negate plane.normalDirection }


xAxis : Plane3d -> Axis3d
xAxis plane =
    Axis3d plane.originPoint plane.xDirection


yAxis : Plane3d -> Axis3d
yAxis plane =
    Axis3d plane.originPoint plane.yDirection


normalAxis : Plane3d -> Axis3d
normalAxis plane =
    Axis3d plane.originPoint plane.normalDirection


scaleAbout : Point3d -> Float -> Plane3d -> Plane3d
scaleAbout centerPoint scale plane =
    let
        scaledOriginPoint =
            Point3d.scaleAbout centerPoint scale plane.originPoint
    in
        { plane | originPoint = scaledOriginPoint }


rotateAround : Axis3d -> Float -> Plane3d -> Plane3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \plane ->
            Plane3d (rotatePoint plane.originPoint)
                (rotateDirection plane.xDirection)
                (rotateDirection plane.yDirection)
                (rotateDirection plane.normalDirection)


translateBy : Vector3d -> Plane3d -> Plane3d
translateBy vector plane =
    { plane | originPoint = Point3d.plus vector plane.originPoint }


translateAlong : Axis3d -> Float -> Plane3d -> Plane3d
translateAlong axis distance =
    translateBy (Vector3d.along axis distance)


mirrorAcross : Plane3d -> Plane3d -> Plane3d
mirrorAcross otherPlane =
    let
        mirrorPoint =
            Point3d.mirrorAcross otherPlane

        mirrorDirection =
            Direction3d.mirrorAcross otherPlane
    in
        \plane ->
            Plane3d (mirrorPoint plane.originPoint)
                (mirrorDirection plane.xDirection)
                (mirrorDirection plane.yDirection)
                (mirrorDirection plane.normalDirection)


toLocalIn : Frame3d -> Plane3d -> Plane3d
toLocalIn frame =
    let
        toLocalPoint =
            Point3d.toLocalIn frame

        toLocalDirection =
            Direction3d.toLocalIn frame
    in
        \plane ->
            Plane3d (toLocalPoint plane.originPoint)
                (toLocalDirection plane.xDirection)
                (toLocalDirection plane.yDirection)
                (toLocalDirection plane.normalDirection)


fromLocalIn : Frame3d -> Plane3d -> Plane3d
fromLocalIn frame =
    let
        fromLocalPoint =
            Point3d.fromLocalIn frame

        fromLocalDirection =
            Direction3d.fromLocalIn frame
    in
        \plane ->
            Plane3d (fromLocalPoint plane.originPoint)
                (fromLocalDirection plane.xDirection)
                (fromLocalDirection plane.yDirection)
                (fromLocalDirection plane.normalDirection)
