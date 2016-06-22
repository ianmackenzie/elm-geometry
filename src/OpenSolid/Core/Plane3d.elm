{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Plane3d
    exposing
        ( xy
        , yx
        , yz
        , zy
        , zx
        , xz
        , fromPointAndNormal
        , offsetBy
        , flip
        , xAxis
        , yAxis
        , normalAxis
        , scaleAbout
        , rotateAround
        , rotateAroundOwn
        , translateBy
        , translateAlong
        , translateAlongOwn
        , mirrorAcross
        , localizeTo
        , placeIn
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


xy : Plane3d
xy =
    Plane3d Point3d.origin
        Direction3d.x
        Direction3d.y
        Direction3d.z


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


zy : Plane3d
zy =
    Plane3d Point3d.origin
        Direction3d.z
        Direction3d.y
        (Direction3d.negate Direction3d.x)


zx : Plane3d
zx =
    Plane3d Point3d.origin
        Direction3d.z
        Direction3d.x
        Direction3d.y


xz : Plane3d
xz =
    Plane3d Point3d.origin
        Direction3d.x
        Direction3d.z
        (Direction3d.negate Direction3d.y)


fromPointAndNormal : Point3d -> Direction3d -> Plane3d
fromPointAndNormal originPoint normalDirection =
    let
        xDirection =
            Direction3d.perpendicularTo normalDirection

        yDirectionVector =
            Direction3d.crossProduct normalDirection xDirection

        yDirection =
            Direction3d (Vector3d.components yDirectionVector)
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


rotateAroundOwn : (Plane3d -> Axis3d) -> Float -> Plane3d -> Plane3d
rotateAroundOwn axis angle plane =
    rotateAround (axis plane) angle plane


translateBy : Vector3d -> Plane3d -> Plane3d
translateBy vector plane =
    { plane | originPoint = Point3d.plus vector plane.originPoint }


translateAlong : Axis3d -> Float -> Plane3d -> Plane3d
translateAlong axis distance =
    translateBy (Vector3d.alongAxis axis distance)


translateAlongOwn : (Plane3d -> Axis3d) -> Float -> Plane3d -> Plane3d
translateAlongOwn axis distance plane =
    translateAlong (axis plane) distance plane


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


localizeTo : Frame3d -> Plane3d -> Plane3d
localizeTo frame =
    let
        localizePoint =
            Point3d.localizeTo frame

        localizeDirection =
            Direction3d.localizeTo frame
    in
        \plane ->
            Plane3d (localizePoint plane.originPoint)
                (localizeDirection plane.xDirection)
                (localizeDirection plane.yDirection)
                (localizeDirection plane.normalDirection)


placeIn : Frame3d -> Plane3d -> Plane3d
placeIn frame =
    let
        placePoint =
            Point3d.placeIn frame

        placeDirection =
            Direction3d.placeIn frame
    in
        \plane ->
            Plane3d (placePoint plane.originPoint)
                (placeDirection plane.xDirection)
                (placeDirection plane.yDirection)
                (placeDirection plane.normalDirection)
