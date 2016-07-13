{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Frame3d
    exposing
        ( xyz
        , originPoint
        , xDirection
        , yDirection
        , zDirection
        , xAxis
        , yAxis
        , zAxis
        , xyPlane
        , yxPlane
        , yzPlane
        , zyPlane
        , zxPlane
        , xzPlane
        , scaleAbout
        , rotateAround
        , rotateAroundOwn
        , translateBy
        , translateAlongOwn
        , translateTo
        , mirrorAcross
        , mirrorAcrossOwn
        , localizeTo
        , placeIn
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


xyz : Frame3d
xyz =
    Frame3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        , zDirection = Direction3d.z
        }


originPoint : Frame3d -> Point3d
originPoint (Frame3d properties) =
    properties.originPoint


xDirection : Frame3d -> Direction3d
xDirection (Frame3d properties) =
    properties.xDirection


yDirection : Frame3d -> Direction3d
yDirection (Frame3d properties) =
    properties.yDirection


zDirection : Frame3d -> Direction3d
zDirection (Frame3d properties) =
    properties.zDirection


xAxis : Frame3d -> Axis3d
xAxis frame =
    Axis3d { originPoint = originPoint frame, direction = xDirection frame }


yAxis : Frame3d -> Axis3d
yAxis frame =
    Axis3d { originPoint = originPoint frame, direction = yDirection frame }


zAxis : Frame3d -> Axis3d
zAxis frame =
    Axis3d { originPoint = originPoint frame, direction = zDirection frame }


xyPlane : Frame3d -> Plane3d
xyPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , normalDirection = zDirection frame
        }


yxPlane : Frame3d -> Plane3d
yxPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = xDirection frame
        , normalDirection = Direction3d.negate (zDirection frame)
        }


yzPlane : Frame3d -> Plane3d
yzPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , xDirection = yDirection frame
        , yDirection = zDirection frame
        , normalDirection = xDirection frame
        }


zyPlane : Frame3d -> Plane3d
zyPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = yDirection frame
        , normalDirection = Direction3d.negate (xDirection frame)
        }


zxPlane : Frame3d -> Plane3d
zxPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , xDirection = zDirection frame
        , yDirection = xDirection frame
        , normalDirection = yDirection frame
        }


xzPlane : Frame3d -> Plane3d
xzPlane frame =
    Plane3d
        { originPoint = originPoint frame
        , xDirection = xDirection frame
        , yDirection = zDirection frame
        , normalDirection = Direction3d.negate (yDirection frame)
        }


scaleAbout : Point3d -> Float -> Frame3d -> Frame3d
scaleAbout centerPoint scale frame =
    Frame3d
        { originPoint = Point3d.scaleAbout centerPoint scale (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


rotateAround : Axis3d -> Float -> Frame3d -> Frame3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \frame ->
            Frame3d
                { originPoint = rotatePoint (originPoint frame)
                , xDirection = rotateDirection (xDirection frame)
                , yDirection = rotateDirection (yDirection frame)
                , zDirection = rotateDirection (zDirection frame)
                }


rotateAroundOwn : (Frame3d -> Axis3d) -> Float -> Frame3d -> Frame3d
rotateAroundOwn axis angle frame =
    rotateAround (axis frame) angle frame


translateBy : Vector3d -> Frame3d -> Frame3d
translateBy vector frame =
    Frame3d
        { originPoint = Point3d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


translateAlongOwn : (Frame3d -> Axis3d) -> Float -> Frame3d -> Frame3d
translateAlongOwn axis distance frame =
    let
        displacement =
            Direction3d.times distance (Axis3d.direction (axis frame))
    in
        translateBy displacement frame


translateTo : Point3d -> Frame3d -> Frame3d
translateTo point frame =
    Frame3d
        { originPoint = point
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        , zDirection = zDirection frame
        }


mirrorAcross : Plane3d -> Frame3d -> Frame3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
        \frame ->
            Frame3d
                { originPoint = mirrorPoint (originPoint frame)
                , xDirection = mirrorDirection (xDirection frame)
                , yDirection = mirrorDirection (yDirection frame)
                , zDirection = mirrorDirection (zDirection frame)
                }


mirrorAcrossOwn : (Frame3d -> Plane3d) -> Frame3d -> Frame3d
mirrorAcrossOwn plane frame =
    mirrorAcross (plane frame) frame


localizeTo : Frame3d -> Frame3d -> Frame3d
localizeTo otherFrame =
    let
        localizePoint =
            Point3d.localizeTo otherFrame

        localizeDirection =
            Direction3d.localizeTo otherFrame
    in
        \frame ->
            Frame3d
                { originPoint = localizePoint (originPoint frame)
                , xDirection = localizeDirection (xDirection frame)
                , yDirection = localizeDirection (yDirection frame)
                , zDirection = localizeDirection (zDirection frame)
                }


placeIn : Frame3d -> Frame3d -> Frame3d
placeIn otherFrame =
    let
        placePoint =
            Point3d.placeIn otherFrame

        placeDirection =
            Direction3d.placeIn otherFrame
    in
        \frame ->
            Frame3d
                { originPoint = placePoint (originPoint frame)
                , xDirection = placeDirection (xDirection frame)
                , yDirection = placeDirection (yDirection frame)
                , zDirection = placeDirection (zDirection frame)
                }
