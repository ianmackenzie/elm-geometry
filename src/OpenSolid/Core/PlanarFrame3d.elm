{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.PlanarFrame3d
    exposing
        ( xy
        , yx
        , yz
        , zy
        , zx
        , xz
        , originPoint
        , xDirection
        , yDirection
        , xAxis
        , yAxis
        , plane
        , scaleAbout
        , rotateAround
        , rotateAroundOwn
        , translateBy
        , translateAlongOwn
        , moveTo
        , mirrorAcross
        , relativeTo
        , placeIn
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Axis3d as Axis3d


xy : PlanarFrame3d
xy =
    PlanarFrame3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        }


yx : PlanarFrame3d
yx =
    PlanarFrame3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.x
        }


yz : PlanarFrame3d
yz =
    PlanarFrame3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        }


zy : PlanarFrame3d
zy =
    PlanarFrame3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.y
        }


zx : PlanarFrame3d
zx =
    PlanarFrame3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.x
        }


xz : PlanarFrame3d
xz =
    PlanarFrame3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.z
        }


originPoint : PlanarFrame3d -> Point3d
originPoint (PlanarFrame3d properties) =
    properties.originPoint


xDirection : PlanarFrame3d -> Direction3d
xDirection (PlanarFrame3d properties) =
    properties.xDirection


yDirection : PlanarFrame3d -> Direction3d
yDirection (PlanarFrame3d properties) =
    properties.yDirection


xAxis : PlanarFrame3d -> Axis3d
xAxis planarFrame =
    Axis3d
        { originPoint = originPoint planarFrame
        , direction = xDirection planarFrame
        }


yAxis : PlanarFrame3d -> Axis3d
yAxis planarFrame =
    Axis3d
        { originPoint = originPoint planarFrame
        , direction = yDirection planarFrame
        }


plane : PlanarFrame3d -> Plane3d
plane planarFrame =
    let
        normalVector =
            Direction3d.crossProduct (xDirection planarFrame)
                (yDirection planarFrame)

        normalDirection =
            Direction3d (Vector3d.components normalVector)
    in
        Plane3d
            { originPoint = originPoint planarFrame
            , normalDirection = normalDirection
            }


scaleAbout : Point3d -> Float -> PlanarFrame3d -> PlanarFrame3d
scaleAbout centerPoint scale planarFrame =
    PlanarFrame3d
        { originPoint =
            Point3d.scaleAbout centerPoint scale (originPoint planarFrame)
        , xDirection = xDirection planarFrame
        , yDirection = yDirection planarFrame
        }


rotateAround : Axis3d -> Float -> PlanarFrame3d -> PlanarFrame3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \planarFrame ->
            PlanarFrame3d
                { originPoint = rotatePoint (originPoint planarFrame)
                , xDirection = rotateDirection (xDirection planarFrame)
                , yDirection = rotateDirection (yDirection planarFrame)
                }


rotateAroundOwn : (PlanarFrame3d -> Axis3d) -> Float -> PlanarFrame3d -> PlanarFrame3d
rotateAroundOwn axis angle planarFrame =
    rotateAround (axis planarFrame) angle planarFrame


translateBy : Vector3d -> PlanarFrame3d -> PlanarFrame3d
translateBy vector planarFrame =
    PlanarFrame3d
        { originPoint = Point3d.translateBy vector (originPoint planarFrame)
        , xDirection = xDirection planarFrame
        , yDirection = yDirection planarFrame
        }


translateAlongOwn : (PlanarFrame3d -> Axis3d) -> Float -> PlanarFrame3d -> PlanarFrame3d
translateAlongOwn axis distance planarFrame =
    let
        axisDirection =
            Axis3d.direction (axis planarFrame)

        displacement =
            Direction3d.times distance axisDirection
    in
        translateBy displacement planarFrame


moveTo : Point3d -> PlanarFrame3d -> PlanarFrame3d
moveTo newOrigin planarFrame =
    PlanarFrame3d
        { originPoint = newOrigin
        , xDirection = xDirection planarFrame
        , yDirection = yDirection planarFrame
        }


mirrorAcross : Plane3d -> PlanarFrame3d -> PlanarFrame3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
        \planarFrame ->
            PlanarFrame3d
                { originPoint = mirrorPoint (originPoint planarFrame)
                , xDirection = mirrorDirection (xDirection planarFrame)
                , yDirection = mirrorDirection (yDirection planarFrame)
                }


relativeTo : Frame3d -> PlanarFrame3d -> PlanarFrame3d
relativeTo frame =
    let
        relativePoint =
            Point3d.relativeTo frame

        relativeDirection =
            Direction3d.relativeTo frame
    in
        \planarFrame ->
            PlanarFrame3d
                { originPoint = relativePoint (originPoint planarFrame)
                , xDirection = relativeDirection (xDirection planarFrame)
                , yDirection = relativeDirection (yDirection planarFrame)
                }


placeIn : Frame3d -> PlanarFrame3d -> PlanarFrame3d
placeIn frame =
    let
        placePoint =
            Point3d.placeIn frame

        placeDirection =
            Direction3d.placeIn frame
    in
        \planarFrame ->
            PlanarFrame3d
                { originPoint = placePoint (originPoint planarFrame)
                , xDirection = placeDirection (xDirection planarFrame)
                , yDirection = placeDirection (yDirection planarFrame)
                }
