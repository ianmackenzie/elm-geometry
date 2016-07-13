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
        , originPoint
        , xDirection
        , yDirection
        , normalDirection
        , offsetBy
        , flip
        , xAxis
        , yAxis
        , normalAxis
        , scaleAbout
        , rotateAround
        , rotateAroundOwn
        , translateBy
        , translateAlongOwn
        , translateTo
        , mirrorAcross
        , localizeTo
        , placeIn
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Axis3d as Axis3d


xy : Plane3d
xy =
    Plane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        , normalDirection = Direction3d.z
        }


yx : Plane3d
yx =
    Plane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.x
        , normalDirection = Direction3d.negate Direction3d.z
        }


yz : Plane3d
yz =
    Plane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        , normalDirection = Direction3d.x
        }


zy : Plane3d
zy =
    Plane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.y
        , normalDirection = Direction3d.negate Direction3d.x
        }


zx : Plane3d
zx =
    Plane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.x
        , normalDirection = Direction3d.y
        }


xz : Plane3d
xz =
    Plane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.z
        , normalDirection = Direction3d.negate Direction3d.y
        }


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
        Plane3d
            { originPoint = originPoint
            , xDirection = xDirection
            , yDirection = yDirection
            , normalDirection = normalDirection
            }


originPoint : Plane3d -> Point3d
originPoint (Plane3d properties) =
    properties.originPoint


xDirection : Plane3d -> Direction3d
xDirection (Plane3d properties) =
    properties.xDirection


yDirection : Plane3d -> Direction3d
yDirection (Plane3d properties) =
    properties.yDirection


normalDirection : Plane3d -> Direction3d
normalDirection (Plane3d properties) =
    properties.normalDirection


offsetBy : Float -> Plane3d -> Plane3d
offsetBy =
    translateAlongOwn normalAxis


flip : Plane3d -> Plane3d
flip plane =
    Plane3d
        { originPoint = originPoint plane
        , xDirection = xDirection plane
        , yDirection = yDirection plane
        , normalDirection = Direction3d.negate (normalDirection plane)
        }


xAxis : Plane3d -> Axis3d
xAxis plane =
    Axis3d { originPoint = originPoint plane, direction = xDirection plane }


yAxis : Plane3d -> Axis3d
yAxis plane =
    Axis3d { originPoint = originPoint plane, direction = yDirection plane }


normalAxis : Plane3d -> Axis3d
normalAxis plane =
    Axis3d
        { originPoint = originPoint plane
        , direction = normalDirection plane
        }


scaleAbout : Point3d -> Float -> Plane3d -> Plane3d
scaleAbout centerPoint scale plane =
    Plane3d
        { originPoint = Point3d.scaleAbout centerPoint scale (originPoint plane)
        , xDirection = xDirection plane
        , yDirection = yDirection plane
        , normalDirection = normalDirection plane
        }


rotateAround : Axis3d -> Float -> Plane3d -> Plane3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \plane ->
            Plane3d
                { originPoint = rotatePoint (originPoint plane)
                , xDirection = rotateDirection (xDirection plane)
                , yDirection = rotateDirection (yDirection plane)
                , normalDirection = rotateDirection (normalDirection plane)
                }


rotateAroundOwn : (Plane3d -> Axis3d) -> Float -> Plane3d -> Plane3d
rotateAroundOwn axis angle plane =
    rotateAround (axis plane) angle plane


translateBy : Vector3d -> Plane3d -> Plane3d
translateBy vector plane =
    Plane3d
        { originPoint = Point3d.translateBy vector (originPoint plane)
        , xDirection = xDirection plane
        , yDirection = yDirection plane
        , normalDirection = normalDirection plane
        }


translateAlongOwn : (Plane3d -> Axis3d) -> Float -> Plane3d -> Plane3d
translateAlongOwn axis distance plane =
    let
        displacement =
            Direction3d.times distance (Axis3d.direction (axis plane))
    in
        translateBy displacement plane


translateTo : Point3d -> Plane3d -> Plane3d
translateTo point plane =
    Plane3d
        { originPoint = point
        , xDirection = xDirection plane
        , yDirection = yDirection plane
        , normalDirection = normalDirection plane
        }


mirrorAcross : Plane3d -> Plane3d -> Plane3d
mirrorAcross otherPlane =
    let
        mirrorPoint =
            Point3d.mirrorAcross otherPlane

        mirrorDirection =
            Direction3d.mirrorAcross otherPlane
    in
        \plane ->
            Plane3d
                { originPoint = mirrorPoint (originPoint plane)
                , xDirection = mirrorDirection (xDirection plane)
                , yDirection = mirrorDirection (yDirection plane)
                , normalDirection = mirrorDirection (normalDirection plane)
                }


localizeTo : Frame3d -> Plane3d -> Plane3d
localizeTo frame =
    let
        localizePoint =
            Point3d.localizeTo frame

        localizeDirection =
            Direction3d.localizeTo frame
    in
        \plane ->
            Plane3d
                { originPoint = localizePoint (originPoint plane)
                , xDirection = localizeDirection (xDirection plane)
                , yDirection = localizeDirection (yDirection plane)
                , normalDirection = localizeDirection (normalDirection plane)
                }


placeIn : Frame3d -> Plane3d -> Plane3d
placeIn frame =
    let
        placePoint =
            Point3d.placeIn frame

        placeDirection =
            Direction3d.placeIn frame
    in
        \plane ->
            Plane3d
                { originPoint = placePoint (originPoint plane)
                , xDirection = placeDirection (xDirection plane)
                , yDirection = placeDirection (yDirection plane)
                , normalDirection = placeDirection (normalDirection plane)
                }
