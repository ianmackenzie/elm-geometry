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
        , originPoint
        , normalDirection
        , sketchPlane
        , offsetBy
        , flip
        , normalAxis
        , rotateAround
        , translateBy
        , moveTo
        , mirrorAcross
        , relativeTo
        , placeIn
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction3d as Direction3d


xy : Plane3d
xy =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.z
        }


yx : Plane3d
yx =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.negate Direction3d.z
        }


yz : Plane3d
yz =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.x
        }


zy : Plane3d
zy =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.negate Direction3d.x
        }


zx : Plane3d
zx =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.y
        }


xz : Plane3d
xz =
    Plane3d
        { originPoint = Point3d.origin
        , normalDirection = Direction3d.negate Direction3d.y
        }


originPoint : Plane3d -> Point3d
originPoint (Plane3d properties) =
    properties.originPoint


normalDirection : Plane3d -> Direction3d
normalDirection (Plane3d properties) =
    properties.normalDirection


sketchPlane : Plane3d -> SketchPlane3d
sketchPlane plane =
    let
        normal =
            normalDirection plane

        xDirection =
            Direction3d.perpendicularTo normal

        yDirectionVector =
            Direction3d.crossProduct normal xDirection

        yDirection =
            Direction3d (Vector3d.components yDirectionVector)
    in
        SketchPlane3d
            { originPoint = originPoint plane
            , xDirection = xDirection
            , yDirection = yDirection
            }


offsetBy : Float -> Plane3d -> Plane3d
offsetBy distance plane =
    translateBy (Direction3d.times distance (normalDirection plane)) plane


flip : Plane3d -> Plane3d
flip plane =
    Plane3d
        { originPoint = originPoint plane
        , normalDirection = Direction3d.negate (normalDirection plane)
        }


normalAxis : Plane3d -> Axis3d
normalAxis plane =
    Axis3d
        { originPoint = originPoint plane
        , direction = normalDirection plane
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
                , normalDirection = rotateDirection (normalDirection plane)
                }


translateBy : Vector3d -> Plane3d -> Plane3d
translateBy vector plane =
    Plane3d
        { originPoint = Point3d.translateBy vector (originPoint plane)
        , normalDirection = normalDirection plane
        }


moveTo : Point3d -> Plane3d -> Plane3d
moveTo newOrigin plane =
    Plane3d
        { originPoint = newOrigin
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
                , normalDirection = mirrorDirection (normalDirection plane)
                }


relativeTo : Frame3d -> Plane3d -> Plane3d
relativeTo frame =
    let
        relativePoint =
            Point3d.relativeTo frame

        relativeDirection =
            Direction3d.relativeTo frame
    in
        \plane ->
            Plane3d
                { originPoint = relativePoint (originPoint plane)
                , normalDirection = relativeDirection (normalDirection plane)
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
                , normalDirection = placeDirection (normalDirection plane)
                }
