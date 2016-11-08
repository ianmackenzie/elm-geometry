--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module OpenSolid.SketchPlane3d
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
        , flipX
        , flipY
        , moveTo
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        )

{-| Various functions for creating and working with `SketchPlane3d` values.
Sketch planes are defined by an origin point and X and Y basis directions in 3D,
which define a 2D coordinate system (the X and Y basis directions are always
perpendicular). A sketch plane can therefore be used to convert between 2D and
3D coordinates, such as taking 2D lines and placing them on a 3D plane, or
projecting a 3D point into a 2D sketch.

Sketch planes can be constructed explicitly by passing a record with
`originPoint`, `xDirection` and `yDirection` fields to the `SketchPlane3d`
constructor, for example

    sketchPlane =
        SketchPlane3d
            { originPoint = Point3d ( 2, 1, 3 )
            , xDirection = Direction3d.y
            , yDirection = Direction3d.negate Direction3d.z
            }

If you construct a `SketchPlane3d` this way, you are responsible for ensuring
that the X and Y basis directions are perpendicular to each other.
-}

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d


xy : SketchPlane3d
xy =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.y
        }


yx : SketchPlane3d
yx =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.x
        }


yz : SketchPlane3d
yz =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        }


zy : SketchPlane3d
zy =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.y
        }


zx : SketchPlane3d
zx =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.z
        , yDirection = Direction3d.x
        }


xz : SketchPlane3d
xz =
    SketchPlane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.x
        , yDirection = Direction3d.z
        }


originPoint : SketchPlane3d -> Point3d
originPoint (SketchPlane3d properties) =
    properties.originPoint


xDirection : SketchPlane3d -> Direction3d
xDirection (SketchPlane3d properties) =
    properties.xDirection


yDirection : SketchPlane3d -> Direction3d
yDirection (SketchPlane3d properties) =
    properties.yDirection


xAxis : SketchPlane3d -> Axis3d
xAxis sketchPlane =
    Axis3d
        { originPoint = originPoint sketchPlane
        , direction = xDirection sketchPlane
        }


yAxis : SketchPlane3d -> Axis3d
yAxis sketchPlane =
    Axis3d
        { originPoint = originPoint sketchPlane
        , direction = yDirection sketchPlane
        }


plane : SketchPlane3d -> Plane3d
plane sketchPlane =
    let
        normalVector =
            Direction3d.crossProduct (xDirection sketchPlane)
                (yDirection sketchPlane)

        normalDirection =
            Direction3d (Vector3d.components normalVector)
    in
        Plane3d
            { originPoint = originPoint sketchPlane
            , normalDirection = normalDirection
            }


flipX : SketchPlane3d -> SketchPlane3d
flipX sketchPlane =
    SketchPlane3d
        { originPoint = originPoint sketchPlane
        , xDirection = Direction3d.negate (xDirection sketchPlane)
        , yDirection = yDirection sketchPlane
        }


flipY : SketchPlane3d -> SketchPlane3d
flipY sketchPlane =
    SketchPlane3d
        { originPoint = originPoint sketchPlane
        , xDirection = xDirection sketchPlane
        , yDirection = Direction3d.negate (yDirection sketchPlane)
        }


moveTo : Point3d -> SketchPlane3d -> SketchPlane3d
moveTo newOrigin sketchPlane =
    SketchPlane3d
        { originPoint = newOrigin
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


rotateAround : Axis3d -> Float -> SketchPlane3d -> SketchPlane3d
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateDirection =
            Direction3d.rotateAround axis angle
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = rotatePoint (originPoint sketchPlane)
                , xDirection = rotateDirection (xDirection sketchPlane)
                , yDirection = rotateDirection (yDirection sketchPlane)
                }


translateBy : Vector3d -> SketchPlane3d -> SketchPlane3d
translateBy vector sketchPlane =
    SketchPlane3d
        { originPoint = Point3d.translateBy vector (originPoint sketchPlane)
        , xDirection = xDirection sketchPlane
        , yDirection = yDirection sketchPlane
        }


mirrorAcross : Plane3d -> SketchPlane3d -> SketchPlane3d
mirrorAcross plane =
    let
        mirrorPoint =
            Point3d.mirrorAcross plane

        mirrorDirection =
            Direction3d.mirrorAcross plane
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = mirrorPoint (originPoint sketchPlane)
                , xDirection = mirrorDirection (xDirection sketchPlane)
                , yDirection = mirrorDirection (yDirection sketchPlane)
                }


relativeTo : Frame3d -> SketchPlane3d -> SketchPlane3d
relativeTo frame =
    let
        relativePoint =
            Point3d.relativeTo frame

        relativeDirection =
            Direction3d.relativeTo frame
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = relativePoint (originPoint sketchPlane)
                , xDirection = relativeDirection (xDirection sketchPlane)
                , yDirection = relativeDirection (yDirection sketchPlane)
                }


placeIn : Frame3d -> SketchPlane3d -> SketchPlane3d
placeIn frame =
    let
        placePoint =
            Point3d.placeIn frame

        placeDirection =
            Direction3d.placeIn frame
    in
        \sketchPlane ->
            SketchPlane3d
                { originPoint = placePoint (originPoint sketchPlane)
                , xDirection = placeDirection (xDirection sketchPlane)
                , yDirection = placeDirection (yDirection sketchPlane)
                }
