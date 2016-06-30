{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Axis2d
    exposing
        ( x
        , y
        , perpendicularTo
        , originPoint
        , direction
        , scaleAbout
        , rotateAround
        , translateBy
        , translateIn
        , mirrorAcross
        , localizeTo
        , placeIn
        , placeOnto
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Direction2d as Direction2d


x : Axis2d
x =
    Axis2d { originPoint = Point2d.origin, direction = Direction2d.x }


y : Axis2d
y =
    Axis2d { originPoint = Point2d.origin, direction = Direction2d.y }


perpendicularTo : Axis2d -> Axis2d
perpendicularTo axis =
    Axis2d
        { originPoint = originPoint axis
        , direction = Direction2d.perpendicularTo (direction axis)
        }


originPoint : Axis2d -> Point2d
originPoint (Axis2d properties) =
    properties.originPoint


direction : Axis2d -> Direction2d
direction (Axis2d properties) =
    properties.direction


scaleAbout : Point2d -> Float -> Axis2d -> Axis2d
scaleAbout centerPoint scale axis =
    Axis2d
        { originPoint = Point2d.scaleAbout centerPoint scale (originPoint axis)
        , direction = direction axis
        }


rotateAround : Point2d -> Float -> Axis2d -> Axis2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
        \axis ->
            Axis2d
                { originPoint = rotatePoint (originPoint axis)
                , direction = rotateDirection (direction axis)
                }


translateBy : Vector2d -> Axis2d -> Axis2d
translateBy vector axis =
    Axis2d
        { originPoint = Point2d.translateBy vector (originPoint axis)
        , direction = direction axis
        }


translateIn : Direction2d -> Float -> Axis2d -> Axis2d
translateIn direction =
    translateBy << Vector2d.inDirection direction


mirrorAcross : Axis2d -> Axis2d -> Axis2d
mirrorAcross otherAxis =
    let
        mirrorPoint =
            Point2d.mirrorAcross otherAxis

        mirrorDirection =
            Direction2d.mirrorAcross otherAxis
    in
        \axis ->
            Axis2d
                { originPoint = mirrorPoint (originPoint axis)
                , direction = mirrorDirection (direction axis)
                }


localizeTo : Frame2d -> Axis2d -> Axis2d
localizeTo frame =
    let
        localizePoint =
            Point2d.localizeTo frame

        localizeDirection =
            Direction2d.localizeTo frame
    in
        \axis ->
            Axis2d
                { originPoint = localizePoint (originPoint axis)
                , direction = localizeDirection (direction axis)
                }


placeIn : Frame2d -> Axis2d -> Axis2d
placeIn frame =
    let
        placePoint =
            Point2d.placeIn frame

        placeDirection =
            Direction2d.placeIn frame
    in
        \axis ->
            Axis2d
                { originPoint = placePoint (originPoint axis)
                , direction = placeDirection (direction axis)
                }


placeOnto : Plane3d -> Axis2d -> Axis3d
placeOnto plane =
    let
        placePoint =
            Point2d.placeOnto plane

        placeDirection =
            Direction2d.placeOnto plane
    in
        \axis ->
            Axis3d
                { originPoint = placePoint (originPoint axis)
                , direction = placeDirection (direction axis)
                }
