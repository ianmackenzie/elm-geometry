{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Frame2d
    exposing
        ( xy
        , originPoint
        , xDirection
        , yDirection
        , xAxis
        , yAxis
        , rotateAround
        , rotateAroundOwn
        , translateBy
        , translateAlongOwn
        , moveTo
        , mirrorAcross
        , mirrorAcrossOwn
        , relativeTo
        , placeIn
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Axis2d as Axis2d


xy : Frame2d
xy =
    Frame2d
        { originPoint = Point2d.origin
        , xDirection = Direction2d.x
        , yDirection = Direction2d.y
        }


originPoint : Frame2d -> Point2d
originPoint (Frame2d properties) =
    properties.originPoint


xDirection : Frame2d -> Direction2d
xDirection (Frame2d properties) =
    properties.xDirection


yDirection : Frame2d -> Direction2d
yDirection (Frame2d properties) =
    properties.yDirection


xAxis : Frame2d -> Axis2d
xAxis frame =
    Axis2d { originPoint = originPoint frame, direction = xDirection frame }


yAxis : Frame2d -> Axis2d
yAxis frame =
    Axis2d { originPoint = originPoint frame, direction = yDirection frame }


rotateAround : Point2d -> Float -> Frame2d -> Frame2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
        \frame ->
            Frame2d
                { originPoint = rotatePoint (originPoint frame)
                , xDirection = rotateDirection (xDirection frame)
                , yDirection = rotateDirection (yDirection frame)
                }


rotateAroundOwn : (Frame2d -> Point2d) -> Float -> Frame2d -> Frame2d
rotateAroundOwn centerPoint angle frame =
    rotateAround (centerPoint frame) angle frame


translateBy : Vector2d -> Frame2d -> Frame2d
translateBy vector frame =
    Frame2d
        { originPoint = Point2d.translateBy vector (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


translateAlongOwn : (Frame2d -> Axis2d) -> Float -> Frame2d -> Frame2d
translateAlongOwn axis distance frame =
    let
        displacement =
            Direction2d.times distance (Axis2d.direction (axis frame))
    in
        translateBy displacement frame


moveTo : Point2d -> Frame2d -> Frame2d
moveTo newOrigin frame =
    Frame2d
        { originPoint = newOrigin
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


mirrorAcross : Axis2d -> Frame2d -> Frame2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis

        mirrorDirection =
            Direction2d.mirrorAcross axis
    in
        \frame ->
            Frame2d
                { originPoint = mirrorPoint (originPoint frame)
                , xDirection = mirrorDirection (xDirection frame)
                , yDirection = mirrorDirection (yDirection frame)
                }


mirrorAcrossOwn : (Frame2d -> Axis2d) -> Frame2d -> Frame2d
mirrorAcrossOwn axis frame =
    mirrorAcross (axis frame) frame


relativeTo : Frame2d -> Frame2d -> Frame2d
relativeTo otherFrame =
    let
        relativePoint =
            Point2d.relativeTo otherFrame

        relativeDirection =
            Direction2d.relativeTo otherFrame
    in
        \frame ->
            Frame2d
                { originPoint = relativePoint (originPoint frame)
                , xDirection = relativeDirection (xDirection frame)
                , yDirection = relativeDirection (yDirection frame)
                }


placeIn : Frame2d -> Frame2d -> Frame2d
placeIn otherFrame =
    let
        placePoint =
            Point2d.placeIn otherFrame

        placeDirection =
            Direction2d.placeIn otherFrame
    in
        \frame ->
            Frame2d
                { originPoint = placePoint (originPoint frame)
                , xDirection = placeDirection (xDirection frame)
                , yDirection = placeDirection (yDirection frame)
                }
