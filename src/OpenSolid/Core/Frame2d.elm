{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Frame2d
    exposing
        ( xy
        , at
        , originPoint
        , xDirection
        , yDirection
        , xAxis
        , yAxis
        , scaleAbout
        , rotateAround
        , rotateAroundOwn
        , translateBy
        , translateIn
        , translateInOwn
        , mirrorAcross
        , mirrorAcrossOwn
        , localizeTo
        , placeIn
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d


xy : Frame2d
xy =
    at Point2d.origin


at : Point2d -> Frame2d
at point =
    Frame2d
        { originPoint = point
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


scaleAbout : Point2d -> Float -> Frame2d -> Frame2d
scaleAbout centerPoint scale frame =
    Frame2d
        { originPoint = Point2d.scaleAbout centerPoint scale (originPoint frame)
        , xDirection = xDirection frame
        , yDirection = yDirection frame
        }


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


translateIn : Direction2d -> Float -> Frame2d -> Frame2d
translateIn direction =
    translateBy << Vector2d.inDirection direction


translateInOwn : (Frame2d -> Direction2d) -> Float -> Frame2d -> Frame2d
translateInOwn direction distance frame =
    translateIn (direction frame) distance frame


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


localizeTo : Frame2d -> Frame2d -> Frame2d
localizeTo otherFrame =
    let
        localizePoint =
            Point2d.localizeTo otherFrame

        localizeDirection =
            Direction2d.localizeTo otherFrame
    in
        \frame ->
            Frame2d
                { originPoint = localizePoint (originPoint frame)
                , xDirection = localizeDirection (xDirection frame)
                , yDirection = localizeDirection (yDirection frame)
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
