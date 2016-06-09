{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Frame2d
    exposing
        ( xy
        , atPoint
        , xAxis
        , yAxis
        , scaleAbout
        , rotateBy
        , rotateAround
        , translateBy
        , translateAlong
        , translateAlongOwn
        , mirrorAcross
        , mirrorAcrossOwn
        , toLocalIn
        , fromLocalIn
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Direction2d as Direction2d


xy : Frame2d
xy =
    atPoint Point2d.origin


atPoint : Point2d -> Frame2d
atPoint point =
    Frame2d point Direction2d.x Direction2d.y


xAxis : Frame2d -> Axis2d
xAxis frame =
    Axis2d frame.originPoint frame.xDirection


yAxis : Frame2d -> Axis2d
yAxis frame =
    Axis2d frame.originPoint frame.yDirection


scaleAbout : Point2d -> Float -> Frame2d -> Frame2d
scaleAbout centerPoint scale frame =
    let
        scaledOriginPoint =
            Point2d.scaleAbout centerPoint scale frame.originPoint
    in
        { frame | originPoint = scaledOriginPoint }


rotateBy : Float -> Frame2d -> Frame2d
rotateBy angle =
    let
        rotateDirection =
            Direction2d.rotateBy angle
    in
        \frame ->
            Frame2d frame.originPoint
                (rotateDirection frame.xDirection)
                (rotateDirection frame.yDirection)


rotateAround : Point2d -> Float -> Frame2d -> Frame2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
        \frame ->
            Frame2d (rotatePoint frame.originPoint)
                (rotateDirection frame.xDirection)
                (rotateDirection frame.yDirection)


translateBy : Vector2d -> Frame2d -> Frame2d
translateBy vector frame =
    { frame | originPoint = Point2d.plus vector frame.originPoint }


translateAlong : Axis2d -> Float -> Frame2d -> Frame2d
translateAlong axis distance =
    translateBy (Vector2d.alongAxis axis distance)


translateAlongOwn : (Frame2d -> Axis2d) -> Float -> Frame2d -> Frame2d
translateAlongOwn axis distance frame =
    translateAlong (axis frame) distance frame


mirrorAcross : Axis2d -> Frame2d -> Frame2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis

        mirrorDirection =
            Direction2d.mirrorAcross axis
    in
        \frame ->
            Frame2d (mirrorPoint frame.originPoint)
                (mirrorDirection frame.xDirection)
                (mirrorDirection frame.yDirection)


mirrorAcrossOwn : (Frame2d -> Axis2d) -> Frame2d -> Frame2d
mirrorAcrossOwn axis frame =
    mirrorAcross (axis frame) frame


toLocalIn : Frame2d -> Frame2d -> Frame2d
toLocalIn otherFrame =
    let
        toLocalPoint =
            Point2d.toLocalIn otherFrame

        toLocalDirection =
            Direction2d.toLocalIn otherFrame
    in
        \frame ->
            Frame2d (toLocalPoint frame.originPoint)
                (toLocalDirection frame.xDirection)
                (toLocalDirection frame.yDirection)


fromLocalIn : Frame2d -> Frame2d -> Frame2d
fromLocalIn frame =
    let
        fromLocalPoint =
            Point2d.fromLocalIn frame

        fromLocalDirection =
            Direction2d.fromLocalIn frame
    in
        \plane ->
            Frame2d (fromLocalPoint plane.originPoint)
                (fromLocalDirection plane.xDirection)
                (fromLocalDirection plane.yDirection)
