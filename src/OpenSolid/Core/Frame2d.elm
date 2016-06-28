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
        , xAxis
        , yAxis
        , scaleAbout
        , rotateAround
        , rotateAroundOwn
        , translateBy
        , translateAlong
        , translateAlongOwn
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


rotateAroundOwn : (Frame2d -> Point2d) -> Float -> Frame2d -> Frame2d
rotateAroundOwn centerPoint angle frame =
    rotateAround (centerPoint frame) angle frame


translateBy : Vector2d -> Frame2d -> Frame2d
translateBy vector frame =
    { frame | originPoint = Point2d.translateBy vector frame.originPoint }


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


localizeTo : Frame2d -> Frame2d -> Frame2d
localizeTo otherFrame =
    let
        localizePoint =
            Point2d.localizeTo otherFrame

        localizeDirection =
            Direction2d.localizeTo otherFrame
    in
        \frame ->
            Frame2d (localizePoint frame.originPoint)
                (localizeDirection frame.xDirection)
                (localizeDirection frame.yDirection)


placeIn : Frame2d -> Frame2d -> Frame2d
placeIn otherFrame =
    let
        placePoint =
            Point2d.placeIn otherFrame

        placeDirection =
            Direction2d.placeIn otherFrame
    in
        \frame ->
            Frame2d (placePoint frame.originPoint)
                (placeDirection frame.xDirection)
                (placeDirection frame.yDirection)
