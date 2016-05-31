{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Frame2d
    exposing
        ( global
        , xAxis
        , yAxis
        , point
        , vector
        , scaleAbout
        , rotateAbout
        , translateBy
        , mirrorAcross
        , toLocalIn
        , toGlobalFrom
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Direction2d as Direction2d


global : Frame2d
global =
    Frame2d Point2d.origin Direction2d.x Direction2d.y


xAxis : Frame2d -> Axis2d
xAxis frame =
    Axis2d frame.originPoint frame.xDirection


yAxis : Frame2d -> Axis2d
yAxis frame =
    Axis2d frame.originPoint frame.yDirection


point : Float -> Float -> Frame2d -> Point2d
point x y frame =
    Point2d.plus (vector x y frame) frame.originPoint


vector : Float -> Float -> Frame2d -> Vector2d
vector x y frame =
    let
        xVector =
            Direction2d.times x frame.xDirection

        yVector =
            Direction2d.times y frame.yDirection
    in
        Vector2d.plus yVector xVector


scaleAbout : Point2d -> Float -> Frame2d -> Frame2d
scaleAbout centerPoint scale frame =
    let
        scaledOriginPoint =
            Point2d.scaleAbout centerPoint scale frame.originPoint
    in
        { frame | originPoint = scaledOriginPoint }


rotateAbout : Point2d -> Float -> Frame2d -> Frame2d
rotateAbout centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAbout centerPoint angle

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


mirrorAcross : Axis2d -> Frame2d -> Frame2d
mirrorAcross axis =
    let
        mirrorPoint =
            Point2d.mirrorAcross axis

        mirrorDirection =
            Direction2d.mirrorAbout axis.direction
    in
        \frame ->
            Frame2d (mirrorPoint frame.originPoint)
                (mirrorDirection frame.xDirection)
                (mirrorDirection frame.yDirection)


toLocalIn : Frame2d -> Frame2d -> Frame2d
toLocalIn otherFrame =
    let
        localizePoint =
            Point2d.toLocalIn otherFrame

        localizeDirection =
            Direction2d.toLocalIn otherFrame
    in
        \frame ->
            Frame2d (localizePoint frame.originPoint)
                (localizeDirection frame.xDirection)
                (localizeDirection frame.yDirection)


toGlobalFrom : Frame2d -> Frame2d -> Frame2d
toGlobalFrom frame =
    let
        globalizePoint =
            Point2d.toGlobalFrom frame

        globalizeDirection =
            Direction2d.toGlobalFrom frame
    in
        \plane ->
            Frame2d (globalizePoint plane.originPoint)
                (globalizeDirection plane.xDirection)
                (globalizeDirection plane.yDirection)
