{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Axis2d
    exposing
        ( x
        , y
        , reverse
        , scaleAbout
        , rotateAround
        , translateBy
        , translateAlong
        , mirrorAcross
        , toLocalIn
        , fromLocalIn
        , placeOnto
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Direction2d as Direction2d


x : Axis2d
x =
    Axis2d Point2d.origin Direction2d.x


y : Axis2d
y =
    Axis2d Point2d.origin Direction2d.y


reverse : Axis2d -> Axis2d
reverse axis =
    Axis2d axis.originPoint (Direction2d.negate axis.direction)


scaleAbout : Point2d -> Float -> Axis2d -> Axis2d
scaleAbout centerPoint scale axis =
    let
        scalePoint =
            Point2d.scaleAbout centerPoint scale
    in
        Axis2d (scalePoint axis.originPoint) axis.direction


rotateAround : Point2d -> Float -> Axis2d -> Axis2d
rotateAround centerPoint angle =
    let
        rotatePoint =
            Point2d.rotateAround centerPoint angle

        rotateDirection =
            Direction2d.rotateBy angle
    in
        \axis ->
            Axis2d (rotatePoint axis.originPoint)
                (rotateDirection axis.direction)


translateBy : Vector2d -> Axis2d -> Axis2d
translateBy vector axis =
    Axis2d (Point2d.plus vector axis.originPoint) axis.direction


translateAlong : Axis2d -> Float -> Axis2d -> Axis2d
translateAlong axis distance =
    translateBy (Vector2d.along axis distance)


mirrorAcross : Axis2d -> Axis2d -> Axis2d
mirrorAcross otherAxis =
    let
        mirrorPoint =
            Point2d.mirrorAcross otherAxis

        mirrorDirection =
            Direction2d.mirrorAcross otherAxis
    in
        \axis ->
            Axis2d (mirrorPoint axis.originPoint)
                (mirrorDirection axis.direction)


toLocalIn : Frame2d -> Axis2d -> Axis2d
toLocalIn frame =
    let
        toLocalPoint =
            Point2d.toLocalIn frame

        toLocalDirection =
            Direction2d.toLocalIn frame
    in
        \axis ->
            Axis2d (toLocalPoint axis.originPoint)
                (toLocalDirection axis.direction)


fromLocalIn : Frame2d -> Axis2d -> Axis2d
fromLocalIn frame =
    let
        fromLocalPoint =
            Point2d.fromLocalIn frame

        fromLocalDirection =
            Direction2d.fromLocalIn frame
    in
        \axis ->
            Axis2d (fromLocalPoint axis.originPoint)
                (fromLocalDirection axis.direction)


placeOnto : Plane3d -> Axis2d -> Axis3d
placeOnto plane =
    let
        placePoint =
            Point2d.placeOnto plane

        placeDirection =
            Direction2d.placeOnto plane
    in
        \axis ->
            Axis3d (placePoint axis.originPoint)
                (placeDirection axis.direction)
