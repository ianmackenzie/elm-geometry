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
        , scaleAbout
        , rotateAround
        , translateBy
        , translateAlong
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
    Axis2d Point2d.origin Direction2d.x


y : Axis2d
y =
    Axis2d Point2d.origin Direction2d.y


perpendicularTo : Axis2d -> Axis2d
perpendicularTo axis =
    Axis2d axis.originPoint (Direction2d.perpendicularTo axis.direction)


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
    Axis2d (Point2d.translateBy vector axis.originPoint) axis.direction


translateAlong : Axis2d -> Float -> Axis2d -> Axis2d
translateAlong axis distance =
    translateBy (Vector2d.in' axis.direction distance)


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


localizeTo : Frame2d -> Axis2d -> Axis2d
localizeTo frame =
    let
        localizePoint =
            Point2d.localizeTo frame

        localizeDirection =
            Direction2d.localizeTo frame
    in
        \axis ->
            Axis2d (localizePoint axis.originPoint)
                (localizeDirection axis.direction)


placeIn : Frame2d -> Axis2d -> Axis2d
placeIn frame =
    let
        placePoint =
            Point2d.placeIn frame

        placeDirection =
            Direction2d.placeIn frame
    in
        \axis ->
            Axis2d (placePoint axis.originPoint)
                (placeDirection axis.direction)


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
