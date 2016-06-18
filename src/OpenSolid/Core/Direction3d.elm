{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Direction3d
    exposing
        ( x
        , y
        , z
        , ofNonZeroVector
        , perpendicularTo
        , perpendicularBasis
        , fromComponents
        , xComponent
        , yComponent
        , zComponent
        , components
        , asVector
        , rotateAround
        , mirrorAcross
        , localizeTo
        , placeIn
        , projectOnto
        , projectInto
        , negate
        , times
        , dotProduct
        , crossProduct
        , angleFrom
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction2d as Direction2d


x : Direction3d
x =
    Direction3d (Vector3d 1 0 0)


y : Direction3d
y =
    Direction3d (Vector3d 0 1 0)


z : Direction3d
z =
    Direction3d (Vector3d 0 0 1)


ofNonZeroVector : Vector3d -> Direction3d
ofNonZeroVector vector =
    Direction3d (Vector3d.times (1 / Vector3d.length vector) vector)


perpendicularTo : Direction3d -> Direction3d
perpendicularTo =
    asVector >> Vector3d.perpendicularTo >> ofNonZeroVector


perpendicularBasis : Direction3d -> ( Direction3d, Direction3d )
perpendicularBasis direction =
    let
        xDirection =
            perpendicularTo direction

        yDirection =
            Direction3d (crossProduct direction xDirection)
    in
        ( xDirection, yDirection )


fromComponents : ( Float, Float, Float ) -> Direction3d
fromComponents =
    Vector3d.fromComponents >> Direction3d


xComponent : Direction3d -> Float
xComponent =
    asVector >> Vector3d.xComponent


yComponent : Direction3d -> Float
yComponent =
    asVector >> Vector3d.yComponent


zComponent : Direction3d -> Float
zComponent =
    asVector >> Vector3d.zComponent


components : Direction3d -> ( Float, Float, Float )
components =
    asVector >> Vector3d.components


asVector : Direction3d -> Vector3d
asVector (Direction3d vector) =
    vector


rotateAround : Axis3d -> Float -> Direction3d -> Direction3d
rotateAround axis angle =
    asVector >> Vector3d.rotateAround axis angle >> Direction3d


mirrorAcross : Plane3d -> Direction3d -> Direction3d
mirrorAcross plane =
    asVector >> Vector3d.mirrorAcross plane >> Direction3d


localizeTo : Frame3d -> Direction3d -> Direction3d
localizeTo frame =
    asVector >> Vector3d.localizeTo frame >> Direction3d


placeIn : Frame3d -> Direction3d -> Direction3d
placeIn frame =
    asVector >> Vector3d.placeIn frame >> Direction3d


projectOnto : Plane3d -> Direction3d -> Maybe Direction3d
projectOnto plane =
    asVector >> Vector3d.projectOnto plane >> Vector3d.direction


projectInto : Plane3d -> Direction3d -> Maybe Direction2d
projectInto plane =
    asVector >> Vector3d.projectInto plane >> Vector2d.direction


negate : Direction3d -> Direction3d
negate =
    asVector >> Vector3d.negate >> Direction3d


times : Float -> Direction3d -> Vector3d
times scale =
    asVector >> Vector3d.times scale


dotProduct : Direction3d -> Direction3d -> Float
dotProduct firstDirection secondDirection =
    Vector3d.dotProduct (asVector firstDirection) (asVector secondDirection)


crossProduct : Direction3d -> Direction3d -> Vector3d
crossProduct firstDirection secondDirection =
    Vector3d.crossProduct (asVector firstDirection) (asVector secondDirection)


angleFrom : Direction3d -> Direction3d -> Float
angleFrom other direction =
    acos (dotProduct direction other)
