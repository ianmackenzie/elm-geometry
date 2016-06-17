{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Direction2d
    exposing
        ( x
        , y
        , fromAngle
        , ofNonZeroVector
        , perpendicularTo
        , fromComponents
        , xComponent
        , yComponent
        , components
        , asVector
        , rotateBy
        , mirrorAcross
        , toLocalIn
        , fromLocalIn
        , placeOnto
        , negate
        , times
        , dotProduct
        , crossProduct
        , angleFrom
        , angleTo
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d


x : Direction2d
x =
    Direction2d (Vector2d 1 0)


y : Direction2d
y =
    Direction2d (Vector2d 0 1)


fromAngle : Float -> Direction2d
fromAngle angle =
    Direction2d (Vector2d (cos angle) (sin angle))


ofNonZeroVector : Vector2d -> Direction2d
ofNonZeroVector vector =
    Direction2d (Vector2d.times (1 / Vector2d.length vector) vector)


perpendicularTo : Direction2d -> Direction2d
perpendicularTo =
    asVector >> Vector2d.perpendicularTo >> Direction2d


fromComponents : ( Float, Float ) -> Direction2d
fromComponents =
    Vector2d.fromComponents >> Direction2d


xComponent : Direction2d -> Float
xComponent =
    asVector >> Vector2d.xComponent


yComponent : Direction2d -> Float
yComponent =
    asVector >> Vector2d.yComponent


components : Direction2d -> ( Float, Float )
components =
    asVector >> Vector2d.components


asVector : Direction2d -> Vector2d
asVector (Direction2d vector) =
    vector


rotateBy : Float -> Direction2d -> Direction2d
rotateBy angle =
    asVector >> Vector2d.rotateBy angle >> Direction2d


mirrorAcross : Axis2d -> Direction2d -> Direction2d
mirrorAcross axis =
    asVector >> Vector2d.mirrorAcross axis >> Direction2d


toLocalIn : Frame2d -> Direction2d -> Direction2d
toLocalIn frame =
    asVector >> Vector2d.toLocalIn frame >> Direction2d


fromLocalIn : Frame2d -> Direction2d -> Direction2d
fromLocalIn frame =
    asVector >> Vector2d.fromLocalIn frame >> Direction2d


placeOnto : Plane3d -> Direction2d -> Direction3d
placeOnto plane =
    asVector >> Vector2d.placeOnto plane >> Direction3d


negate : Direction2d -> Direction2d
negate =
    asVector >> Vector2d.negate >> Direction2d


times : Float -> Direction2d -> Vector2d
times scale =
    asVector >> Vector2d.times scale


dotProduct : Direction2d -> Direction2d -> Float
dotProduct firstDirection secondDirection =
    Vector2d.dotProduct (asVector firstDirection) (asVector secondDirection)


crossProduct : Direction2d -> Direction2d -> Float
crossProduct firstDirection secondDirection =
    Vector2d.crossProduct (asVector firstDirection) (asVector secondDirection)


angleFrom : Direction2d -> Direction2d -> Float
angleFrom other direction =
    atan2 (crossProduct other direction) (dotProduct other direction)


angleTo : Direction2d -> Direction2d -> Float
angleTo other direction =
    atan2 (crossProduct direction other) (dotProduct direction other)
