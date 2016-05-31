{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Direction2d
    exposing
        ( x
        , y
        , fromAngle
        , fromComponents
        , xComponent
        , yComponent
        , components
        , vector
        , perpendicularDirection
        , rotateBy
        , mirrorAbout
        , toLocalIn
        , toGlobalFrom
        , placeOnto
        , negate
        , times
        , dot
        , cross
        , angleTo
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d


x : Direction2d
x =
    Direction2d (Vector2d 1 0)


y : Direction2d
y =
    Direction2d (Vector2d 0 1)


fromAngle : Float -> Direction2d
fromAngle angle =
    Direction2d (Vector2d (cos angle) (sin angle))


fromComponents : ( Float, Float ) -> Direction2d
fromComponents =
    Vector2d.fromComponents >> Direction2d


xComponent : Direction2d -> Float
xComponent =
    vector >> Vector2d.xComponent


yComponent : Direction2d -> Float
yComponent =
    vector >> Vector2d.yComponent


components : Direction2d -> ( Float, Float )
components =
    vector >> Vector2d.components


vector : Direction2d -> Vector2d
vector (Direction2d vector') =
    vector'


perpendicularDirection : Direction2d -> Direction2d
perpendicularDirection =
    vector >> Vector2d.perpendicularVector >> Direction2d


rotateBy : Float -> Direction2d -> Direction2d
rotateBy angle =
    vector >> Vector2d.rotateBy angle >> Direction2d


mirrorAbout : Direction2d -> Direction2d -> Direction2d
mirrorAbout direction =
    vector >> Vector2d.mirrorAbout direction >> Direction2d


toLocalIn : Frame2d -> Direction2d -> Direction2d
toLocalIn frame =
    vector >> Vector2d.toLocalIn frame >> Direction2d


toGlobalFrom : Frame2d -> Direction2d -> Direction2d
toGlobalFrom frame =
    vector >> Vector2d.toGlobalFrom frame >> Direction2d


placeOnto : Plane3d -> Direction2d -> Direction3d
placeOnto plane =
    vector >> Vector2d.placeOnto plane >> Direction3d


negate : Direction2d -> Direction2d
negate =
    vector >> Vector2d.negate >> Direction2d


times : Float -> Direction2d -> Vector2d
times scale =
    vector >> Vector2d.times scale


dot : Direction2d -> Direction2d -> Float
dot other direction =
    Vector2d.dot (vector other) (vector direction)


cross : Direction2d -> Direction2d -> Float
cross other direction =
    Vector2d.cross (vector other) (vector direction)


angleTo : Direction2d -> Direction2d -> Float
angleTo other direction =
    atan2 (cross other direction) (dot other direction)
