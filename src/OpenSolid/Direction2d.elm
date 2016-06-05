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
        , ofVector
        , ofNonZeroVector
        , fromAngle
        , fromComponents
        , xComponent
        , yComponent
        , components
        , vector
        , perpendicularDirection
        , rotateBy
        , mirrorAcross
        , toLocalIn
        , fromLocalIn
        , placeOnto
        , negate
        , times
        , dotProduct
        , crossProduct
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


{-| Attempt to find the direction of a vector. In the case of a zero vector,
return `Nothing`.

    Direction2d.ofVector (Vector2d 1 1) == Just (Direction2d (Vector2d 0.7071 0.7071))
    Direction2d.ofVector (Vector2d 0 0) == Nothing

For instance, given an eye point and a point to look at, the corresponding view
direction could be determined with

    Direction2d.ofVector (Point2d.vectorFrom eyePoint lookAtPoint)

This would return a `Maybe Direction2d`, with `Nothing` corresponding to the
case where the eye point and point to look at are coincident (in which case the
view direction is not well-defined and some special-case logic is needed).
-}
ofVector : Vector2d -> Maybe Direction2d
ofVector vector =
    if vector == Vector2d.zero then
        Nothing
    else
        Just (ofNonZeroVector vector)


ofNonZeroVector : Vector2d -> Direction2d
ofNonZeroVector vector =
    Direction2d (Vector2d.times (1 / Vector2d.length vector) vector)


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


mirrorAcross : Axis2d -> Direction2d -> Direction2d
mirrorAcross axis =
    vector >> Vector2d.mirrorAcross axis >> Direction2d


toLocalIn : Frame2d -> Direction2d -> Direction2d
toLocalIn frame =
    vector >> Vector2d.toLocalIn frame >> Direction2d


fromLocalIn : Frame2d -> Direction2d -> Direction2d
fromLocalIn frame =
    vector >> Vector2d.fromLocalIn frame >> Direction2d


placeOnto : Plane3d -> Direction2d -> Direction3d
placeOnto plane =
    vector >> Vector2d.placeOnto plane >> Direction3d


negate : Direction2d -> Direction2d
negate =
    vector >> Vector2d.negate >> Direction2d


times : Float -> Direction2d -> Vector2d
times scale =
    vector >> Vector2d.times scale


dotProduct : Direction2d -> Direction2d -> Float
dotProduct firstDirection secondDirection =
    Vector2d.dotProduct (vector firstDirection) (vector secondDirection)


crossProduct : Direction2d -> Direction2d -> Float
crossProduct firstDirection secondDirection =
    Vector2d.crossProduct (vector firstDirection) (vector secondDirection)


angleTo : Direction2d -> Direction2d -> Float
angleTo other direction =
    atan2 (crossProduct direction other) (dotProduct direction other)
