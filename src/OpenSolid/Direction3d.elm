{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Direction3d
    exposing
        ( x
        , y
        , z
        , ofVector
        , ofNonZeroVector
        , fromComponents
        , xComponent
        , yComponent
        , zComponent
        , components
        , vector
        , perpendicularDirection
        , normalBasis
        , rotateAbout
        , mirrorAlong
        , toLocalIn
        , toGlobalFrom
        , projectOnto
        , projectInto
        , negate
        , times
        , dotProduct
        , crossProduct
        , angleTo
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction2d as Direction2d


x : Direction3d
x =
    Direction3d (Vector3d 1 0 0)


y : Direction3d
y =
    Direction3d (Vector3d 0 1 0)


z : Direction3d
z =
    Direction3d (Vector3d 0 0 1)


{-| Attempt to find the direction of a vector. In the case of a zero vector,
return `Nothing`.

    Direction3d.ofVector (Vector3d 1 0 1) == Just (Direction3d (Vector3d 0.7071 0 0.7071))
    Direction3d.ofVector (Vector3d 0 0 0) == Nothing

For instance, given an eye point and a point to look at, the corresponding view
direction could be determined with

    Direction3d.ofVector (Point3d.vectorFrom eyePoint lookAtPoint)

This would return a `Maybe Direction3d`, with `Nothing` corresponding to the
case where the eye point and point to look at are coincident (in which case the
view direction is not well-defined and some special-case logic is needed).
-}
ofVector : Vector3d -> Maybe Direction3d
ofVector vector =
    if vector == Vector3d.zero then
        Nothing
    else
        Just (ofNonZeroVector vector)


ofNonZeroVector : Vector3d -> Direction3d
ofNonZeroVector vector =
    Direction3d (Vector3d.times (1 / Vector3d.length vector) vector)


fromComponents : ( Float, Float, Float ) -> Direction3d
fromComponents =
    Vector3d.fromComponents >> Direction3d


xComponent : Direction3d -> Float
xComponent =
    vector >> Vector3d.xComponent


yComponent : Direction3d -> Float
yComponent =
    vector >> Vector3d.yComponent


zComponent : Direction3d -> Float
zComponent =
    vector >> Vector3d.zComponent


components : Direction3d -> ( Float, Float, Float )
components =
    vector >> Vector3d.components


vector : Direction3d -> Vector3d
vector (Direction3d vector') =
    vector'


perpendicularDirection : Direction3d -> Direction3d
perpendicularDirection =
    vector >> Vector3d.perpendicularVector >> ofNonZeroVector


normalBasis : Direction3d -> ( Direction3d, Direction3d )
normalBasis direction =
    let
        xDirection =
            perpendicularDirection direction

        yDirection =
            Direction3d (crossProduct direction xDirection)
    in
        ( xDirection, yDirection )


rotateAbout : Direction3d -> Float -> Direction3d -> Direction3d
rotateAbout direction angle =
    let
        rotateVector =
            Vector3d.rotateAbout direction angle
    in
        vector >> rotateVector >> Direction3d


mirrorAlong : Direction3d -> Direction3d -> Direction3d
mirrorAlong direction =
    let
        mirrorVector =
            Vector3d.mirrorAlong direction
    in
        vector >> mirrorVector >> Direction3d


toLocalIn : Frame3d -> Direction3d -> Direction3d
toLocalIn frame =
    let
        localizeVector =
            Vector3d.toLocalIn frame
    in
        vector >> localizeVector >> Direction3d


toGlobalFrom : Frame3d -> Direction3d -> Direction3d
toGlobalFrom frame =
    let
        globalizeVector =
            Vector3d.toGlobalFrom frame
    in
        vector >> globalizeVector >> Direction3d


projectOnto : Plane3d -> Direction3d -> Maybe Direction3d
projectOnto plane =
    vector >> Vector3d.projectOnto plane >> ofVector


projectInto : Plane3d -> Direction3d -> Maybe Direction2d
projectInto plane =
    vector >> Vector3d.projectInto plane >> Direction2d.ofVector


negate : Direction3d -> Direction3d
negate =
    vector >> Vector3d.negate >> Direction3d


times : Float -> Direction3d -> Vector3d
times scale =
    vector >> Vector3d.times scale


dotProduct : Direction3d -> Direction3d -> Float
dotProduct firstDirection secondDirection =
    Vector3d.dotProduct (vector firstDirection) (vector secondDirection)


crossProduct : Direction3d -> Direction3d -> Vector3d
crossProduct firstDirection secondDirection =
    Vector3d.crossProduct (vector firstDirection) (vector secondDirection)


angleTo : Direction3d -> Direction3d -> Float
angleTo other direction =
    acos (dotProduct direction other)
