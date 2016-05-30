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
        , relativeTo
        , placeIn
        , projectOnto
        , projectInto
        , negate
        , times
        , dot
        , cross
        , angleTo
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Vector3d as Vector3d


x : Direction3d
x =
    Direction3d (Vector3d 1 0 0)


y : Direction3d
y =
    Direction3d (Vector3d 0 1 0)


z : Direction3d
z =
    Direction3d (Vector3d 0 0 1)


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
perpendicularDirection direction =
    let
        perpendicularVector =
            Vector3d.perpendicularVector (vector direction)

        inverseLength =
            1 / Vector3d.length perpendicularVector
    in
        Direction3d (Vector3d.times inverseLength perpendicularVector)


normalBasis : Direction3d -> ( Direction3d, Direction3d )
normalBasis direction =
    let
        xDirection =
            perpendicularDirection direction

        yDirection =
            Direction3d (Vector3d.cross (vector xDirection) (vector direction))
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


relativeTo : Frame3d -> Direction3d -> Direction3d
relativeTo frame =
    let
        localizeVector =
            Vector3d.relativeTo frame
    in
        vector >> localizeVector >> Direction3d


placeIn : Frame3d -> Direction3d -> Direction3d
placeIn frame =
    let
        globalizeVector =
            Vector3d.placeIn frame
    in
        vector >> globalizeVector >> Direction3d


projectOnto : Plane3d -> Direction3d -> Maybe Direction3d
projectOnto plane =
    vector >> Vector3d.projectOnto plane >> Vector3d.direction


projectInto : Plane3d -> Direction3d -> Maybe Direction2d
projectInto plane =
    vector >> Vector3d.projectInto plane >> Vector2d.direction


negate : Direction3d -> Direction3d
negate =
    vector >> Vector3d.negate >> Direction3d


times : Float -> Direction3d -> Vector3d
times scale =
    vector >> Vector3d.times scale


dot : Direction3d -> Direction3d -> Float
dot other direction =
    Vector3d.dot (vector other) (vector direction)


cross : Direction3d -> Direction3d -> Vector3d
cross other direction =
    Vector3d.cross (vector other) (vector direction)


angleTo : Direction3d -> Direction3d -> Float
angleTo other direction =
    acos (dot other direction)
