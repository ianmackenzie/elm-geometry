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
        , perpendicularTo
        , xComponent
        , yComponent
        , zComponent
        , components
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


toVector (Direction3d components) =
    Vector3d components


toDirection (Vector3d components) =
    Direction3d components


x : Direction3d
x =
    Direction3d ( 1, 0, 0 )


y : Direction3d
y =
    Direction3d ( 0, 1, 0 )


z : Direction3d
z =
    Direction3d ( 0, 0, 1 )


perpendicularTo : Direction3d -> Direction3d
perpendicularTo direction =
    let
        perpendicularVector =
            Vector3d.perpendicularTo (toVector direction)

        length =
            Vector3d.length perpendicularVector

        normalizedVector =
            Vector3d.times (1 / length) perpendicularVector
    in
        Direction3d (Vector3d.components normalizedVector)


components : Direction3d -> ( Float, Float, Float )
components (Direction3d components') =
    components'


xComponent : Direction3d -> Float
xComponent (Direction3d ( x, _, _ )) =
    x


yComponent : Direction3d -> Float
yComponent (Direction3d ( _, y, _ )) =
    y


zComponent : Direction3d -> Float
zComponent (Direction3d ( _, _, z )) =
    z


rotateAround : Axis3d -> Float -> Direction3d -> Direction3d
rotateAround axis angle =
    toVector >> Vector3d.rotateAround axis angle >> toDirection


mirrorAcross : Plane3d -> Direction3d -> Direction3d
mirrorAcross plane =
    toVector >> Vector3d.mirrorAcross plane >> toDirection


localizeTo : Frame3d -> Direction3d -> Direction3d
localizeTo frame =
    toVector >> Vector3d.localizeTo frame >> toDirection


placeIn : Frame3d -> Direction3d -> Direction3d
placeIn frame =
    toVector >> Vector3d.placeIn frame >> toDirection


projectOnto : Plane3d -> Direction3d -> Maybe Direction3d
projectOnto plane =
    toVector >> Vector3d.projectOnto plane >> Vector3d.direction


projectInto : Plane3d -> Direction3d -> Maybe Direction2d
projectInto plane =
    toVector >> Vector3d.projectInto plane >> Vector2d.direction


negate : Direction3d -> Direction3d
negate =
    toVector >> Vector3d.negate >> toDirection


times : Float -> Direction3d -> Vector3d
times scale =
    toVector >> Vector3d.times scale


dotProduct : Direction3d -> Direction3d -> Float
dotProduct firstDirection secondDirection =
    Vector3d.dotProduct (toVector firstDirection) (toVector secondDirection)


crossProduct : Direction3d -> Direction3d -> Vector3d
crossProduct firstDirection secondDirection =
    Vector3d.crossProduct (toVector firstDirection) (toVector secondDirection)


angleFrom : Direction3d -> Direction3d -> Float
angleFrom other direction =
    acos (dotProduct direction other)
