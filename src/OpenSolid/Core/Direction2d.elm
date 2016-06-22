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
        , perpendicularTo
        , xComponent
        , yComponent
        , components
        , rotateBy
        , mirrorAcross
        , localizeTo
        , placeIn
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


toVector (Direction2d components) =
    Vector2d components


toDirection (Vector2d components) =
    Direction2d components


x : Direction2d
x =
    Direction2d ( 1, 0 )


y : Direction2d
y =
    Direction2d ( 0, 1 )


fromAngle : Float -> Direction2d
fromAngle angle =
    Direction2d ( cos angle, sin angle )


perpendicularTo : Direction2d -> Direction2d
perpendicularTo =
    toVector >> Vector2d.perpendicularTo >> toDirection


xComponent : Direction2d -> Float
xComponent =
    components >> fst


yComponent : Direction2d -> Float
yComponent =
    components >> snd


components : Direction2d -> ( Float, Float )
components (Direction2d components') =
    components'


rotateBy : Float -> Direction2d -> Direction2d
rotateBy angle =
    toVector >> Vector2d.rotateBy angle >> toDirection


mirrorAcross : Axis2d -> Direction2d -> Direction2d
mirrorAcross axis =
    toVector >> Vector2d.mirrorAcross axis >> toDirection


localizeTo : Frame2d -> Direction2d -> Direction2d
localizeTo frame =
    toVector >> Vector2d.localizeTo frame >> toDirection


placeIn : Frame2d -> Direction2d -> Direction2d
placeIn frame =
    toVector >> Vector2d.placeIn frame >> toDirection


placeOnto : Plane3d -> Direction2d -> Direction3d
placeOnto plane =
    toVector
        >> Vector2d.placeOnto plane
        >> (\(Vector3d components) -> Direction3d components)


negate : Direction2d -> Direction2d
negate =
    toVector >> Vector2d.negate >> toDirection


times : Float -> Direction2d -> Vector2d
times scale =
    toVector >> Vector2d.times scale


dotProduct : Direction2d -> Direction2d -> Float
dotProduct firstDirection secondDirection =
    Vector2d.dotProduct (toVector firstDirection) (toVector secondDirection)


crossProduct : Direction2d -> Direction2d -> Float
crossProduct firstDirection secondDirection =
    Vector2d.crossProduct (toVector firstDirection) (toVector secondDirection)


angleFrom : Direction2d -> Direction2d -> Float
angleFrom other direction =
    atan2 (crossProduct other direction) (dotProduct other direction)


angleTo : Direction2d -> Direction2d -> Float
angleTo other direction =
    atan2 (crossProduct direction other) (dotProduct direction other)
