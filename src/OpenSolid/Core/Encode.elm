{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Encode
    exposing
        ( vector2d
        , vector3d
        , direction2d
        , direction3d
        , point2d
        , point3d
        , axis2d
        , axis3d
        , plane3d
        , frame2d
        , frame3d
        )

{-| JSON encoders for the core OpenSolid types.
-}

import Json.Encode exposing (..)
import OpenSolid.Core.Types exposing (..)


vector2d : Vector2d -> Value
vector2d (Vector2d x y) =
    list [ float x, float y ]
