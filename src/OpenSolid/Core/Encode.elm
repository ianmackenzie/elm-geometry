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
