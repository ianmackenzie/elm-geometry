module OpenSolid.Core.Encode (vector2d, vector3d, direction2d, direction3d, point2d, point3d, axis2d, axis3d, plane3d, frame2d, frame3d) where

{-| JSON encoders for the core OpenSolid types.
-}

import Json.Encode exposing (..)
import OpenSolid.Core.Types exposing (..)
