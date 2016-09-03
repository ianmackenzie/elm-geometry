module OpenSolid.Expect.Vector2d exposing (vector2d, vector2dWithin)

import Expect exposing (Expectation)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Expect as Expect
import OpenSolid.Compare.Vector2d as Compare


vector2d : Vector2d -> Vector2d -> Expectation
vector2d =
    Expect.by Compare.vector2d


vector2dWithin : Float -> Vector2d -> Vector2d -> Expectation
vector2dWithin =
    Expect.by << Compare.vector2dWithin
