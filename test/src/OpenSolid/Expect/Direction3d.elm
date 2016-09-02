module OpenSolid.Expect.Direction3d exposing (direction3d, direction3dWithin)

import Expect exposing (Expectation)
import OpenSolid.Types exposing (..)
import OpenSolid.Expect exposing (by)
import OpenSolid.Compare.Direction3d as Compare


direction3d : Direction3d -> Direction3d -> Expectation
direction3d =
    by Compare.direction3d


direction3dWithin : Float -> Direction3d -> Direction3d -> Expectation
direction3dWithin =
    by << Compare.direction3dWithin
