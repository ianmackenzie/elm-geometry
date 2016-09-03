module OpenSolid.Expect.Direction2d exposing (direction2d, direction2dWithin)

import Expect exposing (Expectation)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Expect as Expect
import OpenSolid.Compare.Direction2d as Compare


direction2d : Direction2d -> Direction2d -> Expectation
direction2d =
    Expect.by Compare.direction2d


direction2dWithin : Float -> Direction2d -> Direction2d -> Expectation
direction2dWithin =
    Expect.by << Compare.direction2dWithin
