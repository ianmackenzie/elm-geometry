module OpenSolid.Expect.Axis2d exposing (axis2d, axis2dWithin)

import Expect exposing (Expectation)
import OpenSolid.Types exposing (..)
import OpenSolid.Compare.Axis2d as Compare
import OpenSolid.Expect as Expect


axis2d : Axis2d -> Axis2d -> Expectation
axis2d =
    Expect.by Compare.axis2d


axis2dWithin : Float -> Axis2d -> Axis2d -> Expectation
axis2dWithin =
    Expect.by << Compare.axis2dWithin
