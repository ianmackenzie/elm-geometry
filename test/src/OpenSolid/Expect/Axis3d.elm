module OpenSolid.Expect.Axis3d exposing (axis3d, axis3dWithin)

import Expect exposing (Expectation)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Compare.Axis3d as Compare
import OpenSolid.Expect as Expect


axis3d : Axis3d -> Axis3d -> Expectation
axis3d =
    Expect.by Compare.axis3d


axis3dWithin : Float -> Axis3d -> Axis3d -> Expectation
axis3dWithin =
    Expect.by << Compare.axis3dWithin
