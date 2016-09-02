module OpenSolid.Expect.Plane3d exposing (plane3d, plane3dWithin)

import Expect exposing (Expectation)
import OpenSolid.Types exposing (..)
import OpenSolid.Compare.Plane3d as Compare
import OpenSolid.Expect as Expect


plane3d : Plane3d -> Plane3d -> Expectation
plane3d =
    Expect.by Compare.plane3d


plane3dWithin : Float -> Plane3d -> Plane3d -> Expectation
plane3dWithin =
    Expect.by << Compare.plane3dWithin
