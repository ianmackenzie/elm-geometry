module OpenSolid.Expect.Point3d exposing (point3d, point3dWithin)

import Expect exposing (Expectation)
import OpenSolid.Types exposing (..)
import OpenSolid.Expect as Expect
import OpenSolid.Compare.Point3d as Compare


point3d : Point3d -> Point3d -> Expectation
point3d =
    Expect.by Compare.point3d


point3dWithin : Float -> Point3d -> Point3d -> Expectation
point3dWithin =
    Expect.by << Compare.point3dWithin
