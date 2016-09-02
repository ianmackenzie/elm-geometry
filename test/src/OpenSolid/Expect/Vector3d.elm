module OpenSolid.Expect.Vector3d exposing (vector3d, vector3dWithin)

import Expect exposing (Expectation)
import OpenSolid.Types exposing (..)
import OpenSolid.Expect as Expect
import OpenSolid.Compare.Vector3d as Compare


vector3d : Vector3d -> Vector3d -> Expectation
vector3d =
    Expect.by Compare.vector3d


vector3dWithin : Float -> Vector3d -> Vector3d -> Expectation
vector3dWithin =
    Expect.by << Compare.vector3dWithin
