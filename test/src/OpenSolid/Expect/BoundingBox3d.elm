module OpenSolid.Expect.BoundingBox3d
    exposing
        ( boundingBox3d
        , boundingBox3dWithin
        )

import Expect exposing (Expectation)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Compare.BoundingBox3d as Compare
import OpenSolid.Expect as Expect


boundingBox3d : BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3d =
    Expect.by Compare.boundingBox3d


boundingBox3dWithin : Float -> BoundingBox3d -> BoundingBox3d -> Expectation
boundingBox3dWithin =
    Expect.by << Compare.boundingBox3dWithin
