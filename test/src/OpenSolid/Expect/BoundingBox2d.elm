module OpenSolid.Expect.BoundingBox2d
    exposing
        ( boundingBox2d
        , boundingBox2dWithin
        )

import Expect exposing (Expectation)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Compare.BoundingBox2d as Compare
import OpenSolid.Expect as Expect


boundingBox2d : BoundingBox2d -> BoundingBox2d -> Expectation
boundingBox2d =
    Expect.by Compare.boundingBox2d


boundingBox2dWithin : Float -> BoundingBox2d -> BoundingBox2d -> Expectation
boundingBox2dWithin =
    Expect.by << Compare.boundingBox2dWithin
