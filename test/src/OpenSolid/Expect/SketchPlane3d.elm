module OpenSolid.Expect.SketchPlane3d
    exposing
        ( sketchPlane3d
        , sketchPlane3dWithin
        )

import Expect exposing (Expectation)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Compare.SketchPlane3d as Compare
import OpenSolid.Expect as Expect


sketchPlane3d : SketchPlane3d -> SketchPlane3d -> Expectation
sketchPlane3d =
    Expect.by Compare.sketchPlane3d


sketchPlane3dWithin : Float -> SketchPlane3d -> SketchPlane3d -> Expectation
sketchPlane3dWithin =
    Expect.by << Compare.sketchPlane3dWithin
