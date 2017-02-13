module OpenSolid.Arc3d
    exposing
        ( Arc3d
        )

import OpenSolid.Geometry.Types exposing (..)


type Arc3d
    = Arc3d { sketchPlane : SketchPlane3d, angle : Float }
