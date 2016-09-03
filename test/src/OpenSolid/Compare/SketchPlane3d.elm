module OpenSolid.Compare.SketchPlane3d
    exposing
        ( sketchPlane3d
        , sketchPlane3dWithin
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.SketchPlane3d as SketchPlane3d
import OpenSolid.Compare as Compare exposing (Comparator)
import OpenSolid.Compare.Point3d as Compare
import OpenSolid.Compare.Direction3d as Compare


sketchPlane3d : Comparator SketchPlane3d
sketchPlane3d =
    sketchPlane3dWithin Compare.defaultTolerance


sketchPlane3dWithin : Float -> Comparator SketchPlane3d
sketchPlane3dWithin tolerance =
    Compare.allOf
        [ Compare.by (Compare.point3dWithin tolerance)
            SketchPlane3d.originPoint
        , Compare.by (Compare.direction3dWithin tolerance)
            SketchPlane3d.xDirection
        , Compare.by (Compare.direction3dWithin tolerance)
            SketchPlane3d.yDirection
        ]
