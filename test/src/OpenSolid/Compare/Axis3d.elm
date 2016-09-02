module OpenSolid.Compare.Axis3d exposing (axis3d, axis3dWithin)

import OpenSolid.Types exposing (..)
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Compare as Compare exposing (Comparator)
import OpenSolid.Compare.Point3d as Compare
import OpenSolid.Compare.Direction3d as Compare


axis3d : Comparator Axis3d
axis3d =
    axis3dWithin Compare.defaultTolerance


axis3dWithin : Float -> Comparator Axis3d
axis3dWithin tolerance =
    Compare.allOf
        [ Compare.by (Compare.point3dWithin tolerance) Axis3d.originPoint
        , Compare.by (Compare.direction3dWithin tolerance) Axis3d.direction
        ]
