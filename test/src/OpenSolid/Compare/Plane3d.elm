module OpenSolid.Compare.Plane3d exposing (plane3d, plane3dWithin)

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Compare as Compare exposing (Comparator)
import OpenSolid.Compare.Point3d as Compare
import OpenSolid.Compare.Direction3d as Compare


plane3d : Comparator Plane3d
plane3d =
    plane3dWithin Compare.defaultTolerance


plane3dWithin : Float -> Comparator Plane3d
plane3dWithin tolerance =
    Compare.allOf
        [ Compare.by (Compare.point3dWithin tolerance)
            Plane3d.originPoint
        , Compare.by (Compare.direction3dWithin tolerance)
            Plane3d.normalDirection
        ]
