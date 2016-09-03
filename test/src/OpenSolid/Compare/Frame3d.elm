module OpenSolid.Compare.Frame3d
    exposing
        ( frame3d
        , frame3dWithin
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Compare as Compare exposing (Comparator)
import OpenSolid.Compare.Point3d as Compare
import OpenSolid.Compare.Direction3d as Compare


frame3d : Comparator Frame3d
frame3d =
    frame3dWithin Compare.defaultTolerance


frame3dWithin : Float -> Comparator Frame3d
frame3dWithin tolerance =
    Compare.allOf
        [ Compare.by (Compare.point3dWithin tolerance)
            Frame3d.originPoint
        , Compare.by (Compare.direction3dWithin tolerance)
            Frame3d.xDirection
        , Compare.by (Compare.direction3dWithin tolerance)
            Frame3d.yDirection
        , Compare.by (Compare.direction3dWithin tolerance)
            Frame3d.zDirection
        ]
