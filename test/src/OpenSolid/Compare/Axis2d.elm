module OpenSolid.Compare.Axis2d exposing (axis2d, axis2dWithin)

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Compare as Compare exposing (Comparator)
import OpenSolid.Compare.Point2d as Compare
import OpenSolid.Compare.Direction2d as Compare


axis2d : Comparator Axis2d
axis2d =
    axis2dWithin Compare.defaultTolerance


axis2dWithin : Float -> Comparator Axis2d
axis2dWithin tolerance =
    Compare.allOf
        [ Compare.by (Compare.point2dWithin tolerance) Axis2d.originPoint
        , Compare.by (Compare.direction2dWithin tolerance) Axis2d.direction
        ]
