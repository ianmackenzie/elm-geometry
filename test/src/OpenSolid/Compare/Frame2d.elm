module OpenSolid.Compare.Frame2d
    exposing
        ( frame2d
        , frame2dWithin
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Compare as Compare exposing (Comparator)
import OpenSolid.Compare.Point2d as Compare
import OpenSolid.Compare.Direction2d as Compare


frame2d : Comparator Frame2d
frame2d =
    frame2dWithin Compare.defaultTolerance


frame2dWithin : Float -> Comparator Frame2d
frame2dWithin tolerance =
    Compare.allOf
        [ Compare.by (Compare.point2dWithin tolerance)
            Frame2d.originPoint
        , Compare.by (Compare.direction2dWithin tolerance)
            Frame2d.xDirection
        , Compare.by (Compare.direction2dWithin tolerance)
            Frame2d.yDirection
        ]
