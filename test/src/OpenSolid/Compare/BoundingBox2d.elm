module OpenSolid.Compare.BoundingBox2d
    exposing
        ( boundingBox2d
        , boundingBox2dWithin
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Compare as Compare exposing (Comparator)


boundingBox2d : Comparator BoundingBox2d
boundingBox2d =
    boundingBox2dWithin Compare.defaultTolerance


boundingBox2dWithin : Float -> Comparator BoundingBox2d
boundingBox2dWithin tolerance =
    let
        compareScalar =
            Compare.approximatelyWithin tolerance
    in
        Compare.allOf
            [ Compare.by compareScalar BoundingBox2d.minX
            , Compare.by compareScalar BoundingBox2d.maxX
            , Compare.by compareScalar BoundingBox2d.minY
            , Compare.by compareScalar BoundingBox2d.maxY
            ]
