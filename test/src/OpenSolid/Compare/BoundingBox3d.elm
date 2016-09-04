module OpenSolid.Compare.BoundingBox3d
    exposing
        ( boundingBox3d
        , boundingBox3dWithin
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Compare as Compare exposing (Comparator)


boundingBox3d : Comparator BoundingBox3d
boundingBox3d =
    boundingBox3dWithin Compare.defaultTolerance


boundingBox3dWithin : Float -> Comparator BoundingBox3d
boundingBox3dWithin tolerance =
    let
        compareScalar =
            Compare.approximatelyWithin tolerance
    in
        Compare.allOf
            [ Compare.by compareScalar BoundingBox3d.minX
            , Compare.by compareScalar BoundingBox3d.maxX
            , Compare.by compareScalar BoundingBox3d.minY
            , Compare.by compareScalar BoundingBox3d.maxY
            , Compare.by compareScalar BoundingBox3d.minZ
            , Compare.by compareScalar BoundingBox3d.maxZ
            ]
