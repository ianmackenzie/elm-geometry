module OpenSolid.Compare.Point3d exposing (point3d, point3dWithin)

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Compare as Compare exposing (Comparator)


point3d : Comparator Point3d
point3d =
    point3dWithin Compare.defaultTolerance


point3dWithin : Float -> Comparator Point3d
point3dWithin tolerance first second =
    Point3d.distanceFrom first second <= tolerance
