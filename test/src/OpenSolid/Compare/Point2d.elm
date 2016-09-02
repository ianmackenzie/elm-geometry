module OpenSolid.Compare.Point2d exposing (point2d, point2dWithin)

import OpenSolid.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Compare as Compare exposing (Comparator)


point2d : Comparator Point2d
point2d =
    point2dWithin Compare.defaultTolerance


point2dWithin : Float -> Comparator Point2d
point2dWithin tolerance first second =
    Point2d.distanceFrom first second <= tolerance
