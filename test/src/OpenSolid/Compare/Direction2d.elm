module OpenSolid.Compare.Direction2d exposing (direction2d, direction2dWithin)

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Compare as Compare exposing (Comparator)
import OpenSolid.Compare.Vector2d as Compare


direction2d : Comparator Direction2d
direction2d =
    direction2dWithin Compare.defaultTolerance


direction2dWithin : Float -> Comparator Direction2d
direction2dWithin tolerance =
    Compare.by (Compare.vector2dWithin tolerance) Direction2d.vector
