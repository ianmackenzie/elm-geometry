module OpenSolid.Compare.Vector2d exposing (vector2d, vector2dWithin)

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Compare as Compare exposing (Comparator)


vector2d : Comparator Vector2d
vector2d =
    vector2dWithin Compare.defaultTolerance


vector2dWithin : Float -> Comparator Vector2d
vector2dWithin tolerance first second =
    Vector2d.length (Vector2d.minus first second) <= tolerance
