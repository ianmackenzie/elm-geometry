module OpenSolid.Compare.Vector3d exposing (vector3d, vector3dWithin)

import OpenSolid.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Compare as Compare exposing (Comparator)


vector3d : Comparator Vector3d
vector3d =
    vector3dWithin Compare.defaultTolerance


vector3dWithin : Float -> Comparator Vector3d
vector3dWithin tolerance first second =
    Vector3d.length (Vector3d.minus first second) <= tolerance
