module OpenSolid.Compare.Direction3d exposing (direction3d, direction3dWithin)

import OpenSolid.Types exposing (..)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Compare as Compare exposing (Comparator)
import OpenSolid.Compare.Vector3d as Compare


direction3d : Comparator Direction3d
direction3d =
    direction3dWithin Compare.defaultTolerance


direction3dWithin : Float -> Comparator Direction3d
direction3dWithin tolerance =
    Compare.by (Compare.vector3dWithin tolerance) Direction3d.vector
