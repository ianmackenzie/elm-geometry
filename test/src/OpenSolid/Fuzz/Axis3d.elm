module OpenSolid.Fuzz.Axis3d exposing (axis3d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Types exposing (..)
import OpenSolid.Fuzz.Point3d as Fuzz
import OpenSolid.Fuzz.Direction3d as Fuzz


axis3d : Fuzzer Axis3d
axis3d =
    let
        tuple =
            Fuzz.tuple ( Fuzz.point3d, Fuzz.direction3d )

        tupleToAxis ( originPoint, direction ) =
            Axis3d { originPoint = originPoint, direction = direction }
    in
        Fuzz.map tupleToAxis tuple
