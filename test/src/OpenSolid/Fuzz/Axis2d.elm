module OpenSolid.Fuzz.Axis2d exposing (axis2d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Fuzz.Point2d as Fuzz
import OpenSolid.Fuzz.Direction2d as Fuzz


axis2d : Fuzzer Axis2d
axis2d =
    let
        tuple =
            Fuzz.tuple ( Fuzz.point2d, Fuzz.direction2d )

        tupleToAxis ( originPoint, direction ) =
            Axis2d { originPoint = originPoint, direction = direction }
    in
        Fuzz.map tupleToAxis tuple
