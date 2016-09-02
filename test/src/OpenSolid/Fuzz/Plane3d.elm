module OpenSolid.Fuzz.Plane3d exposing (plane3d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Types exposing (..)
import OpenSolid.Fuzz.Point3d as Fuzz
import OpenSolid.Fuzz.Direction3d as Fuzz


plane3d : Fuzzer Plane3d
plane3d =
    let
        tuple =
            Fuzz.tuple ( Fuzz.point3d, Fuzz.direction3d )

        tupleToPlane ( originPoint, normalDirection ) =
            Plane3d
                { originPoint = originPoint
                , normalDirection = normalDirection
                }
    in
        Fuzz.map tupleToPlane tuple
