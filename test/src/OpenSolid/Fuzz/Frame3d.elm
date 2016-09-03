module OpenSolid.Fuzz.Frame3d exposing (frame3d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Fuzz.Point3d as Fuzz
import OpenSolid.Fuzz.Direction3d as Fuzz


frame3d : Fuzzer Frame3d
frame3d =
    let
        tuple =
            Fuzz.tuple ( Fuzz.point3d, Fuzz.direction3d )

        tupleToFrame ( originPoint, xDirection ) =
            let
                yDirection =
                    Direction3d.perpendicularTo xDirection

                zDirectionVector =
                    Direction3d.crossProduct xDirection yDirection

                zDirection =
                    Direction3d (Vector3d.components zDirectionVector)
            in
                Frame3d
                    { originPoint = originPoint
                    , xDirection = xDirection
                    , yDirection = yDirection
                    , zDirection = zDirection
                    }
    in
        Fuzz.map tupleToFrame tuple
