module OpenSolid.Fuzz.Frame2d exposing (frame2d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Types exposing (..)
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Fuzz.Point2d as Fuzz
import OpenSolid.Fuzz.Direction2d as Fuzz


frame2d : Fuzzer Frame2d
frame2d =
    let
        tuple =
            Fuzz.tuple ( Fuzz.point2d, Fuzz.direction2d )

        tupleToFrame ( originPoint, xDirection ) =
            Frame2d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = Direction2d.perpendicularTo xDirection
                }
    in
        Fuzz.map tupleToFrame tuple
