module OpenSolid.Fuzz.SketchPlane3d exposing (sketchPlane3d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Fuzz.Point3d as Fuzz
import OpenSolid.Fuzz.Direction3d as Fuzz


sketchPlane3d : Fuzzer SketchPlane3d
sketchPlane3d =
    let
        tuple =
            Fuzz.tuple ( Fuzz.point3d, Fuzz.direction3d )

        tupleToFrame ( originPoint, xDirection ) =
            SketchPlane3d
                { originPoint = originPoint
                , xDirection = xDirection
                , yDirection = Direction3d.perpendicularTo xDirection
                }
    in
        Fuzz.map tupleToFrame tuple
