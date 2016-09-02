module OpenSolid.Fuzz.Direction3d exposing (direction3d)

import Fuzz exposing (Fuzzer)
import Random.Pcg as Random
import Shrink
import OpenSolid.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Fuzz as Fuzz


direction3d : Fuzzer Direction3d
direction3d =
    let
        componentGenerator =
            Random.float -2 2

        vectorGenerator =
            Random.map3 (\x y z -> Vector3d ( x, y, z ))
                componentGenerator
                componentGenerator
                componentGenerator

        isValid vector =
            let
                length =
                    Vector3d.length vector
            in
                length >= 0.5 && length <= 2

        validVectorGenerator =
            Random.filter isValid vectorGenerator

        directionOf validVector =
            let
                normalizedVector =
                    Vector3d.times (1 / Vector3d.length validVector) validVector
            in
                Direction3d (Vector3d.components normalizedVector)

        directionGenerator =
            Random.map directionOf validVectorGenerator
    in
        Fuzz.custom directionGenerator Shrink.noShrink
