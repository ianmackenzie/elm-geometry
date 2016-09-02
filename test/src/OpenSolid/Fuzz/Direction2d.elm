module OpenSolid.Fuzz.Direction2d exposing (direction2d)

import Fuzz exposing (Fuzzer)
import Random.Pcg as Random
import Shrink
import OpenSolid.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Fuzz as Fuzz


direction2d : Fuzzer Direction2d
direction2d =
    let
        componentGenerator =
            Random.float -2 2

        vectorGenerator =
            Random.map2 (\x y -> Vector2d ( x, y ))
                componentGenerator
                componentGenerator

        isValid vector =
            let
                length =
                    Vector2d.length vector
            in
                length >= 0.5 && length <= 2

        validVectorGenerator =
            Random.filter isValid vectorGenerator

        directionOf validVector =
            let
                normalizedVector =
                    Vector2d.times (1 / Vector2d.length validVector) validVector
            in
                Direction2d (Vector2d.components normalizedVector)

        directionGenerator =
            Random.map directionOf validVectorGenerator
    in
        Fuzz.custom directionGenerator Shrink.noShrink
