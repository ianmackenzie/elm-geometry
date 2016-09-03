module OpenSolid.Fuzz.Direction3d exposing (direction3d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Types exposing (..)


direction3d : Fuzzer Direction3d
direction3d =
    let
        thetaFuzzer =
            Fuzz.floatRange -pi pi

        t =
            Fuzz.floatRange -1 1

        phiFuzzer =
            Fuzz.map acos t

        direction ( theta, phi ) =
            Direction3d
                ( sin phi * cos theta
                , sin phi * sin theta
                , cos phi
                )
    in
        Fuzz.map direction (Fuzz.tuple ( thetaFuzzer, phiFuzzer ))
