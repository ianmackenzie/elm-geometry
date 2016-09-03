module OpenSolid.Fuzz.Direction2d exposing (direction2d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Direction2d as Direction2d


direction2d : Fuzzer Direction2d
direction2d =
    Fuzz.map Direction2d.fromAngle (Fuzz.floatRange -pi pi)
