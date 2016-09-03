module OpenSolid.Fuzz.Point2d exposing (point2d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Fuzz as Fuzz


point2d : Fuzzer Point2d
point2d =
    Fuzz.map Point2d (Fuzz.tuple ( Fuzz.scalar, Fuzz.scalar ))
