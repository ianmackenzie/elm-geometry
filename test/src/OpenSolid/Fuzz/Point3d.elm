module OpenSolid.Fuzz.Point3d exposing (point3d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Types exposing (..)
import OpenSolid.Fuzz as Fuzz


point3d : Fuzzer Point3d
point3d =
    Fuzz.map Point3d (Fuzz.tuple3 ( Fuzz.scalar, Fuzz.scalar, Fuzz.scalar ))
