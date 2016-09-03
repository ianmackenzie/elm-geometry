module OpenSolid.Fuzz.Vector3d exposing (vector3d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Fuzz as Fuzz


vector3d : Fuzzer Vector3d
vector3d =
    Fuzz.map Vector3d (Fuzz.tuple3 ( Fuzz.scalar, Fuzz.scalar, Fuzz.scalar ))
