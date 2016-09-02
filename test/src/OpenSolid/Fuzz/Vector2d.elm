module OpenSolid.Fuzz.Vector2d exposing (vector2d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Types exposing (..)
import OpenSolid.Fuzz as Fuzz


vector2d : Fuzzer Vector2d
vector2d =
    Fuzz.map Vector2d (Fuzz.tuple ( Fuzz.scalar, Fuzz.scalar ))
