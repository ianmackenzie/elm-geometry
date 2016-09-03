module OpenSolid.Encode exposing (extremum)

import Json.Encode as Encode exposing (Value)


extremum : Float -> Value
extremum value =
    if isNaN value then
        Encode.string "NaN"
    else if value == 1 / 0 then
        Encode.string "Infinity"
    else if value == -1 / 0 then
        Encode.string "-Infinity"
    else
        Encode.float value
