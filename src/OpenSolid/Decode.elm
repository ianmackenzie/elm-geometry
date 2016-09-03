module OpenSolid.Decode exposing (extremum)

import Json.Decode as Decode exposing (Decoder)


extremum : Decoder Float
extremum =
    let
        parse string =
            case string of
                "NaN" ->
                    Ok (0 / 0)

                "Infinity" ->
                    Ok (1 / 0)

                "-Infinity" ->
                    Ok (-1 / 0)

                _ ->
                    Err "Expected 'NaN', 'Infinity' or '-Infinity'"

        specialValueDecoder =
            Decode.customDecoder Decode.string parse
    in
        Decode.oneOf [ specialValueDecoder, Decode.float ]
