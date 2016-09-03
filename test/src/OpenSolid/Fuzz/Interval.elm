module OpenSolid.Fuzz.Interval exposing (interval)

import Fuzz exposing (Fuzzer)
import OpenSolid.Fuzz as Fuzz


interval : Fuzzer ( Float, Float )
interval =
    let
        tuple =
            Fuzz.tuple ( Fuzz.scalar, Fuzz.scalar )

        ordered ( firstValue, secondValue ) =
            if firstValue <= secondValue then
                ( firstValue, secondValue )
            else
                ( secondValue, firstValue )
    in
        Fuzz.map ordered tuple
