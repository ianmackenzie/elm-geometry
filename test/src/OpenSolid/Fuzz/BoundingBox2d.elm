module OpenSolid.Fuzz.BoundingBox2d exposing (boundingBox2d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Types exposing (..)
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.Fuzz as Fuzz
import OpenSolid.Fuzz.Interval as Fuzz


boundingBox2d : Fuzzer BoundingBox2d
boundingBox2d =
    let
        intervals =
            Fuzz.tuple ( Fuzz.interval, Fuzz.interval )

        intervalsToBoundingBox ( ( minX, maxX ), ( minY, maxY ) ) =
            BoundingBox2d { minX = minX, maxX = maxX, minY = minY, maxY = maxY }

        nonEmptyBoundingBox =
            Fuzz.map intervalsToBoundingBox intervals

        emptyBoundingBox =
            Fuzz.map (always BoundingBox2d.empty) Fuzz.unit
    in
        Fuzz.frequencyOrCrash
            [ ( 1, nonEmptyBoundingBox )
            , ( 1, emptyBoundingBox )
            ]
