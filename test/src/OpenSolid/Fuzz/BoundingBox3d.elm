module OpenSolid.Fuzz.BoundingBox3d exposing (boundingBox3d)

import Fuzz exposing (Fuzzer)
import OpenSolid.Types exposing (..)
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Fuzz as Fuzz
import OpenSolid.Fuzz.Interval as Fuzz


boundingBox3d : Fuzzer BoundingBox3d
boundingBox3d =
    let
        intervals =
            Fuzz.tuple3 ( Fuzz.interval, Fuzz.interval, Fuzz.interval )

        intervalsToBoundingBox ( xInterval, yInterval, zInterval ) =
            let
                ( minX, maxX ) =
                    xInterval

                ( minY, maxY ) =
                    yInterval

                ( minZ, maxZ ) =
                    zInterval
            in
                BoundingBox3d
                    { minX = minX
                    , maxX = maxX
                    , minY = minY
                    , maxY = maxY
                    , minZ = minZ
                    , maxZ = maxZ
                    }

        nonEmptyBoundingBox =
            Fuzz.map intervalsToBoundingBox intervals

        emptyBoundingBox =
            Fuzz.map (always BoundingBox3d.empty) Fuzz.unit
    in
        Fuzz.frequencyOrCrash
            [ ( 1, nonEmptyBoundingBox )
            , ( 1, emptyBoundingBox )
            ]
