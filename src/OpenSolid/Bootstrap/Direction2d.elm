module OpenSolid.Bootstrap.Direction2d
    exposing
        ( components
        , flip
        , perpendicularTo
        )

import OpenSolid.Geometry.Types exposing (..)


components : Direction2d -> ( Float, Float )
components (Direction2d components_) =
    components_


flip : Direction2d -> Direction2d
flip direction =
    let
        ( x, y ) =
            components direction
    in
        Direction2d ( -x, -y )


perpendicularTo : Direction2d -> Direction2d
perpendicularTo direction =
    let
        ( x, y ) =
            components direction
    in
        Direction2d ( -y, x )
