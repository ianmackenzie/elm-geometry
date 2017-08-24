module OpenSolid.Bootstrap.Direction2d
    exposing
        ( components
        , flip
        , perpendicularTo
        , unsafe
        )

import OpenSolid.Geometry.Internal exposing (..)


unsafe : ( Float, Float ) -> Direction2d
unsafe =
    Direction2d


components : Direction2d -> ( Float, Float )
components (Direction2d components_) =
    components_


flip : Direction2d -> Direction2d
flip direction =
    let
        ( x, y ) =
            components direction
    in
    unsafe ( -x, -y )


perpendicularTo : Direction2d -> Direction2d
perpendicularTo direction =
    let
        ( x, y ) =
            components direction
    in
    unsafe ( -y, x )
