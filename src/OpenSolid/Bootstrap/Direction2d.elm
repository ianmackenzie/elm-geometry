module OpenSolid.Bootstrap.Direction2d
    exposing
        ( components
        , flip
        , perpendicularTo
        , withComponents
        )

import OpenSolid.Geometry.Internal exposing (..)


withComponents : ( Float, Float ) -> Direction2d
withComponents =
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
    withComponents ( -x, -y )


perpendicularTo : Direction2d -> Direction2d
perpendicularTo direction =
    let
        ( x, y ) =
            components direction
    in
    withComponents ( -y, x )
