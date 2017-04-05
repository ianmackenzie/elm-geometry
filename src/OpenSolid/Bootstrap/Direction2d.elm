module OpenSolid.Bootstrap.Direction2d exposing (components)

import OpenSolid.Geometry.Types exposing (..)


components : Direction2d -> ( Float, Float )
components (Direction2d components_) =
    components_
