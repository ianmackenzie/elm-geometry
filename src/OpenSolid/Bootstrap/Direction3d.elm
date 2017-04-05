module OpenSolid.Bootstrap.Direction3d exposing (components)

import OpenSolid.Geometry.Types exposing (..)


components : Direction3d -> ( Float, Float, Float )
components (Direction3d components_) =
    components_
