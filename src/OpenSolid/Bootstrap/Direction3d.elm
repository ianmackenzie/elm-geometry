module OpenSolid.Bootstrap.Direction3d exposing (components, withComponents)

import OpenSolid.Geometry.Types exposing (..)


withComponents : ( Float, Float, Float ) -> Direction3d
withComponents =
    Direction3d


components : Direction3d -> ( Float, Float, Float )
components (Direction3d components_) =
    components_
