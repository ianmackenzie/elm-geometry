module OpenSolid.Bootstrap.Direction3d exposing (components, unsafe)

import OpenSolid.Geometry.Internal exposing (..)


unsafe : ( Float, Float, Float ) -> Direction3d
unsafe =
    Direction3d


components : Direction3d -> ( Float, Float, Float )
components (Direction3d components_) =
    components_
