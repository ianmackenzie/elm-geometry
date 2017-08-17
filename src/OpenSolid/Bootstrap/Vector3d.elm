module OpenSolid.Bootstrap.Vector3d exposing (components, withComponents)

import OpenSolid.Geometry.Types exposing (..)


withComponents : ( Float, Float, Float ) -> Vector3d
withComponents =
    Vector3d


components : Vector3d -> ( Float, Float, Float )
components (Vector3d components_) =
    components_
