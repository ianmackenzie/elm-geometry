module OpenSolid.Bootstrap.Polyline3d exposing (withVertices)

import OpenSolid.Geometry.Internal exposing (..)


withVertices : List Point3d -> Polyline3d
withVertices =
    Polyline3d
