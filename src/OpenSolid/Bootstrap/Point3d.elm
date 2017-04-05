module OpenSolid.Bootstrap.Point3d exposing (coordinates)

import OpenSolid.Geometry.Types exposing (..)


coordinates : Point3d -> ( Float, Float, Float )
coordinates (Point3d coordinates_) =
    coordinates_
