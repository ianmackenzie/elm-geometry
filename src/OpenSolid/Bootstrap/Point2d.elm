module OpenSolid.Bootstrap.Point2d exposing (coordinates)

import OpenSolid.Geometry.Types exposing (..)


coordinates : Point2d -> ( Float, Float )
coordinates (Point2d coordinates_) =
    coordinates_
