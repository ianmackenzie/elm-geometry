module OpenSolid.Bootstrap.Point2d exposing (coordinates, withCoordinates)

import OpenSolid.Geometry.Internal exposing (..)


withCoordinates : ( Float, Float ) -> Point2d
withCoordinates =
    Point2d


coordinates : Point2d -> ( Float, Float )
coordinates (Point2d coordinates_) =
    coordinates_
