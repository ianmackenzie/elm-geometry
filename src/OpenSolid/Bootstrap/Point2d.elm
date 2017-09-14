module OpenSolid.Bootstrap.Point2d exposing (coordinates, fromCoordinates)

import OpenSolid.Geometry.Internal exposing (..)


fromCoordinates : ( Float, Float ) -> Point2d
fromCoordinates =
    Point2d


coordinates : Point2d -> ( Float, Float )
coordinates (Point2d coordinates_) =
    coordinates_
