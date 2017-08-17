module OpenSolid.Bootstrap.Point3d exposing (coordinates, withCoordinates)

import OpenSolid.Geometry.Types exposing (..)


withCoordinates : ( Float, Float, Float ) -> Point3d
withCoordinates =
    Point3d


coordinates : Point3d -> ( Float, Float, Float )
coordinates (Point3d coordinates_) =
    coordinates_
