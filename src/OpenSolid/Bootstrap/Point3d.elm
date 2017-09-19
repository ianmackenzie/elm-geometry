module OpenSolid.Bootstrap.Point3d exposing (coordinates, fromCoordinates)

import OpenSolid.Geometry.Internal exposing (..)


fromCoordinates : ( Float, Float, Float ) -> Point3d
fromCoordinates =
    Point3d


coordinates : Point3d -> ( Float, Float, Float )
coordinates (Point3d coordinates_) =
    coordinates_
