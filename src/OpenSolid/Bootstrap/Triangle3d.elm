module OpenSolid.Bootstrap.Triangle3d exposing (withVertices)

import OpenSolid.Geometry.Internal exposing (..)


withVertices : ( Point3d, Point3d, Point3d ) -> Triangle3d
withVertices =
    Triangle3d
