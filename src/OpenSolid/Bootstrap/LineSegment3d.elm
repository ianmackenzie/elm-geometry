module OpenSolid.Bootstrap.LineSegment3d exposing (withEndpoints)

import OpenSolid.Geometry.Internal exposing (..)


withEndpoints : ( Point3d, Point3d ) -> LineSegment3d
withEndpoints =
    LineSegment3d
