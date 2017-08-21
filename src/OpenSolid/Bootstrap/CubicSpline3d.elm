module OpenSolid.Bootstrap.CubicSpline3d exposing (withControlPoints)

import OpenSolid.Geometry.Internal exposing (..)


withControlPoints : ( Point3d, Point3d, Point3d, Point3d ) -> CubicSpline3d
withControlPoints =
    CubicSpline3d
