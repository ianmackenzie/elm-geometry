module OpenSolid.Bootstrap.QuadraticSpline3d exposing (withControlPoints)

import OpenSolid.Geometry.Types exposing (..)


withControlPoints : ( Point3d, Point3d, Point3d ) -> QuadraticSpline3d
withControlPoints =
    QuadraticSpline3d
