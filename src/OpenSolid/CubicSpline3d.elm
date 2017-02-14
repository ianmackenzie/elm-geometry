module OpenSolid.CubicSpline3d
    exposing
        ( bezier
        , controlPoints
        )

import OpenSolid.Geometry.Types exposing (..)


bezier : Point3d -> Point3d -> Point3d -> Point3d -> CubicSpline3d
bezier firstPoint secondPoint thirdPoint fourthPoint =
    CubicSpline3d ( firstPoint, secondPoint, thirdPoint, fourthPoint )


controlPoints : CubicSpline3d -> ( Point3d, Point3d, Point3d, Point3d )
controlPoints (CubicSpline3d controlPoints_) =
    controlPoints_
