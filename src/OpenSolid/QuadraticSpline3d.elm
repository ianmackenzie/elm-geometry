module OpenSolid.QuadraticSpline3d
    exposing
        ( bezier
        , controlPoints
        )

import OpenSolid.Geometry.Types exposing (..)


bezier : Point3d -> Point3d -> Point3d -> QuadraticSpline3d
bezier firstPoint secondPoint thirdPoint =
    QuadraticSpline3d ( firstPoint, secondPoint, thirdPoint )


controlPoints : QuadraticSpline3d -> ( Point3d, Point3d, Point3d )
controlPoints (QuadraticSpline3d controlPoints_) =
    controlPoints_
