module OpenSolid.QuadraticSpline3d
    exposing
        ( QuadraticSpline3d
        , bezier
        , controlPoints
        )

import OpenSolid.Geometry.Types exposing (..)


type QuadraticSpline3d
    = QuadraticSpline3d ( Point3d, Point3d, Point3d )


bezier : Point3d -> Point3d -> Point3d -> QuadraticSpline3d
bezier firstPoint secondPoint thirdPoint =
    QuadraticSpline3d ( firstPoint, secondPoint, thirdPoint )


controlPoints : QuadraticSpline3d -> ( Point3d, Point3d, Point3d )
controlPoints (QuadraticSpline3d controlPoints_) =
    controlPoints_
