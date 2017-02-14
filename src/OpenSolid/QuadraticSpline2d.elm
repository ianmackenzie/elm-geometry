module OpenSolid.QuadraticSpline2d
    exposing
        ( bezier
        , controlPoints
        )

import OpenSolid.Geometry.Types exposing (..)


bezier : Point2d -> Point2d -> Point2d -> QuadraticSpline2d
bezier firstPoint secondPoint thirdPoint =
    QuadraticSpline2d ( firstPoint, secondPoint, thirdPoint )


controlPoints : QuadraticSpline2d -> ( Point2d, Point2d, Point2d )
controlPoints (QuadraticSpline2d controlPoints_) =
    controlPoints_
