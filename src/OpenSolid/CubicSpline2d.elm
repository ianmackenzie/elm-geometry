module OpenSolid.CubicSpline2d
    exposing
        ( bezier
        , controlPoints
        )

import OpenSolid.Geometry.Types exposing (..)


bezier : Point2d -> Point2d -> Point2d -> Point2d -> CubicSpline2d
bezier firstPoint secondPoint thirdPoint fourthPoint =
    CubicSpline2d ( firstPoint, secondPoint, thirdPoint, fourthPoint )


controlPoints : CubicSpline2d -> ( Point2d, Point2d, Point2d, Point2d )
controlPoints (CubicSpline2d controlPoints_) =
    controlPoints_
