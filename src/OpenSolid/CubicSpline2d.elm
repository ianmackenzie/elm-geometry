module OpenSolid.CubicSpline2d
    exposing
        ( CubicSpline2d
        , bezier
        , controlPoints
        )

import OpenSolid.Geometry.Types exposing (..)


type CubicSpline2d
    = CubicSpline2d ( Point2d, Point2d, Point2d, Point2d )


bezier : Point2d -> Point2d -> Point2d -> Point2d -> CubicSpline2d
bezier firstPoint secondPoint thirdPoint fourthPoint =
    CubicSpline2d ( firstPoint, secondPoint, thirdPoint, fourthPoint )


controlPoints : CubicSpline2d -> ( Point2d, Point2d, Point2d, Point2d )
controlPoints (CubicSpline2d controlPoints_) =
    controlPoints_
