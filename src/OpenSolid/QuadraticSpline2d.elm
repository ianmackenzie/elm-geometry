module OpenSolid.QuadraticSpline2d
    exposing
        ( QuadraticSpline2d
        , bezier
        , controlPoints
        )

import OpenSolid.Geometry.Types exposing (..)


type QuadraticSpline2d
    = QuadraticSpline2d ( Point2d, Point2d, Point2d )


bezier : Point2d -> Point2d -> Point2d -> QuadraticSpline2d
bezier firstPoint secondPoint thirdPoint =
    QuadraticSpline2d ( firstPoint, secondPoint, thirdPoint )


controlPoints : QuadraticSpline2d -> ( Point2d, Point2d, Point2d )
controlPoints (QuadraticSpline2d controlPoints_) =
    controlPoints_
