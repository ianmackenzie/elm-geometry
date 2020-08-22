module Region2d exposing
    ( Region2d
    , triangle, rectangle, circle, ellipse, bounded
    , approximate
    , translateBy, scaleAbout, rotateAround, mirrorAcross
    , relativeTo, placeIn
    , boundingBox
    )

{-|

@docs Region2d

@docs triangle, rectangle, circle, ellipse, bounded

@docs approximate

@docs translateBy, translateIn, scaleAbout, rotateAround, mirrorAcross

@docs relativeTo, placeIn

-}

import Circle2d exposing (Circle2d)
import Curve2d exposing (Curve2d)
import Ellipse2d exposing (Ellipse2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Triangle2d exposing (Triangle2d)
import Vector2d exposing (Vector2d)


type alias Region2d units coordinates =
    Types.Region2d units coordinates


triangle : Triangle2d units coordinates -> Region2d units coordinates
triangle givenTriangle =
    Types.TriangularRegion givenTriangle


rectangle : Rectangle2d units coordinates -> Region2d units coordinates
rectangle givenRectangle =
    Types.RectangularRegion givenRectangle


circle : Circle2d units coordinates -> Region2d units coordinates
circle givenCircle =
    Types.CircularRegion givenCircle


ellipse : Ellipse2d units coordinates -> Region2d units coordinates
ellipse givenEllipse =
    Types.EllipticalRegion givenEllipse


bounded :
    { outerLoop : List (Curve2d units coordinates)
    , innerLoops : List (List (Curve2d units coordinates))
    }
    -> Region2d units coordinates
bounded { outerLoop, innerLoops } =
    Types.BoundedRegion outerLoop innerLoops
