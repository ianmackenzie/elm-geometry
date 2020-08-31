module Region2d exposing
    ( Region2d
    , triangle, rectangle, circle, ellipse, polygon, bounded, withHoles
    , approximate
    , translateBy, scaleAbout, rotateAround, mirrorAcross
    , relativeTo, placeIn
    , at, at_
    , boundingBox
    )

{-|

@docs Region2d

@docs triangle, rectangle, circle, ellipse, polygon, bounded, withHoles

@docs approximate

@docs translateBy, translateIn, scaleAbout, rotateAround, mirrorAcross

@docs relativeTo, placeIn

@docs at, at_

@docs boundingBox

-}

import Angle exposing (Angle)
import Arc2d
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import Curve2d exposing (Curve2d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity, Rate)
import Rectangle2d exposing (Rectangle2d)
import Triangle2d exposing (Triangle2d)
import Vector2d exposing (Vector2d)


type alias Region2d units coordinates =
    Types.Region2d units coordinates


empty : Region2d units coordinates
empty =
    Types.EmptyRegion


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


polygon : Polygon2d units coordinates -> Region2d units coordinates
polygon givenPolygon =
    Types.PolygonalRegion givenPolygon


bounded : List (Curve2d units coordinates) -> Region2d units coordinates
bounded loop =
    withHoles [] loop


withHoles :
    List (List (Curve2d units coordinates))
    -> List (Curve2d units coordinates)
    -> Region2d units coordinates
withHoles innerLoops outerLoop =
    Types.BoundedRegion outerLoop innerLoops


approximate : Quantity Float units -> Region2d units coordinates -> Polygon2d units coordinates
approximate maxError region =
    case region of
        Types.EmptyRegion ->
            Polygon2d.singleLoop []

        Types.TriangularRegion givenTriangle ->
            let
                ( p1, p2, p3 ) =
                    Triangle2d.vertices givenTriangle
            in
            Polygon2d.singleLoop [ p1, p2, p3 ]

        Types.RectangularRegion givenRectangle ->
            Polygon2d.singleLoop (Rectangle2d.vertices givenRectangle)

        Types.CircularRegion givenCircle ->
            Circle2d.toArc givenCircle
                |> Arc2d.approximate maxError
                |> Polyline2d.vertices
                |> List.drop 1
                |> Polygon2d.singleLoop

        Types.EllipticalRegion givenEllipse ->
            Ellipse2d.toEllipticalArc givenEllipse
                |> EllipticalArc2d.approximate maxError
                |> Polyline2d.vertices
                |> List.drop 1
                |> Polygon2d.singleLoop

        Types.PolygonalRegion givenPolygon ->
            givenPolygon

        Types.BoundedRegion outerLoop innerLoops ->
            Polygon2d.withHoles
                (List.map (approximateLoop maxError) innerLoops)
                (approximateLoop maxError outerLoop)


approximateLoop :
    Quantity Float units
    -> List (Curve2d units coordinates)
    -> List (Point2d units coordinates)
approximateLoop maxError curves =
    Curve2d.join True maxError curves
        |> List.map (Curve2d.approximate maxError >> Polyline2d.vertices >> List.drop 1)
        |> List.concat


translateBy : Vector2d units coordinates -> Region2d units coordinates -> Region2d units coordinates
translateBy displacement region =
    case region of
        Types.EmptyRegion ->
            Types.EmptyRegion

        Types.TriangularRegion givenTriangle ->
            Types.TriangularRegion (Triangle2d.translateBy displacement givenTriangle)

        Types.RectangularRegion givenRectangle ->
            Types.RectangularRegion (Rectangle2d.translateBy displacement givenRectangle)

        Types.CircularRegion givenCircle ->
            Types.CircularRegion (Circle2d.translateBy displacement givenCircle)

        Types.EllipticalRegion givenEllipse ->
            Types.EllipticalRegion (Ellipse2d.translateBy displacement givenEllipse)

        Types.PolygonalRegion givenPolygon ->
            Types.PolygonalRegion (Polygon2d.translateBy displacement givenPolygon)

        Types.BoundedRegion outerLoop innerLoops ->
            let
                translateCurve =
                    Curve2d.translateBy displacement

                translateLoop =
                    List.map translateCurve
            in
            Types.BoundedRegion (translateLoop outerLoop) (List.map translateLoop innerLoops)


scaleAbout : Point2d units coordinates -> Float -> Region2d units coordinates -> Region2d units coordinates
scaleAbout centerPoint scale region =
    case region of
        Types.EmptyRegion ->
            Types.EmptyRegion

        Types.TriangularRegion givenTriangle ->
            Types.TriangularRegion (Triangle2d.scaleAbout centerPoint scale givenTriangle)

        Types.RectangularRegion givenRectangle ->
            Types.RectangularRegion (Rectangle2d.scaleAbout centerPoint scale givenRectangle)

        Types.CircularRegion givenCircle ->
            Types.CircularRegion (Circle2d.scaleAbout centerPoint scale givenCircle)

        Types.EllipticalRegion givenEllipse ->
            Types.EllipticalRegion (Ellipse2d.scaleAbout centerPoint scale givenEllipse)

        Types.PolygonalRegion givenPolygon ->
            Types.PolygonalRegion (Polygon2d.scaleAbout centerPoint scale givenPolygon)

        Types.BoundedRegion outerLoop innerLoops ->
            let
                scaleCurve =
                    Curve2d.scaleAbout centerPoint scale

                scaleLoop =
                    List.map scaleCurve
            in
            Types.BoundedRegion (scaleLoop outerLoop) (List.map scaleLoop innerLoops)


rotateAround : Point2d units coordinates -> Angle -> Region2d units coordinates -> Region2d units coordinates
rotateAround centerPoint angle region =
    case region of
        Types.EmptyRegion ->
            Types.EmptyRegion

        Types.TriangularRegion givenTriangle ->
            Types.TriangularRegion (Triangle2d.rotateAround centerPoint angle givenTriangle)

        Types.RectangularRegion givenRectangle ->
            Types.RectangularRegion (Rectangle2d.rotateAround centerPoint angle givenRectangle)

        Types.CircularRegion givenCircle ->
            Types.CircularRegion (Circle2d.rotateAround centerPoint angle givenCircle)

        Types.EllipticalRegion givenEllipse ->
            Types.EllipticalRegion (Ellipse2d.rotateAround centerPoint angle givenEllipse)

        Types.PolygonalRegion givenPolygon ->
            Types.PolygonalRegion (Polygon2d.rotateAround centerPoint angle givenPolygon)

        Types.BoundedRegion outerLoop innerLoops ->
            let
                rotateCurve =
                    Curve2d.rotateAround centerPoint angle

                rotateLoop =
                    List.map rotateCurve
            in
            Types.BoundedRegion (rotateLoop outerLoop) (List.map rotateLoop innerLoops)


mirrorAcross : Axis2d units coordinates -> Region2d units coordinates -> Region2d units coordinates
mirrorAcross axis region =
    case region of
        Types.EmptyRegion ->
            Types.EmptyRegion

        Types.TriangularRegion givenTriangle ->
            Types.TriangularRegion (Triangle2d.mirrorAcross axis givenTriangle)

        Types.RectangularRegion givenRectangle ->
            Types.RectangularRegion (Rectangle2d.mirrorAcross axis givenRectangle)

        Types.CircularRegion givenCircle ->
            Types.CircularRegion (Circle2d.mirrorAcross axis givenCircle)

        Types.EllipticalRegion givenEllipse ->
            Types.EllipticalRegion (Ellipse2d.mirrorAcross axis givenEllipse)

        Types.PolygonalRegion givenPolygon ->
            Types.PolygonalRegion (Polygon2d.mirrorAcross axis givenPolygon)

        Types.BoundedRegion outerLoop innerLoops ->
            let
                mirrorCurve =
                    Curve2d.mirrorAcross axis

                mirrorLoop =
                    List.map mirrorCurve
            in
            Types.BoundedRegion (mirrorLoop outerLoop) (List.map mirrorLoop innerLoops)


relativeTo : Frame2d units coordinates { defines : localCoordinates } -> Region2d units coordinates -> Region2d units localCoordinates
relativeTo frame region =
    case region of
        Types.EmptyRegion ->
            Types.EmptyRegion

        Types.TriangularRegion givenTriangle ->
            Types.TriangularRegion (Triangle2d.relativeTo frame givenTriangle)

        Types.RectangularRegion givenRectangle ->
            Types.RectangularRegion (Rectangle2d.relativeTo frame givenRectangle)

        Types.CircularRegion givenCircle ->
            Types.CircularRegion (Circle2d.relativeTo frame givenCircle)

        Types.EllipticalRegion givenEllipse ->
            Types.EllipticalRegion (Ellipse2d.relativeTo frame givenEllipse)

        Types.PolygonalRegion givenPolygon ->
            Types.PolygonalRegion (Polygon2d.relativeTo frame givenPolygon)

        Types.BoundedRegion outerLoop innerLoops ->
            let
                localizeCurve =
                    Curve2d.relativeTo frame

                localizeLoop =
                    List.map localizeCurve
            in
            Types.BoundedRegion (localizeLoop outerLoop) (List.map localizeLoop innerLoops)


placeIn : Frame2d units coordinates { defines : localCoordinates } -> Region2d units localCoordinates -> Region2d units coordinates
placeIn frame region =
    case region of
        Types.EmptyRegion ->
            Types.EmptyRegion

        Types.TriangularRegion givenTriangle ->
            Types.TriangularRegion (Triangle2d.placeIn frame givenTriangle)

        Types.RectangularRegion givenRectangle ->
            Types.RectangularRegion (Rectangle2d.placeIn frame givenRectangle)

        Types.CircularRegion givenCircle ->
            Types.CircularRegion (Circle2d.placeIn frame givenCircle)

        Types.EllipticalRegion givenEllipse ->
            Types.EllipticalRegion (Ellipse2d.placeIn frame givenEllipse)

        Types.PolygonalRegion givenPolygon ->
            Types.PolygonalRegion (Polygon2d.placeIn frame givenPolygon)

        Types.BoundedRegion outerLoop innerLoops ->
            let
                globalizeCurve =
                    Curve2d.placeIn frame

                globalizeLoop =
                    List.map globalizeCurve
            in
            Types.BoundedRegion (globalizeLoop outerLoop) (List.map globalizeLoop innerLoops)


at : Quantity Float (Rate units2 units1) -> Region2d units1 coordinates -> Region2d units2 coordinates
at rate region =
    case region of
        Types.EmptyRegion ->
            Types.EmptyRegion

        Types.TriangularRegion givenTriangle ->
            Types.TriangularRegion (Triangle2d.at rate givenTriangle)

        Types.RectangularRegion givenRectangle ->
            Types.RectangularRegion (Rectangle2d.at rate givenRectangle)

        Types.CircularRegion givenCircle ->
            Types.CircularRegion (Circle2d.at rate givenCircle)

        Types.EllipticalRegion givenEllipse ->
            Types.EllipticalRegion (Ellipse2d.at rate givenEllipse)

        Types.PolygonalRegion givenPolygon ->
            Types.PolygonalRegion (Polygon2d.at rate givenPolygon)

        Types.BoundedRegion outerLoop innerLoops ->
            let
                convertCurveUnits =
                    Curve2d.at rate

                convertLoopUnits =
                    List.map convertCurveUnits
            in
            Types.BoundedRegion (convertLoopUnits outerLoop) (List.map convertLoopUnits innerLoops)


at_ : Quantity Float (Rate units2 units1) -> Region2d units2 coordinates -> Region2d units1 coordinates
at_ rate region =
    at (Quantity.inverse rate) region


boundingBox : Region2d units coordinates -> Maybe (BoundingBox2d units coordinates)
boundingBox region =
    case region of
        Types.EmptyRegion ->
            Nothing

        Types.TriangularRegion givenTriangle ->
            Just (Triangle2d.boundingBox givenTriangle)

        Types.RectangularRegion givenRectangle ->
            Just (Rectangle2d.boundingBox givenRectangle)

        Types.CircularRegion givenCircle ->
            Just (Circle2d.boundingBox givenCircle)

        Types.EllipticalRegion givenEllipse ->
            Just (Ellipse2d.boundingBox givenEllipse)

        Types.PolygonalRegion givenPolygon ->
            Polygon2d.boundingBox givenPolygon

        Types.BoundedRegion outerLoop _ ->
            BoundingBox2d.aggregateOfN Curve2d.boundingBox outerLoop
