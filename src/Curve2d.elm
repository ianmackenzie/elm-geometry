module Curve2d exposing
    ( Curve2d
    , lineSegment, arc, quadraticSpline, cubicSpline
    , startPoint, endPoint
    , toPolyline
    , reverse, translateBy, scaleAbout, rotateAround, mirrorAcross
    , placeIn, relativeTo
    )

{-|

@docs Curve2d

@docs lineSegment, arc, quadraticSpline, cubicSpline

@docs startPoint, endPoint

@docs toPolyline

@docs reverse, translateBy, scaleAbout, rotateAround, mirrorAcross

@docs placeIn, relativeTo

-}

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Axis2d exposing (Axis2d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (maxSecondDerivativeMagnitude)
import Curve
import Direction2d exposing (Direction2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types
import LineSegment2d exposing (LineSegment2d)
import Parameter1d
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity)
import Vector2d exposing (Vector2d)


type alias Curve2d units coordinates =
    Types.Curve2d units coordinates


lineSegment : LineSegment2d units coordinates -> Curve2d units coordinates
lineSegment givenLineSegment =
    Types.LineSegmentCurve2d givenLineSegment


arc : Arc2d units coordinates -> Curve2d units coordinates
arc givenArc =
    Types.ArcCurve2d givenArc


quadraticSpline : QuadraticSpline2d units coordinates -> Curve2d units coordinates
quadraticSpline givenQuadraticSpline =
    Types.QuadraticSplineCurve2d givenQuadraticSpline


cubicSpline : CubicSpline2d units coordinates -> Curve2d units coordinates
cubicSpline givenCubicSpline =
    Types.CubicSplineCurve2d givenCubicSpline


startPoint : Curve2d units coordinates -> Point2d units coordinates
startPoint givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            LineSegment2d.startPoint givenLineSegment

        Types.ArcCurve2d givenArc ->
            Arc2d.startPoint givenArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            QuadraticSpline2d.startPoint givenQuadraticSpline

        Types.CubicSplineCurve2d givenCubicSpline ->
            CubicSpline2d.startPoint givenCubicSpline


endPoint : Curve2d units coordinates -> Point2d units coordinates
endPoint givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            LineSegment2d.endPoint givenLineSegment

        Types.ArcCurve2d givenArc ->
            Arc2d.endPoint givenArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            QuadraticSpline2d.endPoint givenQuadraticSpline

        Types.CubicSplineCurve2d givenCubicSpline ->
            CubicSpline2d.endPoint givenCubicSpline


toPolyline : { maxError : Quantity Float units } -> Curve2d units coordinates -> Polyline2d units coordinates
toPolyline tolerance givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            let
                ( p1, p2 ) =
                    LineSegment2d.endpoints givenLineSegment
            in
            Polyline2d.fromVertices [ p1, p2 ]

        Types.ArcCurve2d givenArc ->
            Arc2d.toPolyline tolerance givenArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            let
                secondDerivativeMagnitude =
                    Vector2d.length (QuadraticSpline2d.secondDerivative givenQuadraticSpline)
            in
            Polyline2d.fromVertices <|
                Parameter1d.steps
                    (Curve.numSegments tolerance secondDerivativeMagnitude)
                    (QuadraticSpline2d.pointOn givenQuadraticSpline)

        Types.CubicSplineCurve2d givenCubicSpline ->
            let
                maxSecondDerivativeMagnitude =
                    CubicSpline2d.maxSecondDerivativeMagnitude givenCubicSpline
            in
            Polyline2d.fromVertices <|
                Parameter1d.steps
                    (Curve.numSegments tolerance maxSecondDerivativeMagnitude)
                    (CubicSpline2d.pointOn givenCubicSpline)


reverse : Curve2d units coordinates -> Curve2d units coordinates
reverse givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d (LineSegment2d.reverse givenLineSegment)

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d (Arc2d.reverse givenArc)

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d (QuadraticSpline2d.reverse givenQuadraticSpline)

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d (CubicSpline2d.reverse givenCubicSpline)


placeIn :
    Frame2d units coordinates { defines : localCoordinates }
    -> Curve2d units localCoordinates
    -> Curve2d units coordinates
placeIn frame givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d (LineSegment2d.placeIn frame givenLineSegment)

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d (Arc2d.placeIn frame givenArc)

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d (QuadraticSpline2d.placeIn frame givenQuadraticSpline)

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d (CubicSpline2d.placeIn frame givenCubicSpline)


relativeTo :
    Frame2d units coordinates { defines : localCoordinates }
    -> Curve2d units coordinates
    -> Curve2d units localCoordinates
relativeTo frame givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d (LineSegment2d.relativeTo frame givenLineSegment)

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d (Arc2d.relativeTo frame givenArc)

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d (QuadraticSpline2d.relativeTo frame givenQuadraticSpline)

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d (CubicSpline2d.relativeTo frame givenCubicSpline)


translateBy : Vector2d units coordinates -> Curve2d units coordinates -> Curve2d units coordinates
translateBy displacement givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d (LineSegment2d.translateBy displacement givenLineSegment)

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d (Arc2d.translateBy displacement givenArc)

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d (QuadraticSpline2d.translateBy displacement givenQuadraticSpline)

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d (CubicSpline2d.translateBy displacement givenCubicSpline)


scaleAbout :
    Point2d units coordinates
    -> Float
    -> Curve2d units coordinates
    -> Curve2d units coordinates
scaleAbout point scale givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d <|
                LineSegment2d.scaleAbout point scale givenLineSegment

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d <|
                Arc2d.scaleAbout point scale givenArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d <|
                QuadraticSpline2d.scaleAbout point scale givenQuadraticSpline

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d <|
                CubicSpline2d.scaleAbout point scale givenCubicSpline


rotateAround :
    Point2d units coordinates
    -> Angle
    -> Curve2d units coordinates
    -> Curve2d units coordinates
rotateAround point angle givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d <|
                LineSegment2d.rotateAround point angle givenLineSegment

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d <|
                Arc2d.rotateAround point angle givenArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d <|
                QuadraticSpline2d.rotateAround point angle givenQuadraticSpline

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d <|
                CubicSpline2d.rotateAround point angle givenCubicSpline


mirrorAcross : Axis2d units coordinates -> Curve2d units coordinates -> Curve2d units coordinates
mirrorAcross axis givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d (LineSegment2d.mirrorAcross axis givenLineSegment)

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d (Arc2d.mirrorAcross axis givenArc)

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d (QuadraticSpline2d.mirrorAcross axis givenQuadraticSpline)

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d (CubicSpline2d.mirrorAcross axis givenCubicSpline)
