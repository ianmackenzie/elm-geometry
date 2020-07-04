module Curve3d exposing
    ( Curve3d
    , lineSegment, arc, ellipticalArc, quadraticSpline, cubicSpline
    , startPoint, endPoint
    , segments, approximate
    , reverse, translateBy, scaleAbout, rotateAround, mirrorAcross, projectOnto, projectInto
    , placeIn, relativeTo
    )

{-|

@docs Curve3d

@docs lineSegment, arc, ellipticalArc, quadraticSpline, cubicSpline, on

@docs startPoint, endPoint

@docs segments, approximate

@docs reverse, translateBy, scaleAbout, rotateAround, mirrorAcross, projectOnto, projectInto

@docs placeIn, relativeTo

-}

import Angle exposing (Angle)
import Arc3d exposing (Arc3d)
import Axis3d exposing (Axis3d)
import CubicSpline3d exposing (CubicSpline3d)
import Curve
import Curve2d exposing (Curve2d)
import Direction3d exposing (Direction3d)
import EllipticalArc3d exposing (EllipticalArc3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment3d exposing (LineSegment3d)
import Parameter1d
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity)
import SketchPlane3d exposing (SketchPlane3d)
import Vector3d exposing (Vector3d)


type alias Curve3d units coordinates =
    Types.Curve3d units coordinates


lineSegment : LineSegment3d units coordinates -> Curve3d units coordinates
lineSegment givenLineSegment =
    Types.LineSegmentCurve3d givenLineSegment


arc : Arc3d units coordinates -> Curve3d units coordinates
arc givenArc =
    Types.ArcCurve3d givenArc


ellipticalArc : EllipticalArc3d units coordinates -> Curve3d units coordinates
ellipticalArc givenEllipticalArc =
    Types.EllipticalArcCurve3d givenEllipticalArc


quadraticSpline : QuadraticSpline3d units coordinates -> Curve3d units coordinates
quadraticSpline givenQuadraticSpline =
    Types.QuadraticSplineCurve3d givenQuadraticSpline


cubicSpline : CubicSpline3d units coordinates -> Curve3d units coordinates
cubicSpline givenCubicSpline =
    Types.CubicSplineCurve3d givenCubicSpline


on :
    SketchPlane3d units coordinates { defines : coordinates2d }
    -> Curve2d units coordinates2d
    -> Curve3d units coordinates
on sketchPlane curve2d =
    case curve2d of
        Types.LineSegmentCurve2d lineSegment2d ->
            Types.LineSegmentCurve3d (LineSegment3d.on sketchPlane lineSegment2d)

        Types.ArcCurve2d arc2d ->
            Types.ArcCurve3d (Arc3d.on sketchPlane arc2d)

        Types.EllipticalArcCurve2d ellipticalArc2d ->
            Types.EllipticalArcCurve3d (EllipticalArc3d.on sketchPlane ellipticalArc2d)

        Types.QuadraticSplineCurve2d quadraticSpline2d ->
            Types.QuadraticSplineCurve3d (QuadraticSpline3d.on sketchPlane quadraticSpline2d)

        Types.CubicSplineCurve2d cubicSpline2d ->
            Types.CubicSplineCurve3d (CubicSpline3d.on sketchPlane cubicSpline2d)


startPoint : Curve3d units coordinates -> Point3d units coordinates
startPoint givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            LineSegment3d.startPoint givenLineSegment

        Types.ArcCurve3d givenArc ->
            Arc3d.startPoint givenArc

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            EllipticalArc3d.startPoint givenEllipticalArc

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            QuadraticSpline3d.startPoint givenQuadraticSpline

        Types.CubicSplineCurve3d givenCubicSpline ->
            CubicSpline3d.startPoint givenCubicSpline


endPoint : Curve3d units coordinates -> Point3d units coordinates
endPoint givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            LineSegment3d.endPoint givenLineSegment

        Types.ArcCurve3d givenArc ->
            Arc3d.endPoint givenArc

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            EllipticalArc3d.endPoint givenEllipticalArc

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            QuadraticSpline3d.endPoint givenQuadraticSpline

        Types.CubicSplineCurve3d givenCubicSpline ->
            CubicSpline3d.endPoint givenCubicSpline


segments : Int -> Curve3d units coordinates -> Polyline3d units coordinates
segments numSegments givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Polyline3d.fromVertices <|
                Parameter1d.steps numSegments (LineSegment3d.interpolate givenLineSegment)

        Types.ArcCurve3d givenArc ->
            Arc3d.segments numSegments givenArc

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            EllipticalArc3d.segments numSegments givenEllipticalArc

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            QuadraticSpline3d.segments numSegments givenQuadraticSpline

        Types.CubicSplineCurve3d givenCubicSpline ->
            CubicSpline3d.segments numSegments givenCubicSpline


approximate : Quantity Float units -> Curve3d units coordinates -> Polyline3d units coordinates
approximate maxError givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Polyline3d.fromVertices
                [ LineSegment3d.startPoint givenLineSegment
                , LineSegment3d.endPoint givenLineSegment
                ]

        Types.ArcCurve3d givenArc ->
            Arc3d.approximate maxError givenArc

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            EllipticalArc3d.approximate maxError givenEllipticalArc

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            QuadraticSpline3d.approximate maxError givenQuadraticSpline

        Types.CubicSplineCurve3d givenCubicSpline ->
            CubicSpline3d.approximate maxError givenCubicSpline


reverse : Curve3d units coordinates -> Curve3d units coordinates
reverse givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve3d (LineSegment3d.reverse givenLineSegment)

        Types.ArcCurve3d givenArc ->
            Types.ArcCurve3d (Arc3d.reverse givenArc)

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve3d (EllipticalArc3d.reverse givenEllipticalArc)

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve3d (QuadraticSpline3d.reverse givenQuadraticSpline)

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve3d (CubicSpline3d.reverse givenCubicSpline)


placeIn :
    Frame3d units coordinates { defines : localCoordinates }
    -> Curve3d units localCoordinates
    -> Curve3d units coordinates
placeIn frame givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve3d (LineSegment3d.placeIn frame givenLineSegment)

        Types.ArcCurve3d givenArc ->
            Types.ArcCurve3d (Arc3d.placeIn frame givenArc)

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve3d (EllipticalArc3d.placeIn frame givenEllipticalArc)

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve3d (QuadraticSpline3d.placeIn frame givenQuadraticSpline)

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve3d (CubicSpline3d.placeIn frame givenCubicSpline)


relativeTo :
    Frame3d units coordinates { defines : localCoordinates }
    -> Curve3d units coordinates
    -> Curve3d units localCoordinates
relativeTo frame givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve3d (LineSegment3d.relativeTo frame givenLineSegment)

        Types.ArcCurve3d givenArc ->
            Types.ArcCurve3d (Arc3d.relativeTo frame givenArc)

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve3d (EllipticalArc3d.relativeTo frame givenEllipticalArc)

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve3d (QuadraticSpline3d.relativeTo frame givenQuadraticSpline)

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve3d (CubicSpline3d.relativeTo frame givenCubicSpline)


translateBy : Vector3d units coordinates -> Curve3d units coordinates -> Curve3d units coordinates
translateBy displacement givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve3d (LineSegment3d.translateBy displacement givenLineSegment)

        Types.ArcCurve3d givenArc ->
            Types.ArcCurve3d (Arc3d.translateBy displacement givenArc)

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve3d (EllipticalArc3d.translateBy displacement givenEllipticalArc)

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve3d (QuadraticSpline3d.translateBy displacement givenQuadraticSpline)

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve3d (CubicSpline3d.translateBy displacement givenCubicSpline)


scaleAbout :
    Point3d units coordinates
    -> Float
    -> Curve3d units coordinates
    -> Curve3d units coordinates
scaleAbout point scale givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve3d <|
                LineSegment3d.scaleAbout point scale givenLineSegment

        Types.ArcCurve3d givenArc ->
            Types.ArcCurve3d <|
                Arc3d.scaleAbout point scale givenArc

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve3d <|
                EllipticalArc3d.scaleAbout point scale givenEllipticalArc

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve3d <|
                QuadraticSpline3d.scaleAbout point scale givenQuadraticSpline

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve3d <|
                CubicSpline3d.scaleAbout point scale givenCubicSpline


rotateAround :
    Axis3d units coordinates
    -> Angle
    -> Curve3d units coordinates
    -> Curve3d units coordinates
rotateAround axis angle givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve3d <|
                LineSegment3d.rotateAround axis angle givenLineSegment

        Types.ArcCurve3d givenArc ->
            Types.ArcCurve3d <|
                Arc3d.rotateAround axis angle givenArc

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve3d <|
                EllipticalArc3d.rotateAround axis angle givenEllipticalArc

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve3d <|
                QuadraticSpline3d.rotateAround axis angle givenQuadraticSpline

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve3d <|
                CubicSpline3d.rotateAround axis angle givenCubicSpline


mirrorAcross : Plane3d units coordinates -> Curve3d units coordinates -> Curve3d units coordinates
mirrorAcross plane givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve3d (LineSegment3d.mirrorAcross plane givenLineSegment)

        Types.ArcCurve3d givenArc ->
            Types.ArcCurve3d (Arc3d.mirrorAcross plane givenArc)

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve3d (EllipticalArc3d.mirrorAcross plane givenEllipticalArc)

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve3d (QuadraticSpline3d.mirrorAcross plane givenQuadraticSpline)

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve3d (CubicSpline3d.mirrorAcross plane givenCubicSpline)


projectOnto : Plane3d units coordinates -> Curve3d units coordinates -> Curve3d units coordinates
projectOnto plane givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve3d (LineSegment3d.projectOnto plane givenLineSegment)

        Types.ArcCurve3d givenArc ->
            Types.EllipticalArcCurve3d (Arc3d.projectOnto plane givenArc)

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve3d (EllipticalArc3d.projectOnto plane givenEllipticalArc)

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve3d (QuadraticSpline3d.projectOnto plane givenQuadraticSpline)

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve3d (CubicSpline3d.projectOnto plane givenCubicSpline)


projectInto : SketchPlane3d units coordinates { defines : coordinates2d } -> Curve3d units coordinates -> Curve2d units coordinates2d
projectInto sketchPlane givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve2d (LineSegment3d.projectInto sketchPlane givenLineSegment)

        Types.ArcCurve3d givenArc ->
            Types.EllipticalArcCurve2d (Arc3d.projectInto sketchPlane givenArc)

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve2d (EllipticalArc3d.projectInto sketchPlane givenEllipticalArc)

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d (QuadraticSpline3d.projectInto sketchPlane givenQuadraticSpline)

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve2d (CubicSpline3d.projectInto sketchPlane givenCubicSpline)
