module Curve3d exposing
    ( Curve3d
    , lineSegment, arc, circle, ellipticalArc, ellipse, quadraticSpline, cubicSpline
    , startPoint, endPoint
    , segments, approximate, samples
    , join
    , reverse, translateBy, scaleAbout, rotateAround, mirrorAcross, projectOnto, projectInto
    , placeIn, relativeTo
    , at, at_
    , boundingBox
    , numApproximationSegments
    )

{-|

@docs Curve3d

@docs lineSegment, arc, circle, ellipticalArc, ellipse, quadraticSpline, cubicSpline, on

@docs startPoint, endPoint

@docs segments, approximate, samples

@docs join

@docs reverse, translateBy, scaleAbout, rotateAround, mirrorAcross, projectOnto, projectInto

@docs placeIn, relativeTo

@docs at, at_

@docs boundingBox

@docs numApproximationSegments

-}

import Angle exposing (Angle)
import Arc3d exposing (Arc3d)
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Circle3d exposing (Circle3d)
import CubicSpline3d exposing (CubicSpline3d)
import Curve
import Curve2d exposing (Curve2d)
import Direction3d exposing (Direction3d)
import Ellipse3d exposing (Ellipse3d)
import EllipticalArc3d exposing (EllipticalArc3d)
import Frame3d exposing (Frame3d)
import Geometry.Types as Types
import LineSegment3d exposing (LineSegment3d)
import Parameter1d
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity, Rate)
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


circle : Circle3d units coordinates -> Curve3d units coordinates
circle givenCircle =
    arc (Circle3d.toArc givenCircle)


ellipticalArc : EllipticalArc3d units coordinates -> Curve3d units coordinates
ellipticalArc givenEllipticalArc =
    Types.EllipticalArcCurve3d givenEllipticalArc


ellipse : Ellipse3d units coordinates -> Curve3d units coordinates
ellipse givenEllipse =
    ellipticalArc (Ellipse3d.toEllipticalArc givenEllipse)


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


samples : Quantity Float units -> Curve3d units coordinates -> List ( Point3d units coordinates, Direction3d coordinates )
samples maxError givenCurve =
    let
        numSegments =
            numApproximationSegments maxError givenCurve
    in
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            case LineSegment3d.direction givenLineSegment of
                Just direction ->
                    [ ( LineSegment3d.startPoint givenLineSegment, direction )
                    , ( LineSegment3d.endPoint givenLineSegment, direction )
                    ]

                Nothing ->
                    []

        Types.ArcCurve3d givenArc ->
            case Arc3d.nondegenerate givenArc of
                Ok nondegenerateArc ->
                    Parameter1d.steps numSegments (Arc3d.sample nondegenerateArc)

                Err _ ->
                    []

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            case EllipticalArc3d.nondegenerate givenEllipticalArc of
                Ok nondegenerateEllipticalArc ->
                    Parameter1d.steps numSegments
                        (EllipticalArc3d.sample nondegenerateEllipticalArc)

                Err _ ->
                    []

        Types.QuadraticSplineCurve3d givenSpline ->
            case QuadraticSpline3d.nondegenerate givenSpline of
                Ok nondegenerateSpline ->
                    Parameter1d.steps numSegments
                        (QuadraticSpline3d.sample nondegenerateSpline)

                Err _ ->
                    []

        Types.CubicSplineCurve3d givenSpline ->
            case CubicSpline3d.nondegenerate givenSpline of
                Ok nondegenerateSpline ->
                    Parameter1d.steps numSegments
                        (CubicSpline3d.sample nondegenerateSpline)

                Err _ ->
                    []


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


at :
    Quantity Float (Rate units2 units1)
    -> Curve3d units1 coordinates
    -> Curve3d units2 coordinates
at rate givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            Types.LineSegmentCurve3d (LineSegment3d.at rate givenLineSegment)

        Types.ArcCurve3d givenArc ->
            Types.ArcCurve3d (Arc3d.at rate givenArc)

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            Types.EllipticalArcCurve3d (EllipticalArc3d.at rate givenEllipticalArc)

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            Types.QuadraticSplineCurve3d (QuadraticSpline3d.at rate givenQuadraticSpline)

        Types.CubicSplineCurve3d givenCubicSpline ->
            Types.CubicSplineCurve3d (CubicSpline3d.at rate givenCubicSpline)


at_ :
    Quantity Float (Rate units2 units1)
    -> Curve3d units2 coordinates
    -> Curve3d units1 coordinates
at_ rate givenCurve =
    at (Quantity.inverse rate) givenCurve


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


boundingBox : Curve3d units coordinates -> BoundingBox3d units coordinates
boundingBox givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d givenLineSegment ->
            LineSegment3d.boundingBox givenLineSegment

        Types.ArcCurve3d givenArc ->
            Arc3d.boundingBox givenArc

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            EllipticalArc3d.boundingBox givenEllipticalArc

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            QuadraticSpline3d.boundingBox givenQuadraticSpline

        Types.CubicSplineCurve3d givenCubicSpline ->
            CubicSpline3d.boundingBox givenCubicSpline


numApproximationSegments : Quantity Float units -> Curve3d units coordinates -> Int
numApproximationSegments maxError givenCurve =
    case givenCurve of
        Types.LineSegmentCurve3d _ ->
            1

        Types.ArcCurve3d givenArc ->
            Arc3d.numApproximationSegments maxError givenArc

        Types.EllipticalArcCurve3d givenEllipticalArc ->
            EllipticalArc3d.numApproximationSegments maxError givenEllipticalArc

        Types.QuadraticSplineCurve3d givenQuadraticSpline ->
            QuadraticSpline3d.numApproximationSegments maxError givenQuadraticSpline

        Types.CubicSplineCurve3d givenCubicSpline ->
            CubicSpline3d.numApproximationSegments maxError givenCubicSpline


join : Bool -> Quantity Float units -> List (Curve3d units coordinates) -> List (Curve3d units coordinates)
join close resolution givenCurves =
    case givenCurves of
        [] ->
            []

        [ givenCurve ] ->
            if close then
                -- Join end point back to start point if necessary
                let
                    p1 =
                        startPoint givenCurve

                    p2 =
                        endPoint givenCurve
                in
                if Point3d.equalWithin resolution p1 p2 then
                    -- Curve already loops back on itself, nothing to do
                    [ givenCurve ]

                else
                    -- Add a line from the end point back to the start point
                    [ givenCurve, lineSegment (LineSegment3d.from p2 p1) ]

            else
                [ givenCurve ]

        first :: second :: rest ->
            let
                firstStart =
                    startPoint first

                firstEnd =
                    endPoint first

                secondStart =
                    startPoint second

                secondEnd =
                    endPoint second
            in
            if
                Point3d.equalWithin resolution firstEnd secondStart
                    || Point3d.equalWithin resolution firstEnd secondEnd
            then
                -- First segment is oriented correctly (end point touches
                -- second segment)
                merge close resolution firstStart firstEnd (second :: rest) [ first ]

            else if
                Point3d.equalWithin resolution firstStart secondStart
                    || Point3d.equalWithin resolution firstStart secondEnd
            then
                -- First segment needs to be reversed (*start* point touches
                -- second segment)
                merge close resolution firstEnd firstStart (second :: rest) [ reverse first ]

            else
                -- First two segments are separated, assume they are correctly
                -- oriented
                merge close resolution firstStart firstEnd (second :: rest) [ first ]


merge :
    Bool
    -> Quantity Float units
    -> Point3d units coordinates
    -> Point3d units coordinates
    -> List (Curve3d units coordinates)
    -> List (Curve3d units coordinates)
    -> List (Curve3d units coordinates)
merge close maxError firstStart previousEnd remainingSegments accumulated =
    case remainingSegments of
        next :: following ->
            let
                nextStart =
                    startPoint next

                nextEnd =
                    endPoint next
            in
            if Point3d.equalWithin maxError previousEnd nextStart then
                -- Next segment is oriented correctly
                merge close maxError firstStart nextEnd following (next :: accumulated)

            else if Point3d.equalWithin maxError previousEnd nextEnd then
                -- Next segment needs to be reversed
                merge close maxError firstStart nextStart following (reverse next :: accumulated)

            else
                -- Segments have a gap between them, assume next segment is
                -- correctly oriented and add a connecting line between them
                let
                    connector =
                        lineSegment (LineSegment3d.from previousEnd nextStart)
                in
                merge close maxError firstStart nextEnd following (next :: connector :: accumulated)

        [] ->
            let
                -- Add a final line segment joining end of last curve back to
                -- start of first curve, if necessary
                finalCurves =
                    if close && not (Point3d.equalWithin maxError previousEnd firstStart) then
                        lineSegment (LineSegment3d.from previousEnd firstStart) :: accumulated

                    else
                        accumulated
            in
            List.reverse finalCurves
