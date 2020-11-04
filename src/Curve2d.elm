module Curve2d exposing
    ( Curve2d
    , lineSegment, arc, circle, ellipticalArc, ellipse, quadraticSpline, cubicSpline
    , startPoint, endPoint
    , segments, approximate, samples
    , join
    , reverse, translateBy, scaleAbout, rotateAround, mirrorAcross
    , placeIn, relativeTo
    , at, at_
    , boundingBox
    , numApproximationSegments
    )

{-|

@docs Curve2d

@docs lineSegment, arc, circle, ellipticalArc, ellipse, quadraticSpline, cubicSpline

@docs startPoint, endPoint

@docs segments, approximate, samples

@docs join

@docs reverse, translateBy, scaleAbout, rotateAround, mirrorAcross

@docs placeIn, relativeTo

@docs at, at_

@docs boundingBox

@docs numApproximationSegments

-}

import Angle exposing (Angle)
import Arc2d exposing (Arc2d)
import Axis2d exposing (Axis2d)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d exposing (Circle2d)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Curve
import Direction2d exposing (Direction2d)
import Ellipse2d exposing (Ellipse2d)
import EllipticalArc2d exposing (EllipticalArc2d)
import Frame2d exposing (Frame2d)
import Geometry.Types as Types exposing (LineSegment2d)
import LineSegment2d exposing (LineSegment2d)
import Parameter1d
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import Quantity exposing (Quantity, Rate)
import Vector2d exposing (Vector2d)


type alias Curve2d units coordinates =
    Types.Curve2d units coordinates


lineSegment : LineSegment2d units coordinates -> Curve2d units coordinates
lineSegment givenLineSegment =
    Types.LineSegmentCurve2d givenLineSegment


arc : Arc2d units coordinates -> Curve2d units coordinates
arc givenArc =
    Types.ArcCurve2d givenArc


circle : Circle2d units coordinates -> Curve2d units coordinates
circle givenCircle =
    arc (Circle2d.toArc givenCircle)


ellipticalArc : EllipticalArc2d units coordinates -> Curve2d units coordinates
ellipticalArc givenEllipticalArc =
    Types.EllipticalArcCurve2d givenEllipticalArc


ellipse : Ellipse2d units coordinates -> Curve2d units coordinates
ellipse givenEllipse =
    ellipticalArc (Ellipse2d.toEllipticalArc givenEllipse)


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

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            EllipticalArc2d.startPoint givenEllipticalArc

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

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            EllipticalArc2d.endPoint givenEllipticalArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            QuadraticSpline2d.endPoint givenQuadraticSpline

        Types.CubicSplineCurve2d givenCubicSpline ->
            CubicSpline2d.endPoint givenCubicSpline


segments : Int -> Curve2d units coordinates -> Polyline2d units coordinates
segments numSegments givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Polyline2d.fromVertices <|
                Parameter1d.steps numSegments (LineSegment2d.interpolate givenLineSegment)

        Types.ArcCurve2d givenArc ->
            Arc2d.segments numSegments givenArc

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            EllipticalArc2d.segments numSegments givenEllipticalArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            QuadraticSpline2d.segments numSegments givenQuadraticSpline

        Types.CubicSplineCurve2d givenCubicSpline ->
            CubicSpline2d.segments numSegments givenCubicSpline


approximate : Quantity Float units -> Curve2d units coordinates -> Polyline2d units coordinates
approximate maxError givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Polyline2d.fromVertices
                [ LineSegment2d.startPoint givenLineSegment
                , LineSegment2d.endPoint givenLineSegment
                ]

        Types.ArcCurve2d givenArc ->
            Arc2d.approximate maxError givenArc

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            EllipticalArc2d.approximate maxError givenEllipticalArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            QuadraticSpline2d.approximate maxError givenQuadraticSpline

        Types.CubicSplineCurve2d givenCubicSpline ->
            CubicSpline2d.approximate maxError givenCubicSpline


samples : Quantity Float units -> Curve2d units coordinates -> List ( Point2d units coordinates, Direction2d coordinates )
samples maxError givenCurve =
    let
        numSegments =
            numApproximationSegments maxError givenCurve
    in
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            case LineSegment2d.direction givenLineSegment of
                Just direction ->
                    [ ( LineSegment2d.startPoint givenLineSegment, direction )
                    , ( LineSegment2d.endPoint givenLineSegment, direction )
                    ]

                Nothing ->
                    []

        Types.ArcCurve2d givenArc ->
            case Arc2d.nondegenerate givenArc of
                Ok nondegenerateArc ->
                    Parameter1d.steps numSegments (Arc2d.sample nondegenerateArc)

                Err _ ->
                    []

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            case EllipticalArc2d.nondegenerate givenEllipticalArc of
                Ok nondegenerateEllipticalArc ->
                    Parameter1d.steps numSegments
                        (EllipticalArc2d.sample nondegenerateEllipticalArc)

                Err _ ->
                    []

        Types.QuadraticSplineCurve2d givenSpline ->
            case QuadraticSpline2d.nondegenerate givenSpline of
                Ok nondegenerateSpline ->
                    Parameter1d.steps numSegments
                        (QuadraticSpline2d.sample nondegenerateSpline)

                Err _ ->
                    []

        Types.CubicSplineCurve2d givenSpline ->
            case CubicSpline2d.nondegenerate givenSpline of
                Ok nondegenerateSpline ->
                    Parameter1d.steps numSegments
                        (CubicSpline2d.sample nondegenerateSpline)

                Err _ ->
                    []


reverse : Curve2d units coordinates -> Curve2d units coordinates
reverse givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d (LineSegment2d.reverse givenLineSegment)

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d (Arc2d.reverse givenArc)

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            Types.EllipticalArcCurve2d (EllipticalArc2d.reverse givenEllipticalArc)

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

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            Types.EllipticalArcCurve2d (EllipticalArc2d.placeIn frame givenEllipticalArc)

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

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            Types.EllipticalArcCurve2d (EllipticalArc2d.relativeTo frame givenEllipticalArc)

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d (QuadraticSpline2d.relativeTo frame givenQuadraticSpline)

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d (CubicSpline2d.relativeTo frame givenCubicSpline)


at :
    Quantity Float (Rate units2 units1)
    -> Curve2d units1 coordinates
    -> Curve2d units2 coordinates
at rate givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d (LineSegment2d.at rate givenLineSegment)

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d (Arc2d.at rate givenArc)

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            Types.EllipticalArcCurve2d (EllipticalArc2d.at rate givenEllipticalArc)

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d (QuadraticSpline2d.at rate givenQuadraticSpline)

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d (CubicSpline2d.at rate givenCubicSpline)


at_ :
    Quantity Float (Rate units2 units1)
    -> Curve2d units2 coordinates
    -> Curve2d units1 coordinates
at_ rate givenCurve =
    at (Quantity.inverse rate) givenCurve


translateBy : Vector2d units coordinates -> Curve2d units coordinates -> Curve2d units coordinates
translateBy displacement givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            Types.LineSegmentCurve2d (LineSegment2d.translateBy displacement givenLineSegment)

        Types.ArcCurve2d givenArc ->
            Types.ArcCurve2d (Arc2d.translateBy displacement givenArc)

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            Types.EllipticalArcCurve2d (EllipticalArc2d.translateBy displacement givenEllipticalArc)

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

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            Types.EllipticalArcCurve2d <|
                EllipticalArc2d.scaleAbout point scale givenEllipticalArc

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

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            Types.EllipticalArcCurve2d <|
                EllipticalArc2d.rotateAround point angle givenEllipticalArc

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

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            Types.EllipticalArcCurve2d (EllipticalArc2d.mirrorAcross axis givenEllipticalArc)

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            Types.QuadraticSplineCurve2d (QuadraticSpline2d.mirrorAcross axis givenQuadraticSpline)

        Types.CubicSplineCurve2d givenCubicSpline ->
            Types.CubicSplineCurve2d (CubicSpline2d.mirrorAcross axis givenCubicSpline)


boundingBox : Curve2d units coordinates -> BoundingBox2d units coordinates
boundingBox givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d givenLineSegment ->
            LineSegment2d.boundingBox givenLineSegment

        Types.ArcCurve2d givenArc ->
            Arc2d.boundingBox givenArc

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            EllipticalArc2d.boundingBox givenEllipticalArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            QuadraticSpline2d.boundingBox givenQuadraticSpline

        Types.CubicSplineCurve2d givenCubicSpline ->
            CubicSpline2d.boundingBox givenCubicSpline


numApproximationSegments : Quantity Float units -> Curve2d units coordinates -> Int
numApproximationSegments maxError givenCurve =
    case givenCurve of
        Types.LineSegmentCurve2d _ ->
            1

        Types.ArcCurve2d givenArc ->
            Arc2d.numApproximationSegments maxError givenArc

        Types.EllipticalArcCurve2d givenEllipticalArc ->
            EllipticalArc2d.numApproximationSegments maxError givenEllipticalArc

        Types.QuadraticSplineCurve2d givenQuadraticSpline ->
            QuadraticSpline2d.numApproximationSegments maxError givenQuadraticSpline

        Types.CubicSplineCurve2d givenCubicSpline ->
            CubicSpline2d.numApproximationSegments maxError givenCubicSpline


join : Bool -> Quantity Float units -> List (Curve2d units coordinates) -> List (Curve2d units coordinates)
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
                if Point2d.equalWithin resolution p1 p2 then
                    -- Curve already loops back on itself, nothing to do
                    [ givenCurve ]

                else
                    -- Add a line from the end point back to the start point
                    [ givenCurve, lineSegment (LineSegment2d.from p2 p1) ]

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
                Point2d.equalWithin resolution firstEnd secondStart
                    || Point2d.equalWithin resolution firstEnd secondEnd
            then
                -- First segment is oriented correctly (end point touches
                -- second segment)
                merge close resolution firstStart firstEnd (second :: rest) [ first ]

            else if
                Point2d.equalWithin resolution firstStart secondStart
                    || Point2d.equalWithin resolution firstStart secondEnd
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
    -> Point2d units coordinates
    -> Point2d units coordinates
    -> List (Curve2d units coordinates)
    -> List (Curve2d units coordinates)
    -> List (Curve2d units coordinates)
merge close maxError firstStart previousEnd remainingSegments accumulated =
    case remainingSegments of
        next :: following ->
            let
                nextStart =
                    startPoint next

                nextEnd =
                    endPoint next
            in
            if Point2d.equalWithin maxError previousEnd nextStart then
                -- Next segment is oriented correctly
                merge close maxError firstStart nextEnd following (next :: accumulated)

            else if Point2d.equalWithin maxError previousEnd nextEnd then
                -- Next segment needs to be reversed
                merge close maxError firstStart nextStart following (reverse next :: accumulated)

            else
                -- Segments have a gap between them, assume next segment is
                -- correctly oriented and add a connecting line between them
                let
                    connector =
                        lineSegment (LineSegment2d.from previousEnd nextStart)
                in
                merge close maxError firstStart nextEnd following (next :: connector :: accumulated)

        [] ->
            let
                -- Add a final line segment joining end of last curve back to
                -- start of first curve, if necessary
                finalCurves =
                    if close && not (Point2d.equalWithin maxError previousEnd firstStart) then
                        lineSegment (LineSegment2d.from previousEnd firstStart) :: accumulated

                    else
                        accumulated
            in
            List.reverse finalCurves
