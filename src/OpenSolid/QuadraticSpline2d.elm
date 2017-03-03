module OpenSolid.QuadraticSpline2d
    exposing
        ( bezier
        , controlPoints
        , startPoint
        , endPoint
        , point
        , derivative
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        , placeOnto
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point2d as Point2d
import OpenSolid.Vector2d as Vector2d


bezier : Point2d -> Point2d -> Point2d -> QuadraticSpline2d
bezier firstPoint secondPoint thirdPoint =
    QuadraticSpline2d ( firstPoint, secondPoint, thirdPoint )


controlPoints : QuadraticSpline2d -> ( Point2d, Point2d, Point2d )
controlPoints (QuadraticSpline2d controlPoints_) =
    controlPoints_


startPoint : QuadraticSpline2d -> Point2d
startPoint (QuadraticSpline2d ( p1, _, _ )) =
    p1


endPoint : QuadraticSpline2d -> Point2d
endPoint (QuadraticSpline2d ( _, _, p3 )) =
    p3


point : QuadraticSpline2d -> Float -> Point2d
point spline t =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        q1 =
            Point2d.interpolate p1 p2 t

        q2 =
            Point2d.interpolate p2 p3 t
    in
        Point2d.interpolate q1 q2 t


derivative : QuadraticSpline2d -> Float -> Vector2d
derivative spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        v1 =
            Point2d.vectorFrom p1 p2

        v2 =
            Point2d.vectorFrom p2 p3
    in
        \t -> Vector2d.interpolate v1 v2 t |> Vector2d.scaleBy 2


mapControlPoints : (Point2d -> Point2d) -> QuadraticSpline2d -> QuadraticSpline2d
mapControlPoints function spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline
    in
        QuadraticSpline2d ( function p1, function p2, function p3 )


scaleAbout : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


rotateAround : Point2d -> Float -> QuadraticSpline2d -> QuadraticSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


translateBy : Vector2d -> QuadraticSpline2d -> QuadraticSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


mirrorAcross : Axis2d -> QuadraticSpline2d -> QuadraticSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


relativeTo : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


placeIn : Frame2d -> QuadraticSpline2d -> QuadraticSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


placeOnto : SketchPlane3d -> QuadraticSpline2d -> QuadraticSpline3d
placeOnto sketchPlane spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        place =
            Point2d.placeOnto sketchPlane
    in
        QuadraticSpline3d ( place p1, place p2, place p3 )
