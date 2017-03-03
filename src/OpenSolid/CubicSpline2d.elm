module OpenSolid.CubicSpline2d
    exposing
        ( bezier
        , hermite
        , controlPoints
        , startPoint
        , endPoint
        , startDerivative
        , endDerivative
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


bezier : Point2d -> Point2d -> Point2d -> Point2d -> CubicSpline2d
bezier firstPoint secondPoint thirdPoint fourthPoint =
    CubicSpline2d ( firstPoint, secondPoint, thirdPoint, fourthPoint )


hermite : ( Point2d, Vector2d ) -> ( Point2d, Vector2d ) -> CubicSpline2d
hermite start end =
    let
        ( startPoint, startDerivative ) =
            start

        ( endPoint, endDerivative ) =
            end

        startControlPoint =
            startPoint
                |> Point2d.translateBy
                    (Vector2d.scaleBy (1 / 3) startDerivative)

        endControlPoint =
            endPoint
                |> Point2d.translateBy
                    (Vector2d.scaleBy (-1 / 3) endDerivative)
    in
        bezier startPoint startControlPoint endControlPoint endPoint


controlPoints : CubicSpline2d -> ( Point2d, Point2d, Point2d, Point2d )
controlPoints (CubicSpline2d controlPoints_) =
    controlPoints_


startPoint : CubicSpline2d -> Point2d
startPoint (CubicSpline2d ( p1, _, _, _ )) =
    p1


endPoint : CubicSpline2d -> Point2d
endPoint (CubicSpline2d ( _, _, _, p4 )) =
    p4


startDerivative : CubicSpline2d -> Vector2d
startDerivative spline =
    let
        ( p1, p2, _, _ ) =
            controlPoints spline
    in
        Point2d.vectorFrom p1 p2 |> Vector2d.scaleBy 3


endDerivative : CubicSpline2d -> Vector2d
endDerivative spline =
    let
        ( _, _, p3, p4 ) =
            controlPoints spline
    in
        Point2d.vectorFrom p3 p4 |> Vector2d.scaleBy 3


point : CubicSpline2d -> Float -> Point2d
point spline t =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        q1 =
            Point2d.interpolate p1 p2 t

        q2 =
            Point2d.interpolate p2 p3 t

        q3 =
            Point2d.interpolate p3 p4 t

        r1 =
            Point2d.interpolate q1 q2 t

        r2 =
            Point2d.interpolate q2 q3 t
    in
        Point2d.interpolate r1 r2 t


derivative : CubicSpline2d -> Float -> Vector2d
derivative spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        v1 =
            Point2d.vectorFrom p1 p2

        v2 =
            Point2d.vectorFrom p2 p3

        v3 =
            Point2d.vectorFrom p3 p4
    in
        \t ->
            let
                w1 =
                    Vector2d.interpolate v1 v2 t

                w2 =
                    Vector2d.interpolate v2 v3 t
            in
                Vector2d.interpolate w1 w2 t |> Vector2d.scaleBy 3


mapControlPoints : (Point2d -> Point2d) -> CubicSpline2d -> CubicSpline2d
mapControlPoints function spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline
    in
        CubicSpline2d ( function p1, function p2, function p3, function p4 )


scaleAbout : Point2d -> Float -> CubicSpline2d -> CubicSpline2d
scaleAbout point scale =
    mapControlPoints (Point2d.scaleAbout point scale)


rotateAround : Point2d -> Float -> CubicSpline2d -> CubicSpline2d
rotateAround point angle =
    mapControlPoints (Point2d.rotateAround point angle)


translateBy : Vector2d -> CubicSpline2d -> CubicSpline2d
translateBy displacement =
    mapControlPoints (Point2d.translateBy displacement)


mirrorAcross : Axis2d -> CubicSpline2d -> CubicSpline2d
mirrorAcross axis =
    mapControlPoints (Point2d.mirrorAcross axis)


relativeTo : Frame2d -> CubicSpline2d -> CubicSpline2d
relativeTo frame =
    mapControlPoints (Point2d.relativeTo frame)


placeIn : Frame2d -> CubicSpline2d -> CubicSpline2d
placeIn frame =
    mapControlPoints (Point2d.placeIn frame)


placeOnto : SketchPlane3d -> CubicSpline2d -> CubicSpline3d
placeOnto sketchPlane spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        place =
            Point2d.placeOnto sketchPlane
    in
        CubicSpline3d ( place p1, place p2, place p3, place p4 )
