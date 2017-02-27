module OpenSolid.CubicSpline3d
    exposing
        ( bezier
        , hermite
        , controlPoints
        , startPoint
        , endPoint
        , startDerivative
        , endDerivative
        , interpolate
        , derivative
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        , projectInto
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d


bezier : Point3d -> Point3d -> Point3d -> Point3d -> CubicSpline3d
bezier firstPoint secondPoint thirdPoint fourthPoint =
    CubicSpline3d ( firstPoint, secondPoint, thirdPoint, fourthPoint )


hermite : ( Point3d, Vector3d ) -> ( Point3d, Vector3d ) -> CubicSpline3d
hermite start end =
    let
        ( startPoint, startDerivative ) =
            start

        ( endPoint, endDerivative ) =
            end

        startControlPoint =
            startPoint
                |> Point3d.translateBy
                    (Vector3d.scaleBy (1 / 3) startDerivative)

        endControlPoint =
            endPoint
                |> Point3d.translateBy
                    (Vector3d.scaleBy (-1 / 3) endDerivative)
    in
        bezier startPoint startControlPoint endControlPoint endPoint


controlPoints : CubicSpline3d -> ( Point3d, Point3d, Point3d, Point3d )
controlPoints (CubicSpline3d controlPoints_) =
    controlPoints_


startPoint : CubicSpline3d -> Point3d
startPoint (CubicSpline3d ( p1, _, _, _ )) =
    p1


endPoint : CubicSpline3d -> Point3d
endPoint (CubicSpline3d ( _, _, _, p4 )) =
    p4


startDerivative : CubicSpline3d -> Vector3d
startDerivative spline =
    let
        ( p1, p2, _, _ ) =
            controlPoints spline
    in
        Point3d.vectorFrom p1 p2 |> Vector3d.scaleBy 3


endDerivative : CubicSpline3d -> Vector3d
endDerivative spline =
    let
        ( _, _, p3, p4 ) =
            controlPoints spline
    in
        Point3d.vectorFrom p3 p4 |> Vector3d.scaleBy 3


interpolate : CubicSpline3d -> Float -> Point3d
interpolate spline t =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        q1 =
            Point3d.interpolate p1 p2 t

        q2 =
            Point3d.interpolate p2 p3 t

        q3 =
            Point3d.interpolate p3 p4 t

        r1 =
            Point3d.interpolate q1 q2 t

        r2 =
            Point3d.interpolate q2 q3 t
    in
        Point3d.interpolate r1 r2 t


derivative : CubicSpline3d -> Float -> Vector3d
derivative spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        v1 =
            Point3d.vectorFrom p1 p2

        v2 =
            Point3d.vectorFrom p2 p3

        v3 =
            Point3d.vectorFrom p3 p4
    in
        \t ->
            let
                w1 =
                    Vector3d.interpolate v1 v2 t

                w2 =
                    Vector3d.interpolate v2 v3 t
            in
                Vector3d.interpolate w1 w2 t |> Vector3d.scaleBy 3


mapControlPoints : (Point3d -> Point3d) -> CubicSpline3d -> CubicSpline3d
mapControlPoints function spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline
    in
        CubicSpline3d ( function p1, function p2, function p3, function p4 )


scaleAbout : Point3d -> Float -> CubicSpline3d -> CubicSpline3d
scaleAbout point scale =
    mapControlPoints (Point3d.scaleAbout point scale)


rotateAround : Axis3d -> Float -> CubicSpline3d -> CubicSpline3d
rotateAround axis angle =
    mapControlPoints (Point3d.rotateAround axis angle)


translateBy : Vector3d -> CubicSpline3d -> CubicSpline3d
translateBy displacement =
    mapControlPoints (Point3d.translateBy displacement)


mirrorAcross : Plane3d -> CubicSpline3d -> CubicSpline3d
mirrorAcross plane =
    mapControlPoints (Point3d.mirrorAcross plane)


projectOnto : Plane3d -> CubicSpline3d -> CubicSpline3d
projectOnto plane =
    mapControlPoints (Point3d.projectOnto plane)


relativeTo : Frame3d -> CubicSpline3d -> CubicSpline3d
relativeTo frame =
    mapControlPoints (Point3d.relativeTo frame)


placeIn : Frame3d -> CubicSpline3d -> CubicSpline3d
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


projectInto : SketchPlane3d -> CubicSpline3d -> CubicSpline2d
projectInto sketchPlane spline =
    let
        ( p1, p2, p3, p4 ) =
            controlPoints spline

        project =
            Point3d.projectInto sketchPlane
    in
        CubicSpline2d ( project p1, project p2, project p3, project p4 )
