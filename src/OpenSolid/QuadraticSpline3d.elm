module OpenSolid.QuadraticSpline3d
    exposing
        ( bezier
        , controlPoints
        , startPoint
        , endPoint
        , evaluate
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


bezier : Point3d -> Point3d -> Point3d -> QuadraticSpline3d
bezier firstPoint secondPoint thirdPoint =
    QuadraticSpline3d ( firstPoint, secondPoint, thirdPoint )


controlPoints : QuadraticSpline3d -> ( Point3d, Point3d, Point3d )
controlPoints (QuadraticSpline3d controlPoints_) =
    controlPoints_


startPoint : QuadraticSpline3d -> Point3d
startPoint (QuadraticSpline3d ( p1, _, _ )) =
    p1


endPoint : QuadraticSpline3d -> Point3d
endPoint (QuadraticSpline3d ( _, _, p3 )) =
    p3


evaluate : QuadraticSpline3d -> Float -> Point3d
evaluate spline t =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        q1 =
            Point3d.interpolate p1 p2 t

        q2 =
            Point3d.interpolate p2 p3 t
    in
        Point3d.interpolate q1 q2 t


derivative : QuadraticSpline3d -> Float -> Vector3d
derivative spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        v1 =
            Point3d.vectorFrom p1 p2

        v2 =
            Point3d.vectorFrom p2 p3
    in
        \t -> Vector3d.interpolate v1 v2 t |> Vector3d.scaleBy 2


mapControlPoints : (Point3d -> Point3d) -> QuadraticSpline3d -> QuadraticSpline3d
mapControlPoints function spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline
    in
        QuadraticSpline3d ( function p1, function p2, function p3 )


scaleAbout : Point3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
scaleAbout point scale =
    mapControlPoints (Point3d.scaleAbout point scale)


rotateAround : Axis3d -> Float -> QuadraticSpline3d -> QuadraticSpline3d
rotateAround axis angle =
    mapControlPoints (Point3d.rotateAround axis angle)


translateBy : Vector3d -> QuadraticSpline3d -> QuadraticSpline3d
translateBy displacement =
    mapControlPoints (Point3d.translateBy displacement)


mirrorAcross : Plane3d -> QuadraticSpline3d -> QuadraticSpline3d
mirrorAcross plane =
    mapControlPoints (Point3d.mirrorAcross plane)


projectOnto : Plane3d -> QuadraticSpline3d -> QuadraticSpline3d
projectOnto plane =
    mapControlPoints (Point3d.projectOnto plane)


relativeTo : Frame3d -> QuadraticSpline3d -> QuadraticSpline3d
relativeTo frame =
    mapControlPoints (Point3d.relativeTo frame)


placeIn : Frame3d -> QuadraticSpline3d -> QuadraticSpline3d
placeIn frame =
    mapControlPoints (Point3d.placeIn frame)


projectInto : SketchPlane3d -> QuadraticSpline3d -> QuadraticSpline2d
projectInto sketchPlane spline =
    let
        ( p1, p2, p3 ) =
            controlPoints spline

        project =
            Point3d.projectInto sketchPlane
    in
        QuadraticSpline2d ( project p1, project p2, project p3 )
