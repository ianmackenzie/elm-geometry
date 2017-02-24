module OpenSolid.CubicSpline3d
    exposing
        ( bezier
        , controlPoints
        , startPoint
        , endPoint
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


bezier : Point3d -> Point3d -> Point3d -> Point3d -> CubicSpline3d
bezier firstPoint secondPoint thirdPoint fourthPoint =
    CubicSpline3d ( firstPoint, secondPoint, thirdPoint, fourthPoint )


controlPoints : CubicSpline3d -> ( Point3d, Point3d, Point3d, Point3d )
controlPoints (CubicSpline3d controlPoints_) =
    controlPoints_


startPoint : CubicSpline3d -> Point3d
startPoint (CubicSpline3d ( p1, _, _, _ )) =
    p1


endPoint : CubicSpline3d -> Point3d
endPoint (CubicSpline3d ( _, _, _, p4 )) =
    p4


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
