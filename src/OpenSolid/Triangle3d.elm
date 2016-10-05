module OpenSolid.Triangle3d
    exposing
        ( vertices
        , edges
        , map
        , mapTo
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , projectOnto
        , projectInto
        , placeOnto
        , area
        , centroid
        , boundingBox
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Point3d as Point3d
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Triangle2d as Triangle2d


vertices : Triangle3d -> ( Point3d, Point3d, Point3d )
vertices (Triangle3d vertices') =
    vertices'


edges : Triangle3d -> ( LineSegment3d, LineSegment3d, LineSegment3d )
edges triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
        ( LineSegment3d ( p1, p2 )
        , LineSegment3d ( p2, p3 )
        , LineSegment3d ( p3, p1 )
        )


map : (Point3d -> Point3d) -> Triangle3d -> Triangle3d
map =
    mapTo Triangle3d


mapTo : (( a, a, a ) -> b) -> (Point3d -> a) -> Triangle3d -> b
mapTo constructor map triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
        constructor ( map p1, map p2, map p3 )


scaleAbout : Point3d -> Float -> Triangle3d -> Triangle3d
scaleAbout centerPoint scale =
    map (Point3d.scaleAbout centerPoint scale)


rotateAround : Axis3d -> Float -> Triangle3d -> Triangle3d
rotateAround axis angle =
    map (Point3d.rotateAround axis angle)


translateBy : Vector3d -> Triangle3d -> Triangle3d
translateBy vector =
    map (Point3d.translateBy vector)


mirrorAcross : Plane3d -> Triangle3d -> Triangle3d
mirrorAcross plane =
    map (Point3d.mirrorAcross plane)


projectOnto : Plane3d -> Triangle3d -> Triangle3d
projectOnto plane =
    map (Point3d.projectOnto plane)


projectInto : SketchPlane3d -> Triangle3d -> Triangle2d
projectInto sketchPlane =
    mapTo Triangle2d (Point3d.projectInto sketchPlane)


placeOnto : SketchPlane3d -> Triangle2d -> Triangle3d
placeOnto sketchPlane =
    Triangle2d.mapTo Triangle3d (Point3d.placeOnto sketchPlane)


area : Triangle3d -> Float
area triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Point3d.vectorFrom p1 p2

        secondVector =
            Point3d.vectorFrom p1 p3
    in
        0.5 * Vector3d.length (Vector3d.crossProduct firstVector secondVector)


centroid : Triangle3d -> Point3d
centroid triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Point3d.vectorFrom p1 p2

        secondVector =
            Point3d.vectorFrom p1 p3

        displacement =
            Vector3d.times (1.0 / 3.0) (Vector3d.plus secondVector firstVector)
    in
        Point3d.translateBy displacement p1


boundingBox : Triangle3d -> BoundingBox3d
boundingBox triangle =
    BoundingBox3d.containing3 (vertices triangle)
