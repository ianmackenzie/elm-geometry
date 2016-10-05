module OpenSolid.Triangle2d
    exposing
        ( vertices
        , edges
        , map
        , mapTo
        , contains
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , area
        , signedArea
        , centroid
        , boundingBox
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Point2d as Point2d
import OpenSolid.BoundingBox2d as BoundingBox2d


vertices : Triangle2d -> ( Point2d, Point2d, Point2d )
vertices (Triangle2d vertices') =
    vertices'


edges : Triangle2d -> ( LineSegment2d, LineSegment2d, LineSegment2d )
edges triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
        ( LineSegment2d ( p1, p2 )
        , LineSegment2d ( p2, p3 )
        , LineSegment2d ( p3, p1 )
        )


map : (Point2d -> Point2d) -> Triangle2d -> Triangle2d
map =
    mapTo Triangle2d


mapTo : (( a, a, a ) -> b) -> (Point2d -> a) -> Triangle2d -> b
mapTo constructor map triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle
    in
        constructor ( map p1, map p2, map p3 )


contains : Point2d -> Triangle2d -> Bool
contains point triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        crossProduct startVertex endVertex =
            let
                vectorToPoint =
                    Point2d.vectorFrom startVertex point

                segmentVector =
                    Point2d.vectorFrom startVertex endVertex
            in
                Vector2d.crossProduct segmentVector vectorToPoint

        firstProduct =
            crossProduct p1 p2

        secondProduct =
            crossProduct p2 p3

        thirdProduct =
            crossProduct p3 p1
    in
        (firstProduct >= 0 && secondProduct >= 0 && thirdProduct >= 0)
            || (firstProduct <= 0 && secondProduct <= 0 && thirdProduct <= 0)


scaleAbout : Point2d -> Float -> Triangle2d -> Triangle2d
scaleAbout point scale =
    map (Point2d.scaleAbout point scale)


rotateAround : Point2d -> Float -> Triangle2d -> Triangle2d
rotateAround centerPoint angle =
    map (Point2d.rotateAround centerPoint angle)


translateBy : Vector2d -> Triangle2d -> Triangle2d
translateBy vector =
    map (Point2d.translateBy vector)


mirrorAcross : Axis2d -> Triangle2d -> Triangle2d
mirrorAcross axis =
    map (Point2d.mirrorAcross axis)


area : Triangle2d -> Float
area =
    abs << signedArea


signedArea : Triangle2d -> Float
signedArea triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Point2d.vectorFrom p1 p2

        secondVector =
            Point2d.vectorFrom p1 p3
    in
        0.5 * Vector2d.crossProduct firstVector secondVector


centroid : Triangle2d -> Point2d
centroid triangle =
    let
        ( p1, p2, p3 ) =
            vertices triangle

        firstVector =
            Point2d.vectorFrom p1 p2

        secondVector =
            Point2d.vectorFrom p1 p3

        displacement =
            Vector2d.times (1.0 / 3.0) (Vector2d.plus secondVector firstVector)
    in
        Point2d.translateBy displacement p1


boundingBox : Triangle2d -> BoundingBox2d
boundingBox triangle =
    BoundingBox2d.containing3 (vertices triangle)
