module OpenSolid.Core.Compare
    exposing
        ( Comparator
        , by
        , allOf
        , defaultTolerance
        , approximately
        , approximatelyWithin
        , angle
        , angleWithin
        , vector2d
        , vector2dWithin
        , vector3d
        , vector3dWithin
        , direction2d
        , direction2dWithin
        , direction3d
        , direction3dWithin
        , point2d
        , point2dWithin
        , point3d
        , point3dWithin
        , axis2d
        , axis3d
        , plane3d
        , frame2d
        , frame3d
        , sketchPlane3d
        , lineSegment2d
        , lineSegment2dWithin
        , lineSegment3d
        , lineSegment3dWithin
        , triangle2d
        , triangle2dWithin
        , triangle3d
        , triangle3dWithin
        , boundingBox2d
        , boundingBox2dWithin
        , boundingBox3d
        , boundingBox3dWithin
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.SketchPlane3d as SketchPlane3d
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Triangle2d as Triangle2d
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.BoundingBox2d as BoundingBox2d
import OpenSolid.BoundingBox3d as BoundingBox3d


type alias Comparator a =
    a -> a -> Bool


by : Comparator b -> (a -> b) -> Comparator a
by comparator property first second =
    comparator (property first) (property second)


allOf : List (Comparator a) -> Comparator a
allOf comparators first second =
    List.all (\comparator -> comparator first second) comparators


defaultTolerance : Float
defaultTolerance =
    1.0e-12


approximately : Comparator Float
approximately =
    approximatelyWithin defaultTolerance


approximatelyWithin : Float -> Comparator Float
approximatelyWithin tolerance first second =
    abs (first - second) <= tolerance


angle : Comparator Float
angle =
    angleWithin defaultTolerance


angleWithin : Float -> Comparator Float
angleWithin tolerance first second =
    let
        difference =
            second - first
    in
        abs (sin difference) <= tolerance && cos difference > 0.0


vector2d : Comparator Vector2d
vector2d =
    vector2dWithin defaultTolerance


vector2dWithin : Float -> Comparator Vector2d
vector2dWithin tolerance first second =
    Vector2d.length (Vector2d.minus first second) <= tolerance


vector3d : Comparator Vector3d
vector3d =
    vector3dWithin defaultTolerance


vector3dWithin : Float -> Comparator Vector3d
vector3dWithin tolerance first second =
    Vector3d.length (Vector3d.minus first second) <= tolerance


direction2d : Comparator Direction2d
direction2d =
    direction2dWithin defaultTolerance


direction2dWithin : Float -> Comparator Direction2d
direction2dWithin tolerance =
    by (vector2dWithin tolerance) Direction2d.vector


direction3d : Comparator Direction3d
direction3d =
    direction3dWithin defaultTolerance


direction3dWithin : Float -> Comparator Direction3d
direction3dWithin tolerance =
    by (vector3dWithin tolerance) Direction3d.vector


point2d : Comparator Point2d
point2d =
    point2dWithin defaultTolerance


point2dWithin : Float -> Comparator Point2d
point2dWithin tolerance first second =
    Point2d.distanceFrom first second <= tolerance


point3d : Comparator Point3d
point3d =
    point3dWithin defaultTolerance


point3dWithin : Float -> Comparator Point3d
point3dWithin tolerance first second =
    Point3d.distanceFrom first second <= tolerance


axis2d : Comparator Axis2d
axis2d =
    allOf [ by point2d Axis2d.originPoint, by direction2d Axis2d.direction ]


axis3d : Comparator Axis3d
axis3d =
    allOf [ by point3d Axis3d.originPoint, by direction3d Axis3d.direction ]


plane3d : Comparator Plane3d
plane3d =
    allOf
        [ by point3d Plane3d.originPoint
        , by direction3d Plane3d.normalDirection
        ]


frame2d : Comparator Frame2d
frame2d =
    allOf
        [ by point2d Frame2d.originPoint
        , by direction2d Frame2d.xDirection
        , by direction2d Frame2d.yDirection
        ]


frame3d : Comparator Frame3d
frame3d =
    allOf
        [ by point3d Frame3d.originPoint
        , by direction3d Frame3d.xDirection
        , by direction3d Frame3d.yDirection
        , by direction3d Frame3d.zDirection
        ]


sketchPlane3d : Comparator SketchPlane3d
sketchPlane3d =
    allOf
        [ by point3d SketchPlane3d.originPoint
        , by direction3d SketchPlane3d.xDirection
        , by direction3d SketchPlane3d.yDirection
        ]


lineSegment2d : Comparator LineSegment2d
lineSegment2d =
    lineSegment2dWithin defaultTolerance


lineSegment2dWithin : Float -> Comparator LineSegment2d
lineSegment2dWithin tolerance =
    allOf
        [ by (point2dWithin tolerance) LineSegment2d.startPoint
        , by (point2dWithin tolerance) LineSegment2d.endPoint
        ]


lineSegment3d : Comparator LineSegment3d
lineSegment3d =
    lineSegment3dWithin defaultTolerance


lineSegment3dWithin : Float -> Comparator LineSegment3d
lineSegment3dWithin tolerance =
    allOf
        [ by (point3dWithin tolerance) LineSegment3d.startPoint
        , by (point3dWithin tolerance) LineSegment3d.endPoint
        ]


triangle2d : Comparator Triangle2d
triangle2d =
    triangle2dWithin defaultTolerance


triangle2dWithin : Float -> Comparator Triangle2d
triangle2dWithin tolerance firstTriangle secondTriangle =
    let
        ( firstVertex1, firstVertex2, firstVertex3 ) =
            Triangle2d.vertices firstTriangle

        ( secondVertex1, secondVertex2, secondVertex3 ) =
            Triangle2d.vertices secondTriangle

        comparePoints =
            point2dWithin tolerance
    in
        comparePoints firstVertex1 secondVertex1
            && comparePoints firstVertex2 secondVertex2
            && comparePoints firstVertex3 secondVertex3


triangle3d : Comparator Triangle3d
triangle3d =
    triangle3dWithin defaultTolerance


triangle3dWithin : Float -> Comparator Triangle3d
triangle3dWithin tolerance firstTriangle secondTriangle =
    let
        ( firstVertex1, firstVertex2, firstVertex3 ) =
            Triangle3d.vertices firstTriangle

        ( secondVertex1, secondVertex2, secondVertex3 ) =
            Triangle3d.vertices secondTriangle

        comparePoints =
            point3dWithin tolerance
    in
        comparePoints firstVertex1 secondVertex1
            && comparePoints firstVertex2 secondVertex2
            && comparePoints firstVertex3 secondVertex3


boundingBox2d : Comparator BoundingBox2d
boundingBox2d =
    boundingBox2dWithin defaultTolerance


boundingBox2dWithin : Float -> Comparator BoundingBox2d
boundingBox2dWithin tolerance =
    allOf
        [ by (approximatelyWithin tolerance) BoundingBox2d.minX
        , by (approximatelyWithin tolerance) BoundingBox2d.maxX
        , by (approximatelyWithin tolerance) BoundingBox2d.minY
        , by (approximatelyWithin tolerance) BoundingBox2d.maxY
        ]


boundingBox3d : Comparator BoundingBox3d
boundingBox3d =
    boundingBox3dWithin defaultTolerance


boundingBox3dWithin : Float -> Comparator BoundingBox3d
boundingBox3dWithin tolerance =
    allOf
        [ by (approximatelyWithin tolerance) BoundingBox3d.minX
        , by (approximatelyWithin tolerance) BoundingBox3d.maxX
        , by (approximatelyWithin tolerance) BoundingBox3d.minY
        , by (approximatelyWithin tolerance) BoundingBox3d.maxY
        , by (approximatelyWithin tolerance) BoundingBox3d.minZ
        , by (approximatelyWithin tolerance) BoundingBox3d.maxZ
        ]
