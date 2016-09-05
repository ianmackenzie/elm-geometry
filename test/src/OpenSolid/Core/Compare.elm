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
        , axis2dWithin
        , axis3d
        , axis3dWithin
        , plane3d
        , plane3dWithin
        , frame2d
        , frame2dWithin
        , frame3d
        , frame3dWithin
        , sketchPlane3d
        , sketchPlane3dWithin
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
    axis2dWithin defaultTolerance


axis2dWithin : Float -> Comparator Axis2d
axis2dWithin tolerance =
    allOf
        [ by (point2dWithin tolerance) Axis2d.originPoint
        , by (direction2dWithin tolerance) Axis2d.direction
        ]


axis3d : Comparator Axis3d
axis3d =
    axis3dWithin defaultTolerance


axis3dWithin : Float -> Comparator Axis3d
axis3dWithin tolerance =
    allOf
        [ by (point3dWithin tolerance) Axis3d.originPoint
        , by (direction3dWithin tolerance) Axis3d.direction
        ]


plane3d : Comparator Plane3d
plane3d =
    plane3dWithin defaultTolerance


plane3dWithin : Float -> Comparator Plane3d
plane3dWithin tolerance =
    allOf
        [ by (point3dWithin tolerance) Plane3d.originPoint
        , by (direction3dWithin tolerance) Plane3d.normalDirection
        ]


frame2d : Comparator Frame2d
frame2d =
    frame2dWithin defaultTolerance


frame2dWithin : Float -> Comparator Frame2d
frame2dWithin tolerance =
    allOf
        [ by (point2dWithin tolerance) Frame2d.originPoint
        , by (direction2dWithin tolerance) Frame2d.xDirection
        , by (direction2dWithin tolerance) Frame2d.yDirection
        ]


frame3d : Comparator Frame3d
frame3d =
    frame3dWithin defaultTolerance


frame3dWithin : Float -> Comparator Frame3d
frame3dWithin tolerance =
    allOf
        [ by (point3dWithin tolerance) Frame3d.originPoint
        , by (direction3dWithin tolerance) Frame3d.xDirection
        , by (direction3dWithin tolerance) Frame3d.yDirection
        , by (direction3dWithin tolerance) Frame3d.zDirection
        ]


sketchPlane3d : Comparator SketchPlane3d
sketchPlane3d =
    sketchPlane3dWithin defaultTolerance


sketchPlane3dWithin : Float -> Comparator SketchPlane3d
sketchPlane3dWithin tolerance =
    allOf
        [ by (point3dWithin tolerance) SketchPlane3d.originPoint
        , by (direction3dWithin tolerance) SketchPlane3d.xDirection
        , by (direction3dWithin tolerance) SketchPlane3d.yDirection
        ]


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
