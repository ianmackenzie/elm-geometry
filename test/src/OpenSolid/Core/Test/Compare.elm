module OpenSolid.Core.Test.Compare
    exposing
        ( Comparator
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
        )

import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d
import OpenSolid.Core.Vector3d as Vector3d
import OpenSolid.Core.Direction2d as Direction2d
import OpenSolid.Core.Direction3d as Direction3d
import OpenSolid.Core.Point2d as Point2d
import OpenSolid.Core.Point3d as Point3d
import OpenSolid.Core.Axis2d as Axis2d
import OpenSolid.Core.Axis3d as Axis3d


defaultTolerance : Float
defaultTolerance =
    1.0e-12


type alias Comparator a =
    a -> a -> Bool


by : Comparator b -> (a -> b) -> Comparator a
by comparator property first second =
    comparator (property first) (property second)


both : Comparator a -> Comparator a -> Comparator a
both firstComparator secondComparator first second =
    firstComparator first second && secondComparator first second


allOf : List (Comparator a) -> Comparator a
allOf comparators first second =
    List.all (\comparator -> comparator first second) comparators


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
