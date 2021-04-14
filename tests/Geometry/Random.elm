module Geometry.Random exposing
    ( angle
    , cubicSpline2d
    , cubicSpline3d
    , length
    , parameterValue
    , positiveLength
    , quadraticSpline2d
    , quadraticSpline3d
    , rationalCubicSpline2d
    , rationalCubicSpline3d
    , rationalQuadraticSpline2d
    , rationalQuadraticSpline3d
    , scale
    , unitlessInterval
    , unitlessQuantity
    , vector2d
    , vector3d
    , vector4d
    , vectorBoundingBox2d
    , vectorBoundingBox3d
    , vectorBoundingBox4d
    )

import Angle exposing (Angle)
import CubicSpline2d exposing (CubicSpline2d)
import CubicSpline3d exposing (CubicSpline3d)
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import QuadraticSpline2d exposing (QuadraticSpline2d)
import QuadraticSpline3d exposing (QuadraticSpline3d)
import Quantity exposing (Quantity, Unitless)
import Quantity.Interval as Interval exposing (Interval)
import Random exposing (Generator)
import RationalCubicSpline2d exposing (RationalCubicSpline2d)
import RationalCubicSpline3d exposing (RationalCubicSpline3d)
import RationalQuadraticSpline2d exposing (RationalQuadraticSpline2d)
import RationalQuadraticSpline3d exposing (RationalQuadraticSpline3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)
import Vector4d exposing (Vector4d)
import VectorBoundingBox2d exposing (VectorBoundingBox2d)
import VectorBoundingBox3d exposing (VectorBoundingBox3d)
import VectorBoundingBox4d exposing (VectorBoundingBox4d)


parameterValue : Generator Float
parameterValue =
    Random.weighted
        ( 1, Random.float 0 1 )
        [ ( 1, Random.constant 0 )
        , ( 1, Random.constant 1 )
        ]
        |> Random.andThen identity


scale : Generator Float
scale =
    Random.weighted
        ( 3, Random.float -10 10 )
        [ ( 1, Random.constant 0 ) ]
        |> Random.andThen identity


length : Generator Length
length =
    Random.map Length.meters (Random.float -10 10)


unitlessQuantity : Generator (Quantity Float Unitless)
unitlessQuantity =
    Random.map Quantity.float scale


unitlessInterval : Generator (Interval Float Unitless)
unitlessInterval =
    Random.map2 Interval.from unitlessQuantity unitlessQuantity


positiveLength : Generator Length
positiveLength =
    Random.map Quantity.abs length


angle : Generator Angle
angle =
    Random.map Angle.radians (Random.float (-2 * pi) (2 * pi))


vector2d : Generator (Vector2d Meters coordinates)
vector2d =
    Random.map2 Vector2d.xy length length


vector3d : Generator (Vector3d Meters coordinates)
vector3d =
    Random.map3 Vector3d.xyz length length length


vector4d : Generator (Vector4d Meters coordinates)
vector4d =
    Random.map4 Vector4d.xyzw length length length length


vectorBoundingBox2d : Generator (VectorBoundingBox2d Meters coordinates)
vectorBoundingBox2d =
    Random.map2 VectorBoundingBox2d.hull2 vector2d vector2d


vectorBoundingBox3d : Generator (VectorBoundingBox3d Meters coordinates)
vectorBoundingBox3d =
    Random.map2 VectorBoundingBox3d.from vector3d vector3d


vectorBoundingBox4d : Generator (VectorBoundingBox4d Meters coordinates)
vectorBoundingBox4d =
    Random.map2 VectorBoundingBox4d.from vector4d vector4d


point2d : Generator (Point2d Meters coordinates)
point2d =
    Random.map2 Point2d.xy length length


point3d : Generator (Point3d Meters coordinates)
point3d =
    Random.map3 Point3d.xyz length length length


quadraticSpline2d : Generator (QuadraticSpline2d Meters coordinates)
quadraticSpline2d =
    Random.map3 QuadraticSpline2d.fromControlPoints point2d point2d point2d


quadraticSpline3d : Generator (QuadraticSpline3d Meters coordinates)
quadraticSpline3d =
    Random.map3 QuadraticSpline3d.fromControlPoints point3d point3d point3d


cubicSpline2d : Generator (CubicSpline2d Meters coordinates)
cubicSpline2d =
    Random.map4 CubicSpline2d.fromControlPoints point2d point2d point2d point2d


weightedControlPoint2d : Generator ( Point2d Meters coordinates, Float )
weightedControlPoint2d =
    Random.map2 Tuple.pair point2d (Random.float 1 10)


rationalQuadraticSpline2d : Generator (RationalQuadraticSpline2d Meters coordinates)
rationalQuadraticSpline2d =
    Random.map3 RationalQuadraticSpline2d.fromControlPoints
        weightedControlPoint2d
        weightedControlPoint2d
        weightedControlPoint2d


rationalCubicSpline2d : Generator (RationalCubicSpline2d Meters coordinates)
rationalCubicSpline2d =
    Random.map4 RationalCubicSpline2d.fromControlPoints
        weightedControlPoint2d
        weightedControlPoint2d
        weightedControlPoint2d
        weightedControlPoint2d


weightedControlPoint3d : Generator ( Point3d Meters coordinates, Float )
weightedControlPoint3d =
    Random.map2 Tuple.pair point3d (Random.float 1 10)


rationalQuadraticSpline3d : Generator (RationalQuadraticSpline3d Meters coordinates)
rationalQuadraticSpline3d =
    Random.map3 RationalQuadraticSpline3d.fromControlPoints
        weightedControlPoint3d
        weightedControlPoint3d
        weightedControlPoint3d


rationalCubicSpline3d : Generator (RationalCubicSpline3d Meters coordinates)
rationalCubicSpline3d =
    Random.map4 RationalCubicSpline3d.fromControlPoints
        weightedControlPoint3d
        weightedControlPoint3d
        weightedControlPoint3d
        weightedControlPoint3d


cubicSpline3d : Generator (CubicSpline3d Meters coordinates)
cubicSpline3d =
    Random.map4 CubicSpline3d.fromControlPoints point3d point3d point3d point3d
