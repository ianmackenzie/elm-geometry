module Geometry.Test exposing
    ( Arc2d
    , Arc3d
    , Axis2d
    , Axis3d
    , BoundingBox2d
    , BoundingBox3d
    , Circle2d
    , Circle3d
    , CubicSpline2d
    , CubicSpline3d
    , Direction2d
    , Direction3d
    , Ellipse2d
    , EllipticalArc2d
    , Frame2d
    , Frame3d
    , LineSegment2d
    , LineSegment3d
    , Plane3d
    , Point2d
    , Point3d
    , Polygon2d
    , Polyline2d
    , Polyline3d
    , QuadraticSpline2d
    , QuadraticSpline3d
    , Rectangle2d
    , SketchPlane3d
    , Sphere3d
    , TestCoordinates(..)
    , Triangle2d
    , Triangle3d
    , Vector2d
    , Vector3d
    )

import Arc2d
import Arc3d
import Axis2d
import Axis3d
import BoundingBox2d
import BoundingBox3d
import Circle2d
import Circle3d
import CubicSpline2d
import CubicSpline3d
import Direction2d
import Direction3d
import Ellipse2d
import EllipticalArc2d
import Frame2d
import Frame3d
import Length exposing (Meters)
import LineSegment2d
import LineSegment3d
import Plane3d
import Point2d
import Point3d
import Polygon2d
import Polyline2d
import Polyline3d
import QuadraticSpline2d
import QuadraticSpline3d
import Rectangle2d
import SketchPlane3d
import Sphere3d
import Triangle2d
import Triangle3d
import Vector2d
import Vector3d


type TestCoordinates
    = TestCoordinates


type alias Arc2d =
    Arc2d.Arc2d Meters TestCoordinates


type alias Arc3d =
    Arc3d.Arc3d Meters TestCoordinates


type alias Axis2d =
    Axis2d.Axis2d Meters TestCoordinates


type alias Axis3d =
    Axis3d.Axis3d Meters TestCoordinates


type alias BoundingBox2d =
    BoundingBox2d.BoundingBox2d Meters TestCoordinates


type alias BoundingBox3d =
    BoundingBox3d.BoundingBox3d Meters TestCoordinates


type alias Circle2d =
    Circle2d.Circle2d Meters TestCoordinates


type alias Circle3d =
    Circle3d.Circle3d Meters TestCoordinates


type alias CubicSpline2d =
    CubicSpline2d.CubicSpline2d Meters TestCoordinates


type alias CubicSpline3d =
    CubicSpline3d.CubicSpline3d Meters TestCoordinates


type alias Direction2d =
    Direction2d.Direction2d TestCoordinates


type alias Direction3d =
    Direction3d.Direction3d TestCoordinates


type alias Ellipse2d =
    Ellipse2d.Ellipse2d Meters TestCoordinates


type alias EllipticalArc2d =
    EllipticalArc2d.EllipticalArc2d Meters TestCoordinates


type alias Frame2d defines =
    Frame2d.Frame2d Meters TestCoordinates defines


type alias Frame3d defines =
    Frame3d.Frame3d Meters TestCoordinates defines


type alias LineSegment2d =
    LineSegment2d.LineSegment2d Meters TestCoordinates


type alias LineSegment3d =
    LineSegment3d.LineSegment3d Meters TestCoordinates


type alias Plane3d =
    Plane3d.Plane3d Meters TestCoordinates


type alias Point2d =
    Point2d.Point2d Meters TestCoordinates


type alias Point3d =
    Point3d.Point3d Meters TestCoordinates


type alias Polygon2d =
    Polygon2d.Polygon2d Meters TestCoordinates


type alias Polyline2d =
    Polyline2d.Polyline2d Meters TestCoordinates


type alias Polyline3d =
    Polyline3d.Polyline3d Meters TestCoordinates


type alias QuadraticSpline2d =
    QuadraticSpline2d.QuadraticSpline2d Meters TestCoordinates


type alias QuadraticSpline3d =
    QuadraticSpline3d.QuadraticSpline3d Meters TestCoordinates


type alias Rectangle2d =
    Rectangle2d.Rectangle2d Meters TestCoordinates


type alias SketchPlane3d defines =
    SketchPlane3d.SketchPlane3d Meters TestCoordinates defines


type alias Sphere3d =
    Sphere3d.Sphere3d Meters TestCoordinates


type alias Triangle2d =
    Triangle2d.Triangle2d Meters TestCoordinates


type alias Triangle3d =
    Triangle3d.Triangle3d Meters TestCoordinates


type alias Vector2d =
    Vector2d.Vector2d Meters TestCoordinates


type alias Vector3d =
    Vector3d.Vector3d Meters TestCoordinates
