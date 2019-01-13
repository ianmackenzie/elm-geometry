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
    , GlobalCoordinates(..)
    , LineSegment2d
    , LineSegment3d
    , LocalCoordinates(..)
    , Plane3d
    , Point2d
    , Point3d
    , Polygon2d
    , Polyline2d
    , Polyline3d
    , QuadraticSpline2d
    , QuadraticSpline3d
    , Rectangle2d
    , SketchCoordinates(..)
    , SketchPlane3d
    , Sphere3d
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
import Quantity exposing (Unitless)
import Rectangle2d
import SketchPlane3d
import Sphere3d
import Triangle2d
import Triangle3d
import Vector2d
import Vector3d


type GlobalCoordinates
    = GlobalCoordinates


type LocalCoordinates
    = LocalCoordinates


type SketchCoordinates
    = SketchCoordinates


type alias Frame2d coordinates =
    Frame2d.Frame2d Unitless coordinates { defines : LocalCoordinates }


type alias Frame3d coordinates =
    Frame3d.Frame3d Unitless coordinates { defines : LocalCoordinates }


type alias SketchPlane3d coordinates =
    SketchPlane3d.SketchPlane3d Unitless coordinates { defines : SketchCoordinates }


type alias Arc2d coordinates =
    Arc2d.Arc2d Unitless coordinates


type alias Arc3d coordinates =
    Arc3d.Arc3d Unitless coordinates


type alias Axis2d coordinates =
    Axis2d.Axis2d Unitless coordinates


type alias Axis3d coordinates =
    Axis3d.Axis3d Unitless coordinates


type alias BoundingBox2d coordinates =
    BoundingBox2d.BoundingBox2d Unitless coordinates


type alias BoundingBox3d coordinates =
    BoundingBox3d.BoundingBox3d Unitless coordinates


type alias Circle2d coordinates =
    Circle2d.Circle2d Unitless coordinates


type alias Circle3d coordinates =
    Circle3d.Circle3d Unitless coordinates


type alias CubicSpline2d coordinates =
    CubicSpline2d.CubicSpline2d Unitless coordinates


type alias CubicSpline3d coordinates =
    CubicSpline3d.CubicSpline3d Unitless coordinates


type alias Direction2d coordinates =
    Direction2d.Direction2d coordinates


type alias Direction3d coordinates =
    Direction3d.Direction3d coordinates


type alias Ellipse2d coordinates =
    Ellipse2d.Ellipse2d Unitless coordinates


type alias EllipticalArc2d coordinates =
    EllipticalArc2d.EllipticalArc2d Unitless coordinates


type alias LineSegment2d coordinates =
    LineSegment2d.LineSegment2d Unitless coordinates


type alias LineSegment3d coordinates =
    LineSegment3d.LineSegment3d Unitless coordinates


type alias Plane3d coordinates =
    Plane3d.Plane3d Unitless coordinates


type alias Point2d coordinates =
    Point2d.Point2d Unitless coordinates


type alias Point3d coordinates =
    Point3d.Point3d Unitless coordinates


type alias Polygon2d coordinates =
    Polygon2d.Polygon2d Unitless coordinates


type alias Polyline2d coordinates =
    Polyline2d.Polyline2d Unitless coordinates


type alias Polyline3d coordinates =
    Polyline3d.Polyline3d Unitless coordinates


type alias QuadraticSpline2d coordinates =
    QuadraticSpline2d.QuadraticSpline2d Unitless coordinates


type alias QuadraticSpline3d coordinates =
    QuadraticSpline3d.QuadraticSpline3d Unitless coordinates


type alias Rectangle2d coordinates =
    Rectangle2d.Rectangle2d Unitless coordinates


type alias Sphere3d coordinates =
    Sphere3d.Sphere3d Unitless coordinates


type alias Triangle2d coordinates =
    Triangle2d.Triangle2d Unitless coordinates


type alias Triangle3d coordinates =
    Triangle3d.Triangle3d Unitless coordinates


type alias Vector2d coordinates =
    Vector2d.Vector2d Unitless coordinates


type alias Vector3d coordinates =
    Vector3d.Vector3d Unitless coordinates
