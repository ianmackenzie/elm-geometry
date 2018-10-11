--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Types exposing
    ( Arc2d(..)
    , Arc3d(..)
    , Axis2d(..)
    , Axis3d(..)
    , Block3d(..)
    , BoundingBox2d(..)
    , BoundingBox3d(..)
    , Circle2d(..)
    , Circle3d(..)
    , Coordinates2d(..)
    , Coordinates3d(..)
    , CubicSpline2d(..)
    , CubicSpline3d(..)
    , DelaunayFace(..)
    , DelaunayTriangulation2d(..)
    , DelaunayVertex
    , Direction2d(..)
    , Direction3d(..)
    , Ellipse2d(..)
    , EllipticalArc2d(..)
    , Frame2d(..)
    , Frame3d(..)
    , LineSegment2d(..)
    , LineSegment3d(..)
    , Plane3d(..)
    , Point2d(..)
    , Point3d(..)
    , Polygon2d(..)
    , Polyline2d(..)
    , Polyline3d(..)
    , QuadraticSpline2d(..)
    , QuadraticSpline3d(..)
    , Rectangle2d(..)
    , Rectangle3d(..)
    , SketchPlane3d(..)
    , Sphere3d(..)
    , SweptAngle(..)
    , Triangle2d(..)
    , Triangle3d(..)
    , Vector2d(..)
    , Vector3d(..)
    )

import Array exposing (Array)
import Quantity exposing (Quantity)


type Coordinates2d system units
    = Coordinates2d ( Quantity Float units, Quantity Float units )


type Coordinates3d system units
    = Coordinates3d ( Quantity Float units, Quantity Float units, Quantity Float units )


type Vector2d coordinates
    = Vector2d coordinates


type Vector3d coordinates
    = Vector3d coordinates


type Direction2d coordinates
    = Direction2d ( Float, Float )


type Direction3d coordinates
    = Direction3d ( Float, Float, Float )


type Point2d coordinates
    = Point2d coordinates


type Point3d coordinates
    = Point3d coordinates


type Axis2d coordinates
    = Axis2d { originPoint : Point2d coordinates, direction : Direction2d coordinates }


type Axis3d coordinates
    = Axis3d { originPoint : Point3d coordinates, direction : Direction3d coordinates }


type Plane3d coordinates
    = Plane3d { originPoint : Point3d coordinates, normalDirection : Direction3d coordinates }


type Frame2d globalCoordinates localCoordinates
    = Frame2d
        { originPoint : Point2d globalCoordinates
        , xDirection : Direction2d globalCoordinates
        , yDirection : Direction2d globalCoordinates
        }


type Frame3d globalCoordinates localCoordinates
    = Frame3d
        { originPoint : Point3d globalCoordinates
        , xDirection : Direction3d globalCoordinates
        , yDirection : Direction3d globalCoordinates
        , zDirection : Direction3d globalCoordinates
        }


type SketchPlane3d globalCoordinates localCoordinates
    = SketchPlane3d
        { originPoint : Point3d globalCoordinates
        , xDirection : Direction3d globalCoordinates
        , yDirection : Direction3d globalCoordinates
        }


type LineSegment2d coordinates
    = LineSegment2d ( Point2d coordinates, Point2d coordinates )


type LineSegment3d coordinates
    = LineSegment3d ( Point3d coordinates, Point3d coordinates )


type Triangle2d coordinates
    = Triangle2d ( Point2d coordinates, Point2d coordinates, Point2d coordinates )


type Triangle3d coordinates
    = Triangle3d ( Point3d coordinates, Point3d coordinates, Point3d coordinates )


type BoundingBox2d coordinates
    = BoundingBox2d (Point2d coordinates) (Point2d coordinates)


type BoundingBox3d coordinates
    = BoundingBox3d (Point3d coordinates) (Point3d coordinates)


type RectangleCoordinates
    = RectangleCoordinates


type Rectangle2d coordinates
    = Rectangle2d
        { axes : Frame2d coordinates RectangleCoordinates
        , dimensions : coordinates
        }


type Rectangle3d coordinates
    = Rectangle3d
        { axes : Frame3d coordinates RectangleCoordinates
        , dimensions : ( Float, Float, Float )
        }


type BlockCoordinates
    = BlockCoordinates


type Block3d coordinates
    = Block3d
        { axes : Frame3d coordinates BlockCoordinates
        , dimensions : coordinates
        }


type Polyline2d coordinates
    = Polyline2d (List (Point2d coordinates))


type Polyline3d coordinates
    = Polyline3d (List (Point3d coordinates))


type Polygon2d coordinates
    = Polygon2d
        { outerLoop : List (Point2d coordinates)
        , innerLoops : List (List (Point2d coordinates))
        }


type Circle2d coordinates
    = Circle2d { centerPoint : Point2d coordinates, radius : Float }


type Circle3d coordinates
    = Circle3d
        { centerPoint : Point3d coordinates
        , axialDirection : Direction3d coordinates
        , radius : Float
        }


type EllipseCoordinates
    = EllipseCoordinates


type Ellipse2d coordinates
    = Ellipse2d
        { axes : Frame2d coordinates EllipseCoordinates
        , xRadius : Float
        , yRadius : Float
        }


type Sphere3d coordinates
    = Sphere3d
        { centerPoint : Point3d coordinates
        , radius : Float
        }


type SweptAngle
    = SmallPositive
    | SmallNegative
    | LargePositive
    | LargeNegative


type Arc2d coordinates
    = Arc2d
        { startPoint : Point2d coordinates
        , xDirection : Direction2d coordinates
        , signedLength : Float
        , sweptAngle : Float
        }


type Arc3d coordinates
    = Arc3d
        { startPoint : Point3d coordinates
        , xDirection : Direction3d coordinates
        , yDirection : Direction3d coordinates
        , signedLength : Float
        , sweptAngle : Float
        }


type QuadraticSpline2d coordinates
    = QuadraticSpline2d
        { startPoint : Point2d coordinates
        , controlPoint : Point2d coordinates
        , endPoint : Point2d coordinates
        }


type QuadraticSpline3d coordinates
    = QuadraticSpline3d
        { startPoint : Point3d coordinates
        , controlPoint : Point3d coordinates
        , endPoint : Point3d coordinates
        }


type CubicSpline2d coordinates
    = CubicSpline2d
        { startPoint : Point2d coordinates
        , startControlPoint : Point2d coordinates
        , endControlPoint : Point2d coordinates
        , endPoint : Point2d coordinates
        }


type CubicSpline3d coordinates
    = CubicSpline3d
        { startPoint : Point3d coordinates
        , startControlPoint : Point3d coordinates
        , endControlPoint : Point3d coordinates
        , endPoint : Point3d coordinates
        }


type EllipticalArc2d coordinates
    = EllipticalArc2d
        { ellipse : Ellipse2d coordinates
        , startAngle : Float
        , sweptAngle : Float
        }


type alias DelaunayVertex vertex coordinates =
    { vertex : vertex
    , index : Int
    , position : Point2d coordinates
    }


type DelaunayFace vertex coordinates
    = ThreeVertexFace (DelaunayVertex vertex coordinates) (DelaunayVertex vertex coordinates) (DelaunayVertex vertex coordinates) (Circle2d coordinates)
    | TwoVertexFace (DelaunayVertex vertex coordinates) (DelaunayVertex vertex coordinates) Int (Direction2d coordinates)
    | OneVertexFace (DelaunayVertex vertex coordinates) Int Int (Direction2d coordinates)


type DelaunayTriangulation2d vertex coordinates
    = EmptyDelaunayTriangulation2d
    | DelaunayTriangulation2d
        { vertices : Array vertex
        , delaunayVertices : List (DelaunayVertex vertex coordinates)
        , faces : List (DelaunayFace vertex coordinates)
        }
