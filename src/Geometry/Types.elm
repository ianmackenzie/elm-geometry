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


type Vector2d coordinates units
    = Vector2d ( Quantity Float units, Quantity Float units )


type Vector3d coordinates units
    = Vector3d ( Quantity Float units, Quantity Float units, Quantity Float units )


type Direction2d coordinates
    = Direction2d ( Float, Float )


type Direction3d coordinates
    = Direction3d ( Float, Float, Float )


type Point2d coordinates units
    = Point2d ( Quantity Float units, Quantity Float units )


type Point3d coordinates units
    = Point3d ( Quantity Float units, Quantity Float units, Quantity Float units )


type Axis2d coordinates units
    = Axis2d { originPoint : Point2d coordinates units, direction : Direction2d coordinates }


type Axis3d coordinates units
    = Axis3d { originPoint : Point3d coordinates units, direction : Direction3d coordinates }


type Plane3d coordinates units
    = Plane3d { originPoint : Point3d coordinates units, normalDirection : Direction3d coordinates }


type Frame2d coordinates units defines
    = Frame2d
        { originPoint : Point2d coordinates units
        , xDirection : Direction2d coordinates
        , yDirection : Direction2d coordinates
        }


type Frame3d coordinates units defines
    = Frame3d
        { originPoint : Point3d coordinates units
        , xDirection : Direction3d coordinates
        , yDirection : Direction3d coordinates
        , zDirection : Direction3d coordinates
        }


type SketchPlane3d coordinates units defines
    = SketchPlane3d
        { originPoint : Point3d coordinates units
        , xDirection : Direction3d coordinates
        , yDirection : Direction3d coordinates
        }


type LineSegment2d coordinates units
    = LineSegment2d ( Point2d coordinates units, Point2d coordinates units )


type LineSegment3d coordinates units
    = LineSegment3d ( Point3d coordinates units, Point3d coordinates units )


type Triangle2d coordinates units
    = Triangle2d ( Point2d coordinates units, Point2d coordinates units, Point2d coordinates units )


type Triangle3d coordinates units
    = Triangle3d ( Point3d coordinates units, Point3d coordinates units, Point3d coordinates units )


type BoundingBox2d coordinates units
    = BoundingBox2d
        { minX : Quantity Float units
        , maxX : Quantity Float units
        , minY : Quantity Float units
        , maxY : Quantity Float units
        }


type BoundingBox3d coordinates units
    = BoundingBox3d
        { minX : Quantity Float units
        , maxX : Quantity Float units
        , minY : Quantity Float units
        , maxY : Quantity Float units
        , minZ : Quantity Float units
        , maxZ : Quantity Float units
        }


type RectangleCoordinates
    = RectangleCoordinates


type Rectangle2d coordinates units
    = Rectangle2d
        { axes : Frame2d coordinates units { defines : RectangleCoordinates }
        , dimensions : ( Quantity Float units, Quantity Float units )
        }


type Rectangle3d coordinates units
    = Rectangle3d
        { axes : SketchPlane3d coordinates units { defines : RectangleCoordinates }
        , dimensions : ( Quantity Float units, Quantity Float units )
        }


type BlockCoordinates
    = BlockCoordinates


type Block3d coordinates units
    = Block3d
        { axes : Frame3d coordinates units { defines : BlockCoordinates }
        , dimensions : ( Quantity Float units, Quantity Float units, Quantity Float units )
        }


type Polyline2d coordinates units
    = Polyline2d (List (Point2d coordinates units))


type Polyline3d coordinates units
    = Polyline3d (List (Point3d coordinates units))


type Polygon2d coordinates units
    = Polygon2d
        { outerLoop : List (Point2d coordinates units)
        , innerLoops : List (List (Point2d coordinates units))
        }


type Circle2d coordinates units
    = Circle2d
        { centerPoint : Point2d coordinates units
        , radius : Quantity Float units
        }


type Circle3d coordinates units
    = Circle3d
        { centerPoint : Point3d coordinates units
        , axialDirection : Direction3d coordinates
        , radius : Quantity Float units
        }


type EllipseCoordinates
    = EllipseCoordinates


type Ellipse2d coordinates units
    = Ellipse2d
        { axes : Frame2d coordinates units { defines : EllipseCoordinates }
        , xRadius : Quantity Float units
        , yRadius : Quantity Float units
        }


type Sphere3d coordinates units
    = Sphere3d
        { centerPoint : Point3d coordinates units
        , radius : Quantity Float units
        }


type SweptAngle
    = SmallPositive
    | SmallNegative
    | LargePositive
    | LargeNegative


type Arc2d coordinates units
    = Arc2d
        { startPoint : Point2d coordinates units
        , xDirection : Direction2d coordinates
        , signedLength : Quantity Float units
        , sweptAngle : Quantity Float units
        }


type Arc3d coordinates units
    = Arc3d
        { startPoint : Point3d coordinates units
        , xDirection : Direction3d coordinates
        , yDirection : Direction3d coordinates
        , signedLength : Quantity Float units
        , sweptAngle : Quantity Float units
        }


type QuadraticSpline2d coordinates units
    = QuadraticSpline2d
        { startPoint : Point2d coordinates units
        , controlPoint : Point2d coordinates units
        , endPoint : Point2d coordinates units
        }


type QuadraticSpline3d coordinates units
    = QuadraticSpline3d
        { startPoint : Point3d coordinates units
        , controlPoint : Point3d coordinates units
        , endPoint : Point3d coordinates units
        }


type CubicSpline2d coordinates units
    = CubicSpline2d
        { startPoint : Point2d coordinates units
        , startControlPoint : Point2d coordinates units
        , endControlPoint : Point2d coordinates units
        , endPoint : Point2d coordinates units
        }


type CubicSpline3d coordinates units
    = CubicSpline3d
        { startPoint : Point3d coordinates units
        , startControlPoint : Point3d coordinates units
        , endControlPoint : Point3d coordinates units
        , endPoint : Point3d coordinates units
        }


type EllipticalArc2d coordinates units
    = EllipticalArc2d
        { ellipse : Ellipse2d coordinates units
        , startAngle : Quantity Float units
        , sweptAngle : Quantity Float units
        }


type alias DelaunayVertex vertex coordinates units =
    { vertex : vertex
    , index : Int
    , position : Point2d coordinates units
    }


type DelaunayFace vertex coordinates units
    = ThreeVertexFace (DelaunayVertex vertex coordinates units) (DelaunayVertex vertex coordinates units) (DelaunayVertex vertex coordinates units) (Circle2d coordinates units)
    | TwoVertexFace (DelaunayVertex vertex coordinates units) (DelaunayVertex vertex coordinates units) Int (Direction2d coordinates)
    | OneVertexFace (DelaunayVertex vertex coordinates units) Int Int (Direction2d coordinates)


type DelaunayTriangulation2d vertex coordinates units
    = EmptyDelaunayTriangulation2d
    | DelaunayTriangulation2d
        { vertices : Array vertex
        , delaunayVertices : List (DelaunayVertex vertex coordinates units)
        , faces : List (DelaunayFace vertex coordinates units)
        }
