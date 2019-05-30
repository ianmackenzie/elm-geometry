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
    , BlockCoordinates(..)
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
    , EllipseCoordinates(..)
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
    , RectangleCoordinates(..)
    , SketchPlane3d(..)
    , Sphere3d(..)
    , SweptAngle(..)
    , Triangle2d(..)
    , Triangle3d(..)
    , Vector2d(..)
    , Vector3d(..)
    )

import Angle exposing (Angle)
import Array exposing (Array)
import Quantity exposing (Quantity)


type Vector2d units coordinates
    = Vector2d ( Quantity Float units, Quantity Float units )


type Vector3d units coordinates
    = Vector3d ( Quantity Float units, Quantity Float units, Quantity Float units )


type Direction2d coordinates
    = Direction2d ( Float, Float )


type Direction3d coordinates
    = Direction3d ( Float, Float, Float )


type Point2d units coordinates
    = Point2d ( Quantity Float units, Quantity Float units )


type Point3d units coordinates
    = Point3d ( Quantity Float units, Quantity Float units, Quantity Float units )


type Axis2d units coordinates
    = Axis2d { originPoint : Point2d units coordinates, direction : Direction2d coordinates }


type Axis3d units coordinates
    = Axis3d { originPoint : Point3d units coordinates, direction : Direction3d coordinates }


type Plane3d units coordinates
    = Plane3d { originPoint : Point3d units coordinates, normalDirection : Direction3d coordinates }


type Frame2d units coordinates1 coordinates2
    = Frame2d
        { originPoint : Point2d units coordinates1
        , xDirection : Direction2d coordinates1
        , yDirection : Direction2d coordinates1
        }


type Frame3d units coordinates1 coordinates2
    = Frame3d
        { originPoint : Point3d units coordinates1
        , xDirection : Direction3d coordinates1
        , yDirection : Direction3d coordinates1
        , zDirection : Direction3d coordinates1
        }


type SketchPlane3d units coordinates1 coordinates2
    = SketchPlane3d
        { originPoint : Point3d units coordinates1
        , xDirection : Direction3d coordinates1
        , yDirection : Direction3d coordinates1
        }


type LineSegment2d units coordinates
    = LineSegment2d ( Point2d units coordinates, Point2d units coordinates )


type LineSegment3d units coordinates
    = LineSegment3d ( Point3d units coordinates, Point3d units coordinates )


type Triangle2d units coordinates
    = Triangle2d ( Point2d units coordinates, Point2d units coordinates, Point2d units coordinates )


type Triangle3d units coordinates
    = Triangle3d ( Point3d units coordinates, Point3d units coordinates, Point3d units coordinates )


type BoundingBox2d units coordinates
    = BoundingBox2d
        { minX : Quantity Float units
        , maxX : Quantity Float units
        , minY : Quantity Float units
        , maxY : Quantity Float units
        }


type BoundingBox3d units coordinates
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


type Rectangle2d units coordinates
    = Rectangle2d
        { axes : Frame2d units coordinates RectangleCoordinates
        , dimensions : ( Quantity Float units, Quantity Float units )
        }


type Rectangle3d units coordinates
    = Rectangle3d
        { axes : SketchPlane3d units coordinates RectangleCoordinates
        , dimensions : ( Quantity Float units, Quantity Float units )
        }


type BlockCoordinates
    = BlockCoordinates


type Block3d units coordinates
    = Block3d
        { axes : Frame3d units coordinates BlockCoordinates
        , dimensions : ( Quantity Float units, Quantity Float units, Quantity Float units )
        }


type Polyline2d units coordinates
    = Polyline2d (List (Point2d units coordinates))


type Polyline3d units coordinates
    = Polyline3d (List (Point3d units coordinates))


type Polygon2d units coordinates
    = Polygon2d
        { outerLoop : List (Point2d units coordinates)
        , innerLoops : List (List (Point2d units coordinates))
        }


type Circle2d units coordinates
    = Circle2d
        { centerPoint : Point2d units coordinates
        , radius : Quantity Float units
        }


type Circle3d units coordinates
    = Circle3d
        { centerPoint : Point3d units coordinates
        , axialDirection : Direction3d coordinates
        , radius : Quantity Float units
        }


type EllipseCoordinates
    = EllipseCoordinates


type Ellipse2d units coordinates
    = Ellipse2d
        { axes : Frame2d units coordinates EllipseCoordinates
        , xRadius : Quantity Float units
        , yRadius : Quantity Float units
        }


type Sphere3d units coordinates
    = Sphere3d
        { centerPoint : Point3d units coordinates
        , radius : Quantity Float units
        }


type SweptAngle
    = SmallPositive
    | SmallNegative
    | LargePositive
    | LargeNegative


type Arc2d units coordinates
    = Arc2d
        { startPoint : Point2d units coordinates
        , xDirection : Direction2d coordinates
        , signedLength : Quantity Float units
        , sweptAngle : Angle
        }


type Arc3d units coordinates
    = Arc3d
        { startPoint : Point3d units coordinates
        , xDirection : Direction3d coordinates
        , yDirection : Direction3d coordinates
        , signedLength : Quantity Float units
        , sweptAngle : Angle
        }


type QuadraticSpline2d units coordinates
    = QuadraticSpline2d
        { firstControlPoint : Point2d units coordinates
        , secondControlPoint : Point2d units coordinates
        , thirdControlPoint : Point2d units coordinates
        }


type QuadraticSpline3d units coordinates
    = QuadraticSpline3d
        { firstControlPoint : Point3d units coordinates
        , secondControlPoint : Point3d units coordinates
        , thirdControlPoint : Point3d units coordinates
        }


type CubicSpline2d units coordinates
    = CubicSpline2d
        { firstControlPoint : Point2d units coordinates
        , secondControlPoint : Point2d units coordinates
        , thirdControlPoint : Point2d units coordinates
        , fourthControlPoint : Point2d units coordinates
        }


type CubicSpline3d units coordinates
    = CubicSpline3d
        { firstControlPoint : Point3d units coordinates
        , secondControlPoint : Point3d units coordinates
        , thirdControlPoint : Point3d units coordinates
        , fourthControlPoint : Point3d units coordinates
        }


type EllipticalArc2d units coordinates
    = EllipticalArc2d
        { ellipse : Ellipse2d units coordinates
        , startAngle : Angle
        , sweptAngle : Angle
        }


type alias DelaunayVertex vertex units coordinates =
    { vertex : vertex
    , index : Int
    , position : Point2d units coordinates
    }


type DelaunayFace vertex units coordinates
    = ThreeVertexFace (DelaunayVertex vertex units coordinates) (DelaunayVertex vertex units coordinates) (DelaunayVertex vertex units coordinates) (Circle2d units coordinates)
    | TwoVertexFace (DelaunayVertex vertex units coordinates) (DelaunayVertex vertex units coordinates) Int (Direction2d coordinates)
    | OneVertexFace (DelaunayVertex vertex units coordinates) Int Int (Direction2d coordinates)


type DelaunayTriangulation2d vertex units coordinates
    = EmptyDelaunayTriangulation2d
    | DelaunayTriangulation2d
        { vertices : Array vertex
        , delaunayVertices : List (DelaunayVertex vertex units coordinates)
        , faces : List (DelaunayFace vertex units coordinates)
        }
