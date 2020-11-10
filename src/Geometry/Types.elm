--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Geometry.Types exposing (..)

import Angle exposing (Angle, Radians)
import Array exposing (Array)
import Quantity exposing (Quantity)
import Quantity.Interval exposing (Interval)


type Vector2d units coordinates
    = Vector2d { x : Float, y : Float }


type Vector3d units coordinates
    = Vector3d { x : Float, y : Float, z : Float }


type Direction2d coordinates
    = Direction2d { x : Float, y : Float }


type Direction3d coordinates
    = Direction3d { x : Float, y : Float, z : Float }


type Point2d units coordinates
    = Point2d { x : Float, y : Float }


type Point3d units coordinates
    = Point3d { x : Float, y : Float, z : Float }


type Axis2d units coordinates
    = Axis2d { originPoint : Point2d units coordinates, direction : Direction2d coordinates }


type Axis3d units coordinates
    = Axis3d { originPoint : Point3d units coordinates, direction : Direction3d coordinates }


type Plane3d units coordinates
    = Plane3d { originPoint : Point3d units coordinates, normalDirection : Direction3d coordinates }


type Frame2d units coordinates defines
    = Frame2d
        { originPoint : Point2d units coordinates
        , xDirection : Direction2d coordinates
        , yDirection : Direction2d coordinates
        }


type Frame3d units coordinates defines
    = Frame3d
        { originPoint : Point3d units coordinates
        , xDirection : Direction3d coordinates
        , yDirection : Direction3d coordinates
        , zDirection : Direction3d coordinates
        }


type SketchPlane3d units coordinates defines
    = SketchPlane3d
        { originPoint : Point3d units coordinates
        , xDirection : Direction3d coordinates
        , yDirection : Direction3d coordinates
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


type Rectangle2d units coordinates
    = Rectangle2d
        { axes : Frame2d units coordinates {}
        , dimensions : ( Quantity Float units, Quantity Float units )
        }


type Rectangle3d units coordinates
    = Rectangle3d
        { axes : SketchPlane3d units coordinates {}
        , dimensions : ( Quantity Float units, Quantity Float units )
        }


type Block3d units coordinates
    = Block3d
        { axes : Frame3d units coordinates {}
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


type Ellipse2d units coordinates
    = Ellipse2d
        { axes : Frame2d units coordinates {}
        , xRadius : Quantity Float units
        , yRadius : Quantity Float units
        }


type Ellipse3d units coordinates
    = Ellipse3d
        { axes : SketchPlane3d units coordinates {}
        , xRadius : Quantity Float units
        , yRadius : Quantity Float units
        }


type Ellipsoid3d units coordinates
    = Ellipsoid3d
        { axes : Frame3d units coordinates {}
        , xRadius : Quantity Float units
        , yRadius : Quantity Float units
        , zRadius : Quantity Float units
        }


type Sphere3d units coordinates
    = Sphere3d
        { centerPoint : Point3d units coordinates
        , radius : Quantity Float units
        }


type Cylinder3d units coordinates
    = Cylinder3d
        { axis : Axis3d units coordinates
        , radius : Quantity Float units
        , length : Quantity Float units
        }


type Cone3d units coordinates
    = Cone3d
        { axis : Axis3d units coordinates
        , radius : Quantity Float units
        , length : Quantity Float units
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


type RationalQuadraticSpline2d units coordinates
    = RationalQuadraticSpline2d
        { firstControlPoint : Point2d units coordinates
        , secondControlPoint : Point2d units coordinates
        , thirdControlPoint : Point2d units coordinates
        , firstWeight : Float
        , secondWeight : Float
        , thirdWeight : Float
        }


type RationalQuadraticSpline3d units coordinates
    = RationalQuadraticSpline3d
        { firstControlPoint : Point3d units coordinates
        , secondControlPoint : Point3d units coordinates
        , thirdControlPoint : Point3d units coordinates
        , firstWeight : Float
        , secondWeight : Float
        , thirdWeight : Float
        }


type RationalCubicSpline2d units coordinates
    = RationalCubicSpline2d
        { firstControlPoint : Point2d units coordinates
        , secondControlPoint : Point2d units coordinates
        , thirdControlPoint : Point2d units coordinates
        , fourthControlPoint : Point2d units coordinates
        , firstWeight : Float
        , secondWeight : Float
        , thirdWeight : Float
        , fourthWeight : Float
        }


type RationalCubicSpline3d units coordinates
    = RationalCubicSpline3d
        { firstControlPoint : Point3d units coordinates
        , secondControlPoint : Point3d units coordinates
        , thirdControlPoint : Point3d units coordinates
        , fourthControlPoint : Point3d units coordinates
        , firstWeight : Float
        , secondWeight : Float
        , thirdWeight : Float
        , fourthWeight : Float
        }


type EllipticalArc2d units coordinates
    = EllipticalArc2d
        { ellipse : Ellipse2d units coordinates
        , startAngle : Angle
        , sweptAngle : Angle
        }


type EllipticalArc3d units coordinates
    = EllipticalArc3d
        { ellipse : Ellipse3d units coordinates
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


type Curve2d units coordinates
    = LineSegmentCurve2d (LineSegment2d units coordinates)
    | ArcCurve2d (Arc2d units coordinates)
    | EllipticalArcCurve2d (EllipticalArc2d units coordinates)
    | QuadraticSplineCurve2d (QuadraticSpline2d units coordinates)
    | CubicSplineCurve2d (CubicSpline2d units coordinates)


type Curve3d units coordinates
    = LineSegmentCurve3d (LineSegment3d units coordinates)
    | ArcCurve3d (Arc3d units coordinates)
    | EllipticalArcCurve3d (EllipticalArc3d units coordinates)
    | QuadraticSplineCurve3d (QuadraticSpline3d units coordinates)
    | CubicSplineCurve3d (CubicSpline3d units coordinates)


type SurfaceHandedness
    = RightHandedSurface
    | LeftHandedSurface


type SketchCoordinates
    = SketchCoordinates


type Surface3d units coordinates
    = TriangularSurface SurfaceHandedness (Triangle3d units coordinates)
    | RectangularSurface SurfaceHandedness (Rectangle3d units coordinates)
    | CircularSurface (Circle3d units coordinates)
    | ExtrusionSurface SurfaceHandedness (Curve3d units coordinates) (Vector3d units coordinates)
    | RevolutionSurface SurfaceHandedness (Curve3d units coordinates) (Frame3d units coordinates Never) Angle
    | PlanarSurface SurfaceHandedness (Region2d units SketchCoordinates) (SketchPlane3d units coordinates { defines : SketchCoordinates })


type Region2d units coordinates
    = EmptyRegion
    | TriangularRegion (Triangle2d units coordinates)
    | RectangularRegion (Rectangle2d units coordinates)
    | CircularRegion (Circle2d units coordinates)
    | EllipticalRegion (Ellipse2d units coordinates)
    | PolygonalRegion (Polygon2d units coordinates)
    | BoundedRegion (List (Curve2d units coordinates)) (List (List (Curve2d units coordinates)))


type Body3d units coordinates
    = EmptyBody
    | RectangularBody (Block3d units coordinates)
    | SphericalBody (Sphere3d units coordinates)
    | CylindricalBody (Cylinder3d units coordinates)
    | ConicalBody (Cone3d units coordinates)
    | ExtrusionBody (SketchPlane3d units coordinates { defines : SketchCoordinates }) (Region2d units SketchCoordinates) (Interval Float units)
    | RevolutionBody (SketchPlane3d units coordinates { defines : SketchCoordinates }) (Region2d units SketchCoordinates) (Axis2d units SketchCoordinates) (Interval Float Radians)
    | BoundedBody (List (Surface3d units coordinates))
