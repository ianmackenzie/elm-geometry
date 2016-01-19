module OpenSolid.Core
  ( Vector2d (Vector2d)
  , Vector3d (Vector3d)
  , Point2d (Point2d)
  , Point3d (Point3d)
  , Direction2d (Direction2d)
  , Direction3d (Direction3d)
  , Axis2d (Axis2d)
  , Axis3d (Axis3d)
  , Plane3d (Plane3d)
  , Frame2d (Frame2d)
  , Frame3d (Frame3d)
  , Transformation2d (Transformation2d)
  , Transformation3d (Transformation3d)
  , LineSegment2d (LineSegment2d)
  , LineSegment3d (LineSegment3d)
  , Triangle2d (Triangle2d)
  , Triangle3d (Triangle3d)
  , Interval (Interval)
  , Box2d (Box2d)
  , Box3d (Box3d)
  ) where


-- Primitives


type Vector2d
  = Vector2d Float Float


type Vector3d
  = Vector3d Float Float Float


type Point2d
  = Point2d Float Float


type Point3d
  = Point3d Float Float Float


type Direction2d
  = Direction2d Vector2d


type Direction3d
  = Direction3d Vector3d


-- Datums


type Axis2d
  = Axis2d Point2d Direction2d


type Axis3d
  = Axis3d Point3d Direction3d


type Plane3d
  = Plane3d Point3d Direction3d Direction3d Direction3d


type Frame2d
  = Frame2d Point2d Direction2d Direction2d


type Frame3d
  = Frame3d Point3d Direction3d Direction3d Direction3d


-- Transformations


type Transformation2d
  = Transformation2d (Vector2d -> Vector2d) (Point2d -> Point2d)


type Transformation3d
  = Transformation3d (Vector3d -> Vector3d) (Point3d -> Point3d)


-- Simplices


type LineSegment2d
  = LineSegment2d Point2d Point2d


type LineSegment3d
  = LineSegment3d Point3d Point3d


type Triangle2d
  = Triangle2d Point2d Point2d Point2d


type Triangle3d
  = Triangle3d Point3d Point3d Point3d


-- Bounds types


type Interval
  = Interval Float Float


type Box2d
  = Box2d Interval Interval


type Box3d
  = Box3d Interval Interval Interval
