module OpenSolid.Core
  ( Vector2d(Vector2d)
  , Vector3d(Vector3d)
  , Direction2d(Direction2d)
  , Direction3d(Direction3d)
  , Point2d(Point2d)
  , Point3d(Point3d)
  , Interval(Interval)
  , BoundingBox2d(BoundingBox2d)
  , BoundingBox3d(BoundingBox3d)
  , LineSegment2d(LineSegment2d)
  , LineSegment3d(LineSegment3d)
  , Triangle2d(Triangle2d)
  , Triangle3d(Triangle3d)
  , Axis2d
  , Axis3d
  , Plane3d
  , Frame2d
  , Frame3d
  ) where


-- Cartesian types


type Vector2d
  = Vector2d Float Float


type Vector3d
  = Vector3d Float Float Float


type Direction2d
  = Direction2d Vector2d


type Direction3d
  = Direction3d Vector3d


type Point2d
  = Point2d Float Float


type Point3d
  = Point3d Float Float Float


-- Bounds types


type Interval
  = Interval Float Float


type BoundingBox2d
  = BoundingBox2d Interval Interval


type BoundingBox3d
  = BoundingBox3d Interval Interval Interval


-- Simplices


type LineSegment2d
  = LineSegment2d Point2d Point2d


type LineSegment3d
  = LineSegment3d Point3d Point3d


type Triangle2d
  = Triangle2d Point2d Point2d Point2d


type Triangle3d
  = Triangle3d Point3d Point3d Point3d


-- Datums


type alias Axis2d =
  { originPoint: Point2d
  , direction: Direction2d
  }


type alias Axis3d =
  { originPoint: Point3d
  , direction: Direction3d
  }


type alias Plane3d =
  { originPoint: Point3d
  , xDirection: Direction3d
  , yDirection: Direction3d
  , normalDirection: Direction3d
  }


type alias Frame2d =
  { originPoint: Point2d
  , xDirection: Direction2d
  , yDirection: Direction2d
  }


type alias Frame3d =
  { originPoint: Point3d
  , xDirection: Direction3d
  , yDirection: Direction3d
  , zDirection: Direction3d
  }
