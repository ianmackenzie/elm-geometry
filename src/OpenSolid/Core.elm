module OpenSolid.Core
  ( Vector2d
  , Vector3d
  , Point2d
  , Point3d
  , Direction2d
  , Direction3d
  , Axis2d
  , Axis3d
  , Transformation2d
  , Transformation3d
  , LineSegment2d
  , LineSegment3d
  , Interval
  , Box2d
  , Box3d
  ) where


-- Primitives


type alias Vector2d =
  { x: Float
  , y: Float
  }


type alias Vector3d =
  { x: Float
  , y: Float
  , z: Float
  }


type alias Point2d =
  { x: Float
  , y: Float
  }


type alias Point3d =
  { x: Float
  , y: Float
  , z: Float
  }


type alias Direction2d =
  { x: Float
  , y: Float
  }


type alias Direction3d =
  { x: Float
  , y: Float
  , z: Float
  }


-- Datums


type alias Axis2d =
  { originPoint: Point2d
  , direction: Direction2d
  }


type alias Axis3d =
  { originPoint: Point3d
  , direction: Direction3d
  }


-- Transformations


type alias Transformation2d =
  ( Vector2d -> Vector2d
  , Point2d -> Point2d
  )


type alias Transformation3d =
  ( Vector3d -> Vector3d
  , Point3d -> Point3d
  )


-- Simplices


type alias LineSegment2d =
  { firstEndpoint: Point2d
  , secondEndpoint: Point2d
  }


type alias LineSegment3d =
  { firstEndpoint: Point3d
  , secondEndpoint: Point3d
  }


-- Bounds types


type alias Interval =
  { lowerBound: Float
  , upperBound: Float
  }


type alias Box2d =
  { x: Interval
  , y: Interval
  }


type alias Box3d =
  { x: Interval
  , y: Interval
  , z: Interval
  }
