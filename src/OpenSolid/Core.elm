module OpenSolid.Core
  ( Interval
  , Vector2d
  , Direction2d
  , Point2d
  , Box2d
  , LineSegment2d
  , Transformation2d
  , Vector3d
  , Direction3d
  , Point3d
  , Box3d
  , Axis3d
  ) where


type alias Interval =
  { lowerBound: Float
  , upperBound: Float
  }


type alias Vector2d =
  { x: Float
  , y: Float
  }


type alias Direction2d =
  { x: Float
  , y: Float
  }


type alias Point2d =
  { x: Float
  , y: Float
  }


type alias Box2d =
  { x: Interval
  , y: Interval
  }


type alias LineSegment2d =
  { firstEndpoint: Point2d
  , secondEndpoint: Point2d
  }


type alias Transformation2d =
  { ofVector: Vector2d -> Vector2d
  , ofPoint: Point2d -> Point2d
  }


type alias Vector3d =
  { x: Float
  , y: Float
  , z: Float
  }


type alias Direction3d =
  { x: Float
  , y: Float
  , z: Float
  }


type alias Point3d =
  { x: Float
  , y: Float
  , z: Float
  }


type alias Box3d =
  { x: Interval
  , y: Interval
  , z: Interval
  }


type alias Axis3d =
  { originPoint: Point3d
  , direction: Direction3d
  }
