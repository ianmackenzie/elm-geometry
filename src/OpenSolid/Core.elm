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


{-| This module contains the definitions of the core OpenSolid geometric data types. Each type also
has a corresponding module providing related functionality. Suggested practice is to import this
module exposing everything, and import all other necessary modules using `as`:

    import OpenSolid.Core exposing (..)
    import OpenSolid.Core.Vector2d as Vector2d
    import OpenSolid.Core.Plane3d as Plane3d

Note that since these types are not opaque, it is possible to construct them directly, and this is
sometimes the most convenient or efficient approach. However, in most cases it is safer and easier
to use the various functions in the associated modules, since many types have restrictions on how
they must be defined. For instance, the `Vector2d` defining a `Direction2d` must have a length of
one; instead of attempting to guarantee this yourself it is usually better to use a constructor
function like `Direction2d.fromAngle` or start with an existing direction such as `Direction2d.y`
and transform it as necessary. For more complex types like `Plane3d` this is even more true!

# Vectors

Vectors represent quantities such as displacements or velocities. OpenSolid distinguishes between
vectors and points, so points should be used instead of vectors to represent positions.

@docs Vector2d, Vector3d

# Directions

A direction is effectively a vector with a length of one, used to represent quantities like the
direction of an axis. Directions are internally represented as vectors and can be constructed
directly from them, but note that in this case you are responsible for ensuring that the given
vector's length is exactly one.

@docs Direction2d, Direction3d

# Points

Points represent positions in space. They are distinct from vectors but interact with them in
well-defined ways; you can add a vector to a point to result in a new point, or you can compute the
vector from one point to another, but you cannot 'add' two points like you can add two vectors.

@docs Point2d, Point3d

# Line segments

Line segments are defined by their start and end points.

@docs LineSegment2d, LineSegment3d

# Triangles

Triangles are defined by the positions of their three vertices, given in counterclockwise order.
Some operations such as computing the area of a 2D triangle or the normal direction of a 3D triangle
are affected by vertex order; typically reversing the vertex order will result in the sign of the
result of these operations being flipped.

@docs Triangle2d, Triangle3d

# Datums

Axes and planes are used extensively within OpenSolid to define different types of transformations.
For instance, rotation in 3D is defined by a rotation axis and mirroring in 3D is defined by a
mirror plane.

@docs Axis2d, Axis3d, Plane3d

# Frames

Frames represent local coordinate systems, and allow for conversion between local and global
coordinates. They are also a common source of datums used in transformations, for example rotating
about the Z axis of a 3D frame or mirroring about its XY plane.

@docs Frame2d, Frame3d

# Bounds types

These types represent bounding boxes around other geometric objects, and are useful building blocks
for tasks such as fast spatial searching or culling non-visible objects during 3D rendering.

@docs Interval, BoundingBox2d, BoundingBox3d

-}


{-| A vector in 2D, defined by its X and Y components. -}
type Vector2d
  = Vector2d Float Float


{-| A vector in 3D, defined by its X, Y and Z components. -}
type Vector3d
  = Vector3d Float Float Float


{-| A direction in 2D, defined by a vector with length one. -}
type Direction2d
  = Direction2d Vector2d


{-| A direction in 3D, defined by a vector with length one. -}
type Direction3d
  = Direction3d Vector3d


{-| A point in 2D, defined by its X and Y components. -}
type Point2d
  = Point2d Float Float


{-| A point in 3D, defined by its X, Y and Z components. -}
type Point3d
  = Point3d Float Float Float


{-| An interval represents a range of floating-point numbers between an upper and lower bound.

Note that if you construct an `Interval` directly from two values, the first must be less than or
equal to the second. `Scalar.hull` provides a convenient way to construct an interval from two
values if it is not known ahead of time which is lesser.

-}
type Interval
  = Interval Float Float


{-| A bounding box in 2D, defined by X and Y intervals. -}
type BoundingBox2d
  = BoundingBox2d Interval Interval


{-| A bounding box in 3D, defined by X, Y and Z intervals. -}
type BoundingBox3d
  = BoundingBox3d Interval Interval Interval


{-| A line segment in 2D. -}
type LineSegment2d
  = LineSegment2d Point2d Point2d


{-| A line segment in 3D. -}
type LineSegment3d
  = LineSegment3d Point3d Point3d


{-| A triangle in 2D. -}
type Triangle2d
  = Triangle2d Point2d Point2d Point2d


{-| A triangle in 3D. -}
type Triangle3d
  = Triangle3d Point3d Point3d Point3d


-- Datums


{-| An axis in 2D, defined by an origin point and direction. Useful for several operations
including:
  - Mirroring about the axis
  - Projecting onto the axis
  - Measuring distance along the axis

-}
type alias Axis2d =
  { originPoint: Point2d
  , direction: Direction2d
  }


{-| An axis in 3D, defined by an origin point and direction. Useful for several operations
including:
  - Rotating about the axis
  - Projecting onto the axis
  - Measuring distance along the axis

-}
type alias Axis3d =
  { originPoint: Point3d
  , direction: Direction3d
  }


{-| A 3D plane, defined by an origin point, two basis directions and a normal direction. Useful for
several operations including:
  - Mirroring about the plane
  - Projecting onto the plane
  - Placing 2D objects onto the plane to result in 3D objects
    (converting from 2D coordinates within the plane to global 3D coordinates)
  - Projecting 3D objects into the plane to result in 2D objects
    (converting from global 3D coordinates to 2D coordinates within the plane)

The basis directions and normal direction are all always mutually perpendicular. If you construct a
`Plane3d` directly, you are responsible for ensuring this yourself.

-}
type alias Plane3d =
  { originPoint: Point3d
  , xDirection: Direction3d
  , yDirection: Direction3d
  , normalDirection: Direction3d
  }


{-| A `Frame2d` represents a coordinate system in 2D space.

The two basis directions are always perpendicular. If you construct a `Frame2d` directly, you are
responsible for ensuring this yourself.

-}
type alias Frame2d =
  { originPoint: Point2d
  , xDirection: Direction2d
  , yDirection: Direction2d
  }


{-| A `Frame3d` represents a coordinate system in 3D space.

The three basis directions are always mutually perpendicular. If you construct a `Frame3d` directly,
you are responsible for ensuring this yourself.

-}
type alias Frame3d =
  { originPoint: Point3d
  , xDirection: Direction3d
  , yDirection: Direction3d
  , zDirection: Direction3d
  }
