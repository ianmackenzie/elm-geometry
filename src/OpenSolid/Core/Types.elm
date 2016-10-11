{- This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, you can
   obtain one at http://mozilla.org/MPL/2.0/.

   Copyright 2016 by Ian Mackenzie
   ian.e.mackenzie@gmail.com
-}


module OpenSolid.Core.Types
    exposing
        ( Vector2d(Vector2d)
        , Vector3d(Vector3d)
        , Direction2d(Direction2d)
        , Direction3d(Direction3d)
        , Point2d(Point2d)
        , Point3d(Point3d)
        , Axis2d(Axis2d)
        , Axis3d(Axis3d)
        , Plane3d(Plane3d)
        , Frame2d(Frame2d)
        , Frame3d(Frame3d)
        , SketchPlane3d(SketchPlane3d)
        , LineSegment2d(LineSegment2d)
        , LineSegment3d(LineSegment3d)
        , Triangle2d(Triangle2d)
        , Triangle3d(Triangle3d)
        , BoundingBox2d(BoundingBox2d)
        , BoundingBox3d(BoundingBox3d)
        )

{-| This module contains the definitions of the core OpenSolid data types. Each
type also has a corresponding module providing related functionality. Suggested
practice is to import this module exposing everything, and import all other
necessary modules using `as`:

    import OpenSolid.Core.Types exposing (..)
    import OpenSolid.Vector2d as Vector2d
    import OpenSolid.Plane3d as Plane3d

Note that since the definitions of these types are exposed, it is possible to
construct them directly, and this is sometimes the most convenient or efficient
approach:

    vector =
        Vector2d ( 2, 3 )

    point =
        Point3d ( 3, 1, 4 )

    axis =
        Axis3d
            { originPoint = Point3d ( 1, 0, 2 )
            , direction = Direction3d ( 0, 1, 0 )
            }

Alternatively, you can use constructor functions like `Direction2d.fromAngle`,
or start with predefined values such as `Point3d.origin` and transform them as
necessary.

# Vectors

Vectors represent quantities such as displacements or velocities. OpenSolid
distinguishes between vectors and points, so points should be used instead of
vectors to represent positions.

@docs Vector2d, Vector3d

# Directions

A direction is effectively a vector with a length of one, used to represent
quantities like the direction of an axis. Directions can be directly constructed
from their components, but note that in this case you are responsible for
ensuring that the sum of the squares of the components is exactly one.

@docs Direction2d, Direction3d

# Points

Points represent positions in space. They are distinct from vectors but interact
with them in well-defined ways; you can translate a point by a vector to result
in a new point, or you can compute the vector from one point to another, but you
cannot 'add' two points like you can add two vectors.

@docs Point2d, Point3d

# Datums

Axes and planes are used extensively within OpenSolid to define different types
of transformations. For instance, rotation in 3D is defined by a rotation axis
and mirroring in 3D is defined by a mirror plane.

@docs Axis2d, Axis3d, Plane3d

# Frames

Frames represent local coordinate systems, and allow for conversions between
local and global coordinates. They are also a common source of datums used in
transformations, for example rotating around the Z axis of a 3D frame or
mirroring across its XY plane.

@docs Frame2d, Frame3d

# Sketch planes

@docs SketchPlane3d

# Bounding boxes

These types represent bounding boxes around other geometric objects, and are
useful building blocks for tasks such as fast spatial searching or culling non-
visible objects during 3D rendering.

@docs BoundingBox2d, BoundingBox3d
-}


{-| A vector in 2D, defined by its X and Y components.
-}
type Vector2d
    = Vector2d ( Float, Float )


{-| A vector in 3D, defined by its X, Y and Z components.
-}
type Vector3d
    = Vector3d ( Float, Float, Float )


{-| A direction in 2D, defined by its X and Y components.
-}
type Direction2d
    = Direction2d ( Float, Float )


{-| A direction in 3D, defined by its X, Y and Z components.
-}
type Direction3d
    = Direction3d ( Float, Float, Float )


{-| A point in 2D, defined by its X and Y coordinates.
-}
type Point2d
    = Point2d ( Float, Float )


{-| A point in 3D, defined by its X, Y and Z coordinates.
-}
type Point3d
    = Point3d ( Float, Float, Float )


{-| An axis in 2D, defined by an origin point and direction. Useful for several
operations including:
  - Mirroring across the axis
  - Projecting onto the axis
  - Measuring distance along the axis
-}
type Axis2d
    = Axis2d { originPoint : Point2d, direction : Direction2d }


{-| An axis in 3D, defined by an origin point and direction. Useful for several
operations including:
  - Rotating around the axis
  - Projecting onto the axis
  - Measuring distance along the axis
-}
type Axis3d
    = Axis3d { originPoint : Point3d, direction : Direction3d }


{-| A 3D plane, defined by an origin point and a normal direction. Useful for
several operations including:
  - Mirroring across the plane
  - Projecting onto the plane
  - Measuring distance from the plane
-}
type Plane3d
    = Plane3d { originPoint : Point3d, normalDirection : Direction3d }


{-| A `Frame2d` represents a coordinate system in 2D space.

The two basis directions are always perpendicular. If you construct a `Frame2d`
directly, you are responsible for ensuring this yourself.
-}
type Frame2d
    = Frame2d
        { originPoint : Point2d
        , xDirection : Direction2d
        , yDirection : Direction2d
        }


{-| A `Frame3d` represents a coordinate system in 3D space.

The three basis directions are always mutually perpendicular. If you construct a
`Frame3d` directly, you are responsible for ensuring this yourself.
-}
type Frame3d
    = Frame3d
        { originPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        , zDirection : Direction3d
        }


{-| A `SketchPlane3d` represents a 2D planar coordinate system in 3D space.
Used to convert between 2D and 3D coordinates, such as taking 2D lines and
placing them on a 3D plane, or projecting a 3D point into a 2D sketch.

The two basis directions are all always perpendicular. If you construct a
`SketchPlane3d` directly, you are responsible for ensuring this yourself.
-}
type SketchPlane3d
    = SketchPlane3d
        { originPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        }


{-| A line segment in 2D, defined by its start and end points.
-}
type LineSegment2d
    = LineSegment2d ( Point2d, Point2d )


{-| A line segment in 3D, defined by its start and end points.
-}
type LineSegment3d
    = LineSegment3d ( Point3d, Point3d )


{-| A triangle in 2D, defined by its three vertices.
-}
type Triangle2d
    = Triangle2d ( Point2d, Point2d, Point2d )


{-| A triangle in 3D, defined by its three vertices.
-}
type Triangle3d
    = Triangle3d ( Point3d, Point3d, Point3d )


{-| A bounding box in 2D, defined by its minimum and maximum X and Y values.

If you construct an `BoundingBox2d` directly from its values, you must ensure
that they are properly ordered: `minX <= maxX`, `minY <= maxY`.
-}
type BoundingBox2d
    = BoundingBox2d
        { minX : Float
        , maxX : Float
        , minY : Float
        , maxY : Float
        }


{-| A bounding box in 3D, defined by its minimum and maximum X, Y and Z values.

If you construct an `BoundingBox3d` directly from its values, you must ensure
that they are properly ordered: `minX <= maxX`, `minY <= maxY`, `minZ <= maxZ`.
-}
type BoundingBox3d
    = BoundingBox3d
        { minX : Float
        , maxX : Float
        , minY : Float
        , maxY : Float
        , minZ : Float
        , maxZ : Float
        }
