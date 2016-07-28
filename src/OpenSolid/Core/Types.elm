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
        , PlanarFrame3d(PlanarFrame3d)
        )

{-| This module contains the definitions of the core OpenSolid data types. Each
type also has a corresponding module providing related functionality. Suggested
practice is to import this module exposing everything, and import all other
necessary modules using `as`:

    import OpenSolid.Core.Types exposing (..)
    import OpenSolid.Core.Vector2d as Vector2d
    import OpenSolid.Core.Plane3d as Plane3d

Note that since these types are not opaque, it is possible to construct them
directly, and this is sometimes the most convenient or efficient approach.
However, this can also circumvent the otherwise rigorous type safety of
OpenSolid, since it makes it possible to define nonsense objects that do not
satisfy certain invariants that are otherwise always maintained. Specifically:

  - The components defining a `Direction2d` or `Direction3d` must be properly
    normalized so that the direction has a 'length' of one.
  - The X and Y directions of a `Frame2d` or `PlanarFrame3d` must be
    perpendicular to each other.
  - The X, Y, and Z directions of a `Frame3d` must all be mutually
    perpendicular.

For example, instead of constructing a `Direction2d` directly from its
components, it is usually better to use a constructor function like
`Direction2d.fromAngle` or start with an existing direction like `Direction2d.y`
and transform it as necessary. For more complex types like `Plane3d` this is
even more true! All OpenSolid functions (transformations etc.) will properly
maintain the invariants described above, so for example it is impossible to
produce a `Frame2d` with non-perpendicular X and Y directions except by directly
constructing one.

# Vectors

Vectors represent quantities such as displacements or velocities. OpenSolid
distinguishes between vectors and points, so points should be used instead of
vectors to represent positions.

@docs Vector2d, Vector3d

# Directions

A direction is effectively a vector with a length of one, used to represent
quantities like the direction of an axis. Directions can be directly constructed
from their components, but note that in this case you are responsible for
ensuring that the direction's 'length' is exactly one.

@docs Direction2d, Direction3d

# Points

Points represent positions in space. They are distinct from vectors but interact
with them in well-defined ways; you can add a vector to a point to result in a
new point, or you can compute the vector from one point to another, but you
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
transformations, for example rotating about the Z axis of a 3D frame or
mirroring about its XY plane.

@docs Frame2d, Frame3d, PlanarFrame3d
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


{-| A `PlanarFrame3d` represents a 2D planar coordinate system in 3D space.
Used to convert between 2D and 3D coordinates, such as taking 2D lines and
placing them on a 3D plane, or projecting a 3D point into a 2D sketch.

The two basis directions are all always perpendicular. If you construct a
`PlanarFrame3d` directly, you are responsible for ensuring this yourself.
-}
type PlanarFrame3d
    = PlanarFrame3d
        { originPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        }
