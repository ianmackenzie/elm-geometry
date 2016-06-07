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
        , Axis2d
        , Axis3d
        , Plane3d
        , Frame2d
        , Frame3d
        )

{-| This module contains the definitions of the core OpenSolid geometric data
types. Each type also has a corresponding module providing related
functionality. Suggested practice is to import this module exposing everything,
and import all other necessary modules using `as`:

    import OpenSolid.Core.Types exposing (..)
    import OpenSolid.Core.Vector2d as Vector2d
    import OpenSolid.Core.Plane3d as Plane3d

Note that since these types are not opaque, it is possible to construct them
directly, and this is sometimes the most convenient or efficient approach.
However, in most cases it is safer and easier to use the various functions in
the associated modules, since many types have restrictions on how they must be
defined. For instance, the `Vector2d` defining a `Direction2d` must have a
length of one; instead of attempting to guarantee this yourself it is usually
better to use a constructor function like `Direction2d.fromAngle` or start with
an existing direction such as `Direction2d.y` and transform it as necessary. For
more complex types like `Plane3d` this is even more true!

# Vectors

Vectors represent quantities such as displacements or velocities. OpenSolid
distinguishes between vectors and points, so points should be used instead of
vectors to represent positions.

@docs Vector2d, Vector3d

# Directions

A direction is effectively a vector with a length of one, used to represent
quantities like the direction of an axis. Directions are internally represented
by vectors and can be constructed directly from them, but note that in this case
you are responsible for ensuring that the given vector's length is exactly one.

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

@docs Frame2d, Frame3d
-}


{-| A vector in 2D, defined by its X and Y components.
-}
type Vector2d
    = Vector2d Float Float


{-| A vector in 3D, defined by its X, Y and Z components.
-}
type Vector3d
    = Vector3d Float Float Float


{-| A direction in 2D, defined by a vector with length one.
-}
type Direction2d
    = Direction2d Vector2d


{-| A direction in 3D, defined by a vector with length one.
-}
type Direction3d
    = Direction3d Vector3d


{-| A point in 2D, defined by its X and Y components.
-}
type Point2d
    = Point2d Float Float


{-| A point in 3D, defined by its X, Y and Z components.
-}
type Point3d
    = Point3d Float Float Float


{-| An axis in 2D, defined by an origin point and direction. Useful for several
operations including:
  - Mirroring about the axis
  - Projecting onto the axis
  - Measuring distance along the axis
-}
type alias Axis2d =
    { originPoint : Point2d
    , direction : Direction2d
    }


{-| An axis in 3D, defined by an origin point and direction. Useful for several
operations including:
  - Rotating about the axis
  - Projecting onto the axis
  - Measuring distance along the axis
-}
type alias Axis3d =
    { originPoint : Point3d
    , direction : Direction3d
    }


{-| A 3D plane, defined by an origin point, two basis directions and a normal
direction. Useful for several operations including:
  - Mirroring about the plane
  - Projecting onto the plane
  - Placing 2D objects onto the plane to result in 3D objects
    (converting from 2D coordinates within the plane to global 3D coordinates)
  - Projecting 3D objects into the plane to result in 2D objects
    (converting from global 3D coordinates to 2D coordinates within the plane)

The basis directions and normal direction are all always mutually perpendicular.
If you construct a `Plane3d` directly, you are responsible for ensuring this
yourself.
-}
type alias Plane3d =
    { originPoint : Point3d
    , xDirection : Direction3d
    , yDirection : Direction3d
    , normalDirection : Direction3d
    }


{-| A `Frame2d` represents a coordinate system in 2D space.

The two basis directions are always perpendicular. If you construct a `Frame2d`
directly, you are responsible for ensuring this yourself.
-}
type alias Frame2d =
    { originPoint : Point2d
    , xDirection : Direction2d
    , yDirection : Direction2d
    }


{-| A `Frame3d` represents a coordinate system in 3D space.

The three basis directions are always mutually perpendicular. If you construct a
`Frame3d` directly, you are responsible for ensuring this yourself.
-}
type alias Frame3d =
    { originPoint : Point3d
    , xDirection : Direction3d
    , yDirection : Direction3d
    , zDirection : Direction3d
    }
