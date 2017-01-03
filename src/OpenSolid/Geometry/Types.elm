--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--                                                                            --
-- Copyright 2016 by Ian Mackenzie                                            --
-- ian.e.mackenzie@gmail.com                                                  --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module OpenSolid.Geometry.Types
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
        , Polyline2d(Polyline2d)
        , Polyline3d(Polyline3d)
        , Polygon2d(Polygon2d)
        , Circle2d(Circle2d)
        )

{-| This module contains the definitions of the core OpenSolid data types. Each
type also has a corresponding module providing related functionality. Suggested
practice is to import this module exposing everything, and import all other
necessary modules using `as`, for example:

    import OpenSolid.Geometry.Types exposing (..)
    import OpenSolid.Vector2d as Vector2d
    import OpenSolid.Plane3d as Plane3d

# Vectors

Vectors represent quantities such as displacements or velocities. OpenSolid
distinguishes between vectors and points, so points should be used instead of
vectors to represent positions.

@docs Vector2d, Vector3d

# Directions

A direction is effectively a vector with a length of one, used to represent
quantities like the direction of an axis or the normal direction of a plane.

Directions can be directly constructed from their components, but note that in
this case you are responsible for ensuring that the sum of the squares of the
components is exactly one. In many cases it is easier and safer to use the
various constructors and transformation functions in corresponding modules
instead.

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

# Line segments

@docs LineSegment2d, LineSegment3d

# Triangles

@docs Triangle2d, Triangle3d

# Bounding boxes

These types represent bounding boxes around other geometric objects, and are
useful for tasks such as fast spatial searching or culling non-visible objects
during 3D rendering.

@docs BoundingBox2d, BoundingBox3d

# Polylines

@docs Polyline2d, Polyline3d

# Polygons

@docs Polygon2d

# Circles

@docs Circle2d
-}


{-| A vector in 2D, defined by its X and Y components. See the `Vector2d`
module for details.
-}
type Vector2d
    = Vector2d ( Float, Float )


{-| A vector in 3D, defined by its X, Y and Z components. See the `Vector3d`
module for details.
-}
type Vector3d
    = Vector3d ( Float, Float, Float )


{-| A direction in 2D, defined by its X and Y components. See the
`Direction2d` module for details.
-}
type Direction2d
    = Direction2d ( Float, Float )


{-| A direction in 3D, defined by its X, Y and Z components. See the
`Direction3d` module for details.
-}
type Direction3d
    = Direction3d ( Float, Float, Float )


{-| A point in 2D, defined by its X and Y coordinates. See the `Point2d`
module for details.
-}
type Point2d
    = Point2d ( Float, Float )


{-| A point in 3D, defined by its X, Y and Z coordinates. See the `Point3d`
module for details.
-}
type Point3d
    = Point3d ( Float, Float, Float )


{-| An axis in 2D, defined by an origin point and direction. See the `Axis2d`
module for details.
-}
type Axis2d
    = Axis2d { originPoint : Point2d, direction : Direction2d }


{-| An axis in 3D, defined by an origin point and direction. See the `Axis3d`
module for details.
-}
type Axis3d
    = Axis3d { originPoint : Point3d, direction : Direction3d }


{-| A 3D plane, defined by an origin point and a normal direction. See the
`Plane3d` module for details.
-}
type Plane3d
    = Plane3d { originPoint : Point3d, normalDirection : Direction3d }


{-| A `Frame2d` represents a coordinate system in 2D space. See the `Frame2d`
module for details.
-}
type Frame2d
    = Frame2d
        { originPoint : Point2d
        , xDirection : Direction2d
        , yDirection : Direction2d
        }


{-| A `Frame3d` represents a coordinate system in 3D space. See the `Frame3d`
module for details.
-}
type Frame3d
    = Frame3d
        { originPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        , zDirection : Direction3d
        }


{-| A `SketchPlane3d` represents a 2D planar coordinate system in 3D space, and
allows operations such as projecting 3D geometry into a 2D coordinate system or
placing 2D geometry on a 3D plane. See the `SketchPlane3d` module for details.
-}
type SketchPlane3d
    = SketchPlane3d
        { originPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        }


{-| A line segment in 2D, defined by its start and end points. See the
`LineSegment2d` module for details.
-}
type LineSegment2d
    = LineSegment2d ( Point2d, Point2d )


{-| A line segment in 3D, defined by its start and end points. See the
`LineSegment3d` module for details.
-}
type LineSegment3d
    = LineSegment3d ( Point3d, Point3d )


{-| A triangle in 2D, defined by its three vertices. See the `Triangle2d` module
for details.
-}
type Triangle2d
    = Triangle2d ( Point2d, Point2d, Point2d )


{-| A triangle in 3D, defined by its three vertices. See the `Triangle3d` module
for details.
-}
type Triangle3d
    = Triangle3d ( Point3d, Point3d, Point3d )


{-| A bounding box in 2D, defined by its minimum and maximum X and Y values. See
the `BoundingBox2d` module for details.
-}
type BoundingBox2d
    = BoundingBox2d
        { minX : Float
        , maxX : Float
        , minY : Float
        , maxY : Float
        }


{-| A bounding box in 3D, defined by its minimum and maximum X, Y and Z values.
See the `BoundingBox3d` module for details.
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


{-| A polyline in 2D, defined by a list of vertices. See the `Polyline2d` module
for details.
-}
type Polyline2d
    = Polyline2d (List Point2d)


{-| A polyline in 3D, defined by a list of vertices. See the `Polyline3d` module
for details.
-}
type Polyline3d
    = Polyline3d (List Point3d)


{-| A polygon in 2D, defined by a list of vertices. Very similar to a
`Polyline2d` but the last point is implicitly considered to connect back to the
first point. See the `Polygon2d` module for details.
-}
type Polygon2d
    = Polygon2d (List Point2d)


{-| A circle in 2D, defined by its center point and radius. See the `Circle2d`
module for details.
-}
type Circle2d
    = Circle2d { centerPoint : Point2d, radius : Float }
