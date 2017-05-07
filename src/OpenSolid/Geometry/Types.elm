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
        , Circle3d(Circle3d)
        , Arc2d(Arc2d)
        , Arc3d(Arc3d)
        , QuadraticSpline2d(QuadraticSpline2d)
        , QuadraticSpline3d(QuadraticSpline3d)
        , CubicSpline2d(CubicSpline2d)
        , CubicSpline3d(CubicSpline3d)
        )

{-| This module contains the definitions of the core OpenSolid data types. Each
type also has a corresponding module providing related functionality. Suggested
practice is to import this module exposing everything, and import all other
necessary modules using `as`, for example:

    import OpenSolid.Geometry.Types exposing (..)
    import OpenSolid.Vector2d as Vector2d
    import OpenSolid.Plane3d as Plane3d


# Vectors

Vectors represent quantities such as displacements or velocities. For positions,
`Point2d` or `Point3d` should be used instead.

@docs Vector2d, Vector3d


# Directions

A direction is effectively a vector with a length of one, used to represent
quantities like the direction of an axis or the normal direction of a plane.

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

@docs Circle2d, Circle3d


# Arcs

@docs Arc2d, Arc3d


# Splines

@docs QuadraticSpline2d, QuadraticSpline3d, CubicSpline2d, CubicSpline3d

-}


{-| ![Vector2d](https://opensolid.github.io/images/geometry/icons/vector2d.svg)

A vector in 2D, defined by its X and Y components. See the [`Vector2d`](OpenSolid-Vector2d)
module for details.

-}
type Vector2d
    = Vector2d ( Float, Float )


{-| ![Vector3d](https://opensolid.github.io/images/geometry/icons/vector3d.svg)

A vector in 3D, defined by its X, Y and Z components. See the [`Vector3d`](OpenSolid-Vector3d)
module for details.

-}
type Vector3d
    = Vector3d ( Float, Float, Float )


{-| ![Direction2d](https://opensolid.github.io/images/geometry/icons/direction2d.svg)

A direction in 2D, defined by its X and Y components. See the [`Direction2d`](OpenSolid-Direction2d)
module for details.

-}
type Direction2d
    = Direction2d ( Float, Float )


{-| ![Direction3d](https://opensolid.github.io/images/geometry/icons/direction3d.svg)

A direction in 3D, defined by its X, Y and Z components. See the [`Direction3d`](OpenSolid-Direction3d)
module for details.

-}
type Direction3d
    = Direction3d ( Float, Float, Float )


{-| ![Point2d](https://opensolid.github.io/images/geometry/icons/point2d.svg)

A point in 2D, defined by its X and Y coordinates. See the [`Point2d`](OpenSolid-Point2d)
module for details.

-}
type Point2d
    = Point2d ( Float, Float )


{-| ![Point3d](https://opensolid.github.io/images/geometry/icons/point3d.svg)

A point in 3D, defined by its X, Y and Z coordinates. See the [`Point3d`](OpenSolid-Point3d)
module for details.

-}
type Point3d
    = Point3d ( Float, Float, Float )


{-| ![Axis2d](https://opensolid.github.io/images/geometry/icons/axis2d.svg)

An axis in 2D, defined by its origin point and direction. See the [`Axis2d`](OpenSolid-Axis2d)
module for details.

-}
type Axis2d
    = Axis2d { originPoint : Point2d, direction : Direction2d }


{-| ![Axis3d](https://opensolid.github.io/images/geometry/icons/axis3d.svg)

An axis in 3D, defined by its origin point and direction. See the [`Axis3d`](OpenSolid-Axis3d)
module for details.

-}
type Axis3d
    = Axis3d { originPoint : Point3d, direction : Direction3d }


{-| ![Plane3d](https://opensolid.github.io/images/geometry/icons/plane3d.svg)

A plane in 3D, defined by its origin point and normal direction. See the
[`Plane3d`](OpenSolid-Plane3d) module for details.

-}
type Plane3d
    = Plane3d { originPoint : Point3d, normalDirection : Direction3d }


{-| ![Frame2d](https://opensolid.github.io/images/geometry/icons/frame2d.svg)

A coordinate system in 2D space, defined by its origin point and X and Y
directions. See the [`Frame2d`](OpenSolid-Frame2d) module for details.

-}
type Frame2d
    = Frame2d
        { originPoint : Point2d
        , xDirection : Direction2d
        , yDirection : Direction2d
        }


{-| ![Frame3d](https://opensolid.github.io/images/geometry/icons/frame3d.svg)

A coordinate system in 3D space, defined by its origin point and X, Y and Z
directions. See the [`Frame3d`](OpenSolid-Frame3d) module for details.

-}
type Frame3d
    = Frame3d
        { originPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        , zDirection : Direction3d
        }


{-| ![SketchPlane3d](https://opensolid.github.io/images/geometry/icons/sketchPlane3d.svg)

A 2D planar coordinate system embedded in 3D space, defined by its origin point
and X and Y directions. Used for operations such as projecting 3D geometry into
a 2D coordinate system or placing 2D geometry in 3D. See the [`SketchPlane3d`](OpenSolid-SketchPlane3d)
module for details.

-}
type SketchPlane3d
    = SketchPlane3d
        { originPoint : Point3d
        , xDirection : Direction3d
        , yDirection : Direction3d
        }


{-| ![LineSegment2d](https://opensolid.github.io/images/geometry/icons/lineSegment2d.svg)

A line segment in 2D, defined by its start and end points. See the
[`LineSegment2d`](OpenSolid-LineSegment2d) module for details.

-}
type LineSegment2d
    = LineSegment2d ( Point2d, Point2d )


{-| ![LineSegment3d](https://opensolid.github.io/images/geometry/icons/lineSegment3d.svg)

A line segment in 3D, defined by its start and end points. See the
[`LineSegment3d`](OpenSolid-LineSegment3d) module for details.

-}
type LineSegment3d
    = LineSegment3d ( Point3d, Point3d )


{-| ![Triangle2d](https://opensolid.github.io/images/geometry/icons/triangle2d.svg)

A triangle in 2D, defined by its three vertices. See the [`Triangle2d`](OpenSolid-Triangle2d)
module for details.

-}
type Triangle2d
    = Triangle2d ( Point2d, Point2d, Point2d )


{-| ![Triangle3d](https://opensolid.github.io/images/geometry/icons/triangle3d.svg)

A triangle in 3D, defined by its three vertices. See the [`Triangle3d`](OpenSolid-Triangle3d)
module for details.

-}
type Triangle3d
    = Triangle3d ( Point3d, Point3d, Point3d )


{-| ![BoundingBox2d](https://opensolid.github.io/images/geometry/icons/boundingBox2d.svg)

A bounding box in 2D, defined by its minimum and maximum X and Y values. See the
[`BoundingBox2d`](OpenSolid-BoundingBox2d) module for details.

-}
type BoundingBox2d
    = BoundingBox2d
        { minX : Float
        , maxX : Float
        , minY : Float
        , maxY : Float
        }


{-| ![BoundingBox3d](https://opensolid.github.io/images/geometry/icons/boundingBox3d.svg)

A bounding box in 3D, defined by its minimum and maximum X, Y and Z values. See
the [`BoundingBox3d`](OpenSolid-BoundingBox3d) module for details.

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


{-| ![Polyline2d](https://opensolid.github.io/images/geometry/icons/polyline2d.svg)

A polyline in 2D, defined by a list of vertices. See the [`Polyline2d`](OpenSolid-Polyline2d)
module for details.

-}
type Polyline2d
    = Polyline2d (List Point2d)


{-| ![Polyline3d](https://opensolid.github.io/images/geometry/icons/polyline3d.svg)

A polyline in 3D, defined by a list of vertices. See the [`Polyline3d`](OpenSolid-Polyline3d)
module for details.

-}
type Polyline3d
    = Polyline3d (List Point3d)


{-| ![Polygon2d](https://opensolid.github.io/images/geometry/icons/polygon2d.svg)

A polygon in 2D, defined by a list of vertices. Very similar to a `Polyline2d`
but the last point is implicitly considered to connect back to the first point.
See the [`Polygon2d`](OpenSolid-Polygon2d) module for details.

-}
type Polygon2d
    = Polygon2d (List Point2d)


{-| ![Circle2d](https://opensolid.github.io/images/geometry/icons/circle2d.svg)

A circle in 2D, defined by its center point and radius. See the [`Circle2d`](OpenSolid-Circle2d)
module for details.

-}
type Circle2d
    = Circle2d { centerPoint : Point2d, radius : Float }


{-| ![Circle3d](https://opensolid.github.io/images/geometry/icons/circle3d.svg)

A circle in 3D, defined by its center point, axial direction and radius. See the
[`Circle3d`](OpenSolid-Circle3d) module for details.

-}
type Circle3d
    = Circle3d
        { centerPoint : Point3d
        , axialDirection : Direction3d
        , radius : Float
        }


{-| ![Arc2d](https://opensolid.github.io/images/geometry/icons/arc2d.svg)

An arc in 2D, defined by its center point, start point and swept angle. See the
[`Arc2d`](OpenSolid-Arc2d) module for details.

-}
type Arc2d
    = Arc2d { centerPoint : Point2d, startPoint : Point2d, sweptAngle : Float }


{-| ![Arc3d](https://opensolid.github.io/images/geometry/icons/arc3d.svg)

An arc in 3D, defined by its axis, start point and swept angle. See the
[`Arc3d`](OpenSolid-Arc3d) module for details.

-}
type Arc3d
    = Arc3d { axis : Axis3d, startPoint : Point3d, sweptAngle : Float }


{-| ![QuadraticSpline2d](https://opensolid.github.io/images/geometry/icons/quadraticSpline2d.svg)

A quadratic Bezier spline in 2D, defined by its three control points. See the
[`QuadraticSpline2d`](OpenSolid-QuadraticSpline2d) module for details.

-}
type QuadraticSpline2d
    = QuadraticSpline2d ( Point2d, Point2d, Point2d )


{-| ![QuadraticSpline3d](https://opensolid.github.io/images/geometry/icons/quadraticSpline3d.svg)

A quadratic Bezier spline in 3D, defined by its three control points. See the
[`QuadraticSpline3d`](OpenSolid-QuadraticSpline3d) module for details.

-}
type QuadraticSpline3d
    = QuadraticSpline3d ( Point3d, Point3d, Point3d )


{-| ![CubicSpline2d](https://opensolid.github.io/images/geometry/icons/cubicSpline2d.svg)

A cubic Bezier spline in 2D, defined by its four control points. See the
[`CubicSpline2d`](OpenSolid-CubicSpline2d) module for details.

-}
type CubicSpline2d
    = CubicSpline2d ( Point2d, Point2d, Point2d, Point2d )


{-| ![CubicSpline3d](https://opensolid.github.io/images/geometry/icons/cubicSpline3d.svg)

A cubic Bezier spline in 3D, defined by its four control points. See the
[`CubicSpline3d`](OpenSolid-CubicSpline3d) module for details.

-}
type CubicSpline3d
    = CubicSpline3d ( Point3d, Point3d, Point3d, Point3d )
