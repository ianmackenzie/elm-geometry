## What is it?

OpenSolid consists of a set of Elm packages for working with geometry. It is
intended to provide a solid foundation for HTML-based applications in areas such
as CAD (computer-aided design), CAM (computer-aided manufacturing), and 2D/3D
visualization.

This package contains modules for creating and working with the core OpenSolid
data types:

  - `Point2d`, `Point3d`
  - `Vector2d`, `Vector3d`
  - `Direction2d`, `Direction3d` (type-safe unit vectors)
  - `Axis2d`, `Axis3d`, `Plane3d` (useful for rotations, mirrors, projections)
  - `Frame2d`, `Frame3d` (local coordinate systems)
  - `SketchPlane3d` (local 2D coordinate system embedded in 3D)
  - `BoundingBox2d`, `BoundingBox3d`
  - `LineSegment2d`, `LineSegment3d`
  - `Triangle2d`, `Triangle3d`
  - `Polyline2d`, `Polyline3d`
  - `Polygon2d`

A large range of geometric operations are supported:

```elm
-- Constructors

Direction2d.fromAngle (degrees 30) ==
    Direction2d ( 0.866, 0.5 )

Point3d.midpoint Point3d.origin (Point3d (1, 4, 5)) ==
    Vector2d ( 0.5, 2, 2.5 )

-- Arithmetic

Vector3d.plus (Vector3d ( 1, 2, 3 )) (Vector3d ( 4, 5, 6 )) ==
    Vector3d ( 5, 7, 9 )

Point3d.vectorFrom (Point3d ( 1, 1, 1 )) (Point3d ( 3, 5, 4 )) ==
    Vector3d ( 2, 4, 3 )

Point2d.distanceFrom Point2d.origin (Point2d ( 1, 1 )) == 1.4142

-- Transformations

Point3d.mirrorAcross Plane3d.xy (Point3d ( 1, 2, 3 )) ==
    Point3d ( 1, 2, -3 )

Vector2d.rotateBy (degrees 45) (Vector2d ( 1, 1 )) ==
    Vector2d ( 0, 1.4142 )

Point2d.rotateAround Point2d.origin (degrees 45) (Point2d ( 1, 0 )) ==
    Point2d ( 0.7071, 0.7071 )

Plane3d.translateBy (Vector3d ( 0, 0, 3 )) Plane3d.xy ==
    Plane3d.offsetBy 3 Plane3d.xy

Point3d.projectOnto Plane3d.xy (Point3d ( 2, 1, 3 )) ==
    Point3d ( 2, 1, 0 )

Vector3d.projectionIn Direction3d.z (Vector3d ( 3, 1, 4 )) ==
    Vector3d ( 0, 0, 4 )

-- Coordinate transformations

rotatedFrame =
    Frame2d.xy |> Frame2d.rotateBy (degrees 30)

Vector2d.relativeTo rotatedFrame (Vector2d ( 2, 0 )) ==
    Vector2d ( 1.7321, -1 )

Point3d.projectInto SketchPlane3d.yz (Point3d ( 2, 1, 3 )) ==
    Point2d ( 1, 3 )

-- Bounding boxes

Point2d.hull (Point2d ( 1, 4 )) (Point2d ( 2, 3 )) ==
    BoundingBox2d
        { minX = 1
        , maxX = 2
        , minY = 3
        , maxY = 4
        }
```

JSON encoders and decoders for all types are also provided in the `Encode` and
`Decode` modules.

## How do I use it?

To install, run

```
elm package install opensolid/geometry
```

or add

```json
"opensolid/geometry": "1.0.0 <= v < 2.0.0"
```

to your `elm-package.json`.

Most OpenSolid modules are designed to imported as qualified, for example

```elm
import OpenSolid.Point3d as Point3d
```

The main exception is the `Types` module, which only contains type definitions
and is intended to be imported without qualification:

```elm
import OpenSolid.Geometry.Types exposing (..)
```

For technical details, check out the documentation for each module.
