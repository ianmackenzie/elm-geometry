## What is it?

OpenSolid consists of a set of Elm libraries for working with geometry. It is
intended to provide a solid foundation for HTML-based applications in areas such
as CAD (computer-aided design), CAM (computer-aided manufacturing), and 2D/3D
visualization.

This library contains the OpenSolid core data types and operations - vectors,
directions (type-safe unit vectors), points, axes, planes and frames (coordinate
systems) in 2D and 3D:

```elm
Vector3d.zero == Vector3d ( 0, 0, 0 )
Direction2d.y == Direction2d ( 0, 1 )
Point2d.origin == Point2d ( 0, 0 )

Axis3d.z ==
    Axis3d { originPoint = Point3d.origin, direction = Direction3d.z }

Frame2d.xy ==
    Frame2d
        { originPoint = Point2d.origin
        , xDirection = Direction2d.x
        , yDirection = Direction2d.y
        }

Plane3d.yz ==
    Plane3d
        { originPoint = Point3d.origin
        , xDirection = Direction3d.y
        , yDirection = Direction3d.z
        , normalDirection = Direction3d.x
        }
```

A large range of geometric operations are supported:

```elm
-- Constructors
Direction2d.fromAngle (degrees 30) == Direction2d ( 0.866, 0.5 )
Vector2d.inDirection Direction2d.y 2.5 == Vector2d ( 0, 2.5 )
Plane3d.fromPointAndNormal Point3d.origin Direction3d.y == Plane3d.zx

-- Arithmetic
Vector3d.plus (Vector3d ( 1, 2, 3 )) (Vector3d ( 4, 5, 6 )) == Vector3d ( 5, 7, 9 )
Point3d.vectorFrom (Point3d ( 1, 1, 1 )) (Point3d ( 3, 5, 4 )) == Vector3d ( 2, 4, 3 )
Point2d.distanceFrom Point2d.origin (Point2d ( 1, 1 )) == 1.4142

-- Transformations
Point3d.mirrorAcross Plane3d.xy (Point3d ( 1, 2, 3 )) == Point3d ( 1, 2, -3 )
Vector2d.rotateBy (degrees 45) (Vector2d ( 1, 1 )) == Vector2d ( 0, 1.4142 )
Point2d.rotateAround Point2d.origin (degrees 45) (Point2d ( 1, 0 )) == Point2d ( 0.7071, 0.7071 )
Plane3d.translateIn Direction3d.z 3 Plane3d.xy == Plane3d.offsetBy 3 Plane3d.xy
Point3d.projectOnto Plane3d.xy (Point3d ( 2, 1, 3 )) == Point3d ( 2, 1, 0 )
Vector3d.projectionIn Direction3d.z (Vector3d ( 3, 1, 4 )) == Vector3d ( 0, 0, 4 )
```

## How do I use it?

Short answer: run `elm package install ianmackenzie/elm-opensolid-core`, then
look through the documentation for the modules that you're interested in.
Everything you need to know you should be able to get by reading the
documentation, but if you're interested in some of the guiding principles and
rationale behind OpenSolid, check out the [usage notes](USAGE.md).
