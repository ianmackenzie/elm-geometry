# OpenSolid Core Library

OpenSolid consists of a set of Elm libraries for working with geometry. It is
intended to provide a solid foundation for HTML-based applications in areas such
as CAD (computer-aided design), CAM (computer-aided manufacturing), 2D/3D
visualization, and potentially games or numerical analysis (although OpenSolid
prioritizes flexibility and expressiveness over raw speed).

This library defines the OpenSolid core data types - vectors, directions,
points, axes, planes and frames (coordinate systems) in 2D and 3D:

```elm
myVector3d = Vector3d ( 1, 2, 3 )
myPoint2d = Point2d ( 4, 5 )
negativeXDirection3d = Direction3d ( -1, 0, 0 )

Vector3d.zero == Vector3d ( 0, 0, 0 )
Direction2d.y == Direction2d ( 0, 1 )
Point2d.origin == Point2d ( 0, 0 )
Axis3d.z == Axis3d Point3d.origin Direction3d.z
Frame2d.xy == Frame2d Point2d.origin Direction2d.x Direction2d.y
```

(Directions are effectively type-safe unit vectors.) A large range of geometric
operations are supported:

```elm
Vector3d.plus (Vector3d ( 1, 2, 3 )) (Vector3d ( 4, 5, 6 )) == Vector3d ( 5, 7, 9 )
Point3d.vectorFrom (Point3d ( 1, 1, 1 )) (Point3d ( 3, 5, 4 )) == Vector3d ( 2, 4, 3 )
Point2d.distanceFrom Point2d.origin (Point2d ( 1, 1 )) == sqrt 2
Direction2d.fromAngle (degrees 30) == Direction2d ( 0.866, 0.5 )
Point3d.mirrorAcross Plane3d.xy (Point3d ( 1, 2, 3 )) == Point3d ( 1, 2, -3 )
Point2d.rotateAround Point2d.origin (degrees 45) (Point2d ( 1, 0 )) == Point2d ( 0.7071, 0.7071 )
Vector2d.direction (Vector2d ( 3, 0 )) == Just Direction2d.x
```

## Philosophy

OpenSolid is similar in functionality to other vector/linear algebra libraries
but has a more geometric than mathematical focus.

### Transformations

OpenSolid does not use matrices to define transformations (in fact, matrices are
not used anywhere). Instead, transformations are simply Elm functions such as
`Point2d.rotateAround` shown above. This has many advantages. First, it means
that transformations can be directly used with higher-order functions like
`List.map`:

```elm
pointsOnXAxis =
    [ Point2d ( 1, 0 ), Point2d ( 2, 0 ), Point2d ( 3, 0 ) ]

rotateNinetyDegrees =
    Point2d.rotateAround Point2d.origin (degrees 90)

pointsOnYAxis =
    List.map rotateNinetyDegrees pointsOnXAxis

pointsOnYAxis == [ Point2d ( 0, 1 ), Point2d ( 0, 2 ), Point2d ( 0, 3 ) ]
```

Second, transformations can be composed like any other functions to produce
composite transformations (no more having to remember multiplication order of
matrices!):

```elm
rotateThenScale =
    Point2d.rotateAround Point2d.origin (degrees 90) >> Point2d.scaleAbout Point2d.origin 1.5

rotateThenScale (Point2d ( 1, 0 )) == Point2d ( 0, 1.5 )
rotateThenScale (Point2d ( 0, 2 )) == Point2d ( -3, 0 )
```

(Yes, in this particular case it doesn't actually matter whether you rotate
first and then scale or the other way around, but you get the idea.) You can
even compose transformation functions with other functions (even ones you define
yourself!) to produce composite 'transformations' that would be impossible to
represent with a transformation matrix:

```elm
horizontalDistance =
    Point3d.projectOnto Plane3d.xy >> Point3d.distanceFrom Point3d.origin

horizontalDistance (Point3d ( 3, 4, 2 )) == 5
horizontalDistance (Point3d ( 1, 1, 1 )) == sqrt 2
```

### Components

OpenSolid encourages thinking about points and vectors as geometric entities
instead of (x,y,z) values. For example, instead of using

```elm
Point3d.xCoordinate myPoint
```

you could use

```elm
Point3d.distanceAlong Axis3d.x myPoint
```

which has the advantage that it is easy to adapt to work with axes that might be
angled, not at the origin, or even changing dynamically. The key idea is to
think in terms of fundamental geometric concepts like "distance of a point along
an axis" since they are independent of the particular coordinate system being
used.

### Type safety

OpenSolid distinguishes between vectors, directions and points and only allows
operations that make sense. For example, vectors can be added together but
points cannot (however, a vector can be added to a point to produce a shifted
point).

OpenSolid uses the concept of a 'direction' where other libraries typically use
vectors with unit length. Having a separate type helps to keep track of whether
a vector has already been normalized (no more having to guess whether a function
that accepts a vector argument actually needs a unit vector, and if so whether
you're expected to normalize the vector yourself or whether the function will do
that internally).

You can normalize a vector to produce a direction with the `Vector2d.direction`
and `Vector3d.direction` functions, but they actually return `Maybe` values
since a zero vector has no direction - passing `Vector3d.zero` to
`Vector3d.direction` will result in `None`. This takes advantage of Elm's type
system to ensure that all code considers the degenerate zero-vector case, which
is easy to run into when (for example) trying to compute the normal direction to
a degenerate triangle in 3D.
