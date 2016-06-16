# OpenSolid Core Library

This library defines the core OpenSolid data types: vectors, directions, points,
axes, planes and coordinate frames in both 2D and 3D:

```elm
myVector = Vector3d 1 2 3
myPoint = Point2d 4 5
negativeXDirection = Direction3d (Vector3d -1 0 0)

Vector3d.zero == Vector3d 0 0 0
Direction2d.y == Direction2d (Vector2d 0 1)
Point2d.origin == Point2d 0 0
Axis3d.z == Axis3d Point3d.origin Direction3d.z
Frame2d.xy == Frame2d Point2d.origin Direction2d.x Direction2d.y

A large range of geometric operations are supported:

```elm
Vector3d.plus (Vector3d 1 2 3) (Vector3d 4 5 6) == Vector3d 5 7 9
Point3d.vectorFrom (Point3d 1 1 1) (Point3d 3 5 4) == Vector3d 2 4 3
Direction2d.fromAngle (degrees 30) == Direction2d (Vector2d 0.866 0.5)
Point2d.rotateAround Point2d.origin (degrees 45) (Point2d 1 0) == Point2d 0.7071 0.7071
Point3d.mirrorAcross Plane3d.xy (Point3d 1 2 3) == Point3d 1 2 -3
Vector2d.direction (Vector2d 3 0) == Just Direction2d.x
```

## Philosophy

OpenSolid is similar in functionality to other vector/linear algebra libraries
but has a more geometric than mathematical focus. For example, OpenSolid does
not use matrices to define transformations (in fact, matrices are not used
anywhere). Instead, transformations are simply Elm functions such as
`Point2d.rotateAround` shown above. This has many advantages. First, it means
that transformations can directly used with functions like `List.map`:

```elm
points =
    [ Point2d 1 0, Point2d 2 0, Point2d 3 0]

rotateNinetyDegrees =
    Point2d.rotateAround Point2d.origin (degrees 90)

rotatedPoints =
    List.map rotateNinetyDegrees points

rotatedPoints == [ Point2d 0 1, Point2d 0 2, Point2d 0 3 ]
```

Second, transformations can be composed like any other functions to produce
composite transformations (no more having to remember multiplication order of
matrices):

```elm
rotateThenScale =
    Point2d.rotateAround Point2d.origin (degrees 90) >> Point2d.scaleAbout Point2d.origin 3

rotateThenScale (Point2d 1 0) == Point2d 0 3
rotateThenScale (Point2d 0 2) == Point2d -6 0
```

OpenSolid also encourages thinking about points and vectors as geometric
entities instead of (x,y,z) values. For example, instead of using

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
an axis" since (unlike "X component of a point") that is independent of the
particular coordinate system being used to define the point and axis.
