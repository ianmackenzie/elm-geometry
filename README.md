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

JSON encoders and decoders for all types are also provided in the `Encode` and
`Decode` modules.

## How do I use it?

To install, run

```
elm package install ianmackenzie/elm-opensolid-core
```

or add

```json
"ianmackenzie/elm-opensolid-core": "1.0.0 <= v < 2.0.0"
```

to your `elm-package.json`.

Most OpenSolid modules are designed to imported as qualified, for example

```elm
import OpenSolid.Core.Point3d as Point3d
```

The main exception is the `Types` module, which only contains type definitions
and is intended to be imported without qualification:

```elm
import OpenSolid.Core.Types exposing (..)
```

For technical details, check out the documentation for each module, but if
you're interested in some of the guiding principles and rationale behind
OpenSolid, read on!

## What makes it different?

OpenSolid is functionally similar to other vector/geometry libraries, but works
differently in a few subtle but meaningful ways. In general, OpenSolid has a
more geometric than mathematical focus. For example, distinct types are used for
points, vectors and directions which many other libraries treat as a single
generic vector type.

### Coordinates and components

Explicitly working with individual X/Y/Z point coordinates and vector components
is easy to do in OpenSolid when necessary, but in many cases it is more flexible
and meaningful to consider points and vectors as abstract geometric quantities
and treat their internal representations as an implementation detail. For
example, instead of using

```elm
forwardSpeed = Vector3d.xComponent velocityVector
height = Point3d.zCoordinate position
```

you could use

```elm
forwardSpeed = Vector3d.componentIn forwardDirection velocityVector
height = Point3d.signedDistanceFrom groundPlane position
```

where (in a `Constants` module or similar) you would define

```elm
forwardDirection = Direction3d.x
groundPlane = Plane3d.xy
```

This is slightly more verbose, but very explicit, and adapts more easily to
changes (perhaps the forward direction becomes a configuration setting, or the
ground plane becomes dynamic and shifts or tilts over time).

### Directions

OpenSolid uses the concept of a 'direction' where other libraries typically use
vectors with unit length. Having separate types helps to keep track of whether a
vector has already been normalized - no more having to guess whether a function
that accepts a vector argument actually needs a unit vector, and if so whether
you're expected to normalize the vector yourself or whether the function will do
that internally.

You can normalize a vector to produce a direction with the `Vector2d.direction`
and `Vector3d.direction` functions, but they actually return `Maybe` values
since the zero vector has no direction:

```elm
Vector2d.direction (Vector2d ( 3, 0 )) == Just (Direction2d ( 1, 0 ))
Vector2d.direction (Vector2d ( -2, 2 )) == Just (Direction2d ( -0.7071, 0.7071 ))
Vector2d.direction (Vector2d ( 0, 0 )) == Nothing
```

This takes advantage of Elm's type system to ensure that all code considers the
degenerate zero-vector case. For example, given an eye point and a point to look
at, the corresponding view direction could be determined with

```elm
Vector3d.direction (Point3d.vectorFrom eyePoint lookAtPoint)
```

This would return a `Maybe Direction3d`, with `Nothing` corresponding to the
case where the eye point and point to look at are coincident (in which case the
view direction is not well-defined and some special-case logic is needed).

### Transformations

Many of the functions in this library deal with different kinds of
transformations - translations, rotations, scalings, mirrors, and projections.
Unlike most other geometric libraries, however, OpenSolid does not use matrices
to define transformations (in fact, matrices are not used anywhere). Instead of
having functions to create transformation matrices which can then later be
applied to values, transformations in OpenSolid are just Elm functions that can
be used directly:

```elm
rotatedPoint = Point2d.rotateAround Point2d.origin (degrees 45) originalPoint
```

This has many advantages. First, partial function application means that
transformations can be directly used with higher-order functions like
`List.map`:

```elm
pointsOnXAxis =
    [ Point2d ( 1, 0 ), Point2d ( 2, 0 ), Point2d ( 3, 0 ) ]

rotateNinetyDegrees : Point2d -> Point2d
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
rotateThenScale : Point2d -> Point2d
rotateThenScale =
    Point2d.rotateAround Point2d.origin (degrees 90) >> Point2d.scaleAbout Point2d.origin 1.5

rotateThenScale (Point2d ( 1, 0 )) == Point2d ( 0, 1.5 )
rotateThenScale (Point2d ( 0, 2 )) == Point2d ( -3, 0 )
```

(Yes, in this particular case it doesn't actually matter whether you rotate
first and then scale or the other way around, but you get the idea.)

### Function suffixes

Many OpenSolid functions (including most transformations, as seen above) end
with a preposition like "to", "from", "around", "onto", "along", "plus", or
"minus". In these cases, where the function name "leaves you hanging", the
general rule is that the last word in the function name applies to the first
function argument. A few examples:

  - `Point3d.rotateAround` takes the axis to rotate around as the first
    argument. (If the angle to rotate by was the first argument, the
    function would have been named `rotateBy` instead.)
  - `Vector3d.minus a b` means `b - a` - think of it as `minus a` applied to
    `b`. This is often handy when used with partial function application.
    Compare to `Vector3d.crossProduct` which takes its arguments in 'normal'
    order since 'product' is not a preposition - this function was in fact
    originally named `Vector3d.cross` and took its arguments in 'reverse' order,
    but this was found to be error-prone and not very useful.
  - `Point3d.vectorTo` and `Point3d.vectorFrom` are flipped versions of each
    other. The first computes the vector to the first point from the second
    point, and the second computes the vector from the first point to the second
    point.

In many cases, these functions are designed to work well when leaving off the
last argument (partial function application) and then chaining the
partially-applied functions together:

```elm
horizontalDistanceFromOrigin =
    Point3d.projectOnto Plane3d.xy >> Point3d.distanceFrom Point3d.origin

horizontalDistanceFromOrigin (Point3d ( 3, 4, 2 )) == 5
horizontalDistanceFromOrigin (Point3d ( 1, 1, 1 )) == sqrt 2
```

Passing partially-applied functions to higher-order functions like `List.map`
also works well:

```elm
offset =
    Vector2d ( 2, 0 )

vectors =
    [ Vector2d ( 3, 4 ), Vector2d ( 5, 6 ) ]

List.map (Vector2d.minus offset) vectors == [ Vector2d ( 1, 4 ), Vector2d ( 3, 6 ) ]
```
