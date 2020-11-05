# elm-geometry

_Release notes for all versions of `elm-geometry` are available
[here](https://github.com/ianmackenzie/elm-geometry/releases)._

`elm-geometry` is an [Elm](http://elm-lang.org) package for working with 2D and
3D geometry. It provides a wide variety of geometric data types such as points,
vectors, arcs, spline curves and coordinate frames, along with functions for
transforming and combining them in many different ways. You can:

- Rotate points around axes in 3D
- Mirror triangles across 3D planes
- Project 3D geometry into 2D sketch planes
- Measure distances and angles between different objects
- Convert objects between different coordinate systems
- Compose complex 2D/3D transformations
- ...and much more!

## Table of contents

- [Overview](#overview)
- [Units and coordinate systems](#units-and-coordinate-systems)
  - [Units](#units)
  - [Coordinate systems](#coordinate-systems)
  - [Conversions](#conversions)
- [Installation](#installation)
- [Using the package](#using-the-package)
- [Documentation](#documentation)
- [Related packages](#related-packages)
- [Climate action](#climate-action)
- [Questions and feedback](#questions-and-feedback)

## Overview

`elm-geometry` includes a wide variety of data types: points, vectors, directions...

![Point2d](https://opensolid.github.io/images/geometry/icons/point2d.svg)
![Point3d](https://opensolid.github.io/images/geometry/icons/point3d.svg)
![Vector2d](https://opensolid.github.io/images/geometry/icons/vector2d.svg)
![Vector3d](https://opensolid.github.io/images/geometry/icons/vector3d.svg)
![Direction2d](https://opensolid.github.io/images/geometry/icons/direction2d.svg)
![Direction3d](https://opensolid.github.io/images/geometry/icons/direction3d.svg)

...line segments, triangles, bounding boxes...

![LineSegment2d](https://opensolid.github.io/images/geometry/icons/lineSegment2d.svg)
![LineSegment3d](https://opensolid.github.io/images/geometry/icons/lineSegment3d.svg)
![Triangle2d](https://opensolid.github.io/images/geometry/icons/triangle2d.svg)
![Triangle3d](https://opensolid.github.io/images/geometry/icons/triangle3d.svg)
![BoundingBox2d](https://opensolid.github.io/images/geometry/icons/boundingBox2d.svg)
![BoundingBox3d](https://opensolid.github.io/images/geometry/icons/boundingBox3d.svg)

...polylines, polygons, quadratic and cubic splines...

![Polyline2d](https://opensolid.github.io/images/geometry/icons/polyline2d.svg)
![Polyline3d](https://opensolid.github.io/images/geometry/icons/polyline3d.svg)
![Polygon2d](https://opensolid.github.io/images/geometry/icons/polygon2d.svg)
![QuadraticSpline2d](https://opensolid.github.io/images/geometry/icons/quadraticSpline2d.svg)
![QuadraticSpline3d](https://opensolid.github.io/images/geometry/icons/quadraticSpline3d.svg)
![CubicSpline2d](https://opensolid.github.io/images/geometry/icons/cubicSpline2d.svg)
![CubicSpline3d](https://opensolid.github.io/images/geometry/icons/cubicSpline3d.svg)

...circles, arcs, ellipses and elliptical arcs...

![Circle2d](https://opensolid.github.io/images/geometry/icons/circle2d.svg)
![Circle3d](https://opensolid.github.io/images/geometry/icons/circle3d.svg)
![Arc2d](https://opensolid.github.io/images/geometry/icons/arc2d.svg)
![Arc3d](https://opensolid.github.io/images/geometry/icons/arc3d.svg)
![Ellipse2d](https://opensolid.github.io/images/geometry/icons/ellipse2d.svg)
![EllipticalArc2d](https://opensolid.github.io/images/geometry/icons/ellipticalArc2d.svg)

...plus axes, planes, and various forms of 2D/3D coordinate systems:

![Axis2d](https://opensolid.github.io/images/geometry/icons/axis2d.svg)
![Axis3d](https://opensolid.github.io/images/geometry/icons/axis3d.svg)
![Plane3d](https://opensolid.github.io/images/geometry/icons/plane3d.svg)
![Frame2d](https://opensolid.github.io/images/geometry/icons/frame2d.svg)
![Frame3d](https://opensolid.github.io/images/geometry/icons/frame3d.svg)
![SketchPlane3d](https://opensolid.github.io/images/geometry/icons/sketchPlane3d.svg)

A large range of geometric functionality is included, such as various forms of
constructors...

```elm
Point3d.xyz
    (Length.meters 2)
    (Length.meters 4)
    (Length.meters 5)
-- OR --
Point3d.meters 2 4 5

Direction2d.fromAngle (Angle.degrees 30)
-- OR --
Direction2d.degrees 30

Point3d.midpoint p1 p2

Vector2d.withLength (Length.feet 3) Direction2d.y

Triangle2d.fromVertices ( p1, p2, p3 )
-- OR --
Triangle2d.from p1 p2 p3

Plane3d.throughPoints p1 p2 p3

Axis3d.through Point3d.origin Direction3d.z

Arc2d.from p1 p2 (Angle.degrees 90)

QuadraticSpline3d.fromControlPoints p1 p2 p3

CubicSpline2d.fromEndpoints
    startPoint
    startDerivative
    endPoint
    endDerivative
```

...point/vector arithmetic...

```elm
v1 |> Vector3d.plus v2

-- the vector from the point p1 to the point p2
Vector2d.from p1 p2

v1 |> Vector3d.cross v2

Vector2d.length vector

-- distance of a point from the origin
point |> Point2d.distanceFrom Point2d.origin
```

...and 2D/3D transformations:

```elm
vector |> Vector2d.rotateBy angle

point |> Point2d.rotateAround Point2d.origin angle

point |> Point3d.mirrorAcross Plane3d.xy

vector |> Vector3d.projectionIn Direction3d.z

triangle |> Triangle3d.rotateAround Axis3d.x angle

lineSegment
    |> LineSegment3d.mirrorAcross Plane3d.yz
    |> LineSegment3d.projectOnto Plane3d.xy

Plane3d.xy |> Plane3d.offsetBy (Length.meters 3)
```

## Units and coordinate systems

Most types in `elm-geometry` include two [phantom type parameters](https://blog.ilias.xyz/5-things-you-didnt-know-about-elm-7bdb67b1b3cd#ea40)
that allow compile-time tracking of both what units that geometry is in (usually
either meters for real-world geometry, or pixels for on-screen geometry) and
what coordinate system the geometry is defined in. For example, you might use a

```elm
Point2d Pixels YUpCoordinates
```

to represent a point on the screen that is defined in Y-up coordinates (from
the lower-left corner of an SVG drawing, for example) as opposed to Y-down
coordinates from the top left corner of the screen.

### Units

`elm-geometry` uses the `Quantity` type from [`elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest) to track/convert the units associated with
numeric values such as point coordinates, vector components, lengths, distances
and angles. Internally, `elm-units` converts everything to [SI](https://en.wikipedia.org/wiki/International_System_of_Units)
units, so

```elm
Point2d.inches 10 20
```

and

```elm
Point2d.centimeters 25.4 50.8
```

are equivalent. Tracking units at compile time prevents mixing and matching
different types of geometry; for example,

```elm
Point2d.xy (Length.meters 3) (Length.meters 4)
```

and

```elm
Point2d.xy (Pixels.pixels 200) (Pixels.pixels 300)
```

have completely different units, so the compiler can catch nonsensical
operations like trying to find the distance from the first point to the second.

### Coordinate systems

2D/3D geometry is often represented using X/Y/Z coordinates. As a result, in
addition to tracking which units are used, `elm-geometry` also lets you add type
annotations to specify what _coordinate system_ particular geometry is defined
in. For example, we might declare a `TopLeftCoordinates` type and then add a
type annotation to a `point` asserting that it is defined in coordinates
relative to the top-left corner of the screen:

```elm
{-| A coordinate system where (0, 0) is the top left corner
of the screen, positive X is to the right, and positive Y
is down.
-}
type TopLeftCoordinates =
    TopLeftCoordinates

point : Point2d Pixels TopLeftCoordinates
point =
    Point2d.pixels 200 300
```

Note that the `TopLeftCoordinates` type we declared gives us a convenient place
to document exactly how that coordinate system is defined. This combination now
gives us some nice type safety - the compiler will tell us if we try to mix two
points that have different units or are defined in different coordinate systems.

## Installation

Assuming you have [installed Elm](https://guide.elm-lang.org/install.html) and
started a new project, you'll want to run

```text
elm install ianmackenzie/elm-geometry
elm install ianmackenzie/elm-units
```

in a command prompt inside your project directory. Note that even though
`elm-units` is a dependency of `elm-geometry`, you'll still need to explicitly
install it so that you can import modules like [`Quantity`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity)
and [`Length`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length)
in your own code (which will be needed in basically any code that uses
`elm-geometry`.)

## Using the package

By itself, `elm-geometry` only performs abstract geometric operations like
measurements (distances, areas), checks (containment, intersection) and
transformations (scaling, rotation, translation, mirroring). See the [related
packages](#related-packages) section below for links to some packages that build
on top of `elm-geometry` to perform 2D drawing, 3D rendering, physics simulation
etc.

In general when using `elm-geometry`, you'll need to import a module for every
different data type that you want to work with; there is no "main" module. For
example, to calculate the distance between two 2D points, you would import the
`Point2d` module and write something like:

```elm
module Main exposing (main)

import Html exposing (Html)
import Length -- from elm-units, see 'Installation'
import Point2d

main : Html msg
main =
    let
        firstPoint =
            Point2d.meters 1 2

        secondPoint =
            Point2d.meters 3 4

        distanceInCentimeters =
            Point2d.distanceFrom firstPoint secondPoint
                |> Length.inCentimeters
    in
    Html.text <|
        "Distance: "
            ++ String.fromInt (round distanceInCentimeters)
            ++ " cm"
```

which should end up displaying "Distance: 283 cm".

Note that it was necessary to also import the [`Length`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length)
module from [`elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/),
since the [`Point2d.distanceFrom`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Point2d#distanceFrom)
function returns a [`Quantity Float units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity#Quantity),
not a plain `Float`. In general, in addition to `elm-geometry` modules, you'll
likely need to import either the `Length` or [`Pixels`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Pixels)
modules from `elm-units` (depending on whether you're working in real-world or
on-screen units) to work with any individual values returned by `elm-geometry`
functions (distances, areas, point coordinates, vector components, etc.).

## Documentation

[Full API documentation](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest)
is available for each module. Most modules are associated with a particular data
type (for example, the [`Point3d`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Point3d)
module contains functions for creating and manipulating `Point3d` values).

## Related packages

There are several other Elm packages related to `elm-geometry`:

- For drawing in 2D, check out [`elm-geometry-svg`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-svg/latest/)
- For 3D graphics, check out [`elm-3d-scene`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/)
  for a high-level approach or [`elm-geometry-linear-algebra-interop`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry-linear-algebra-interop/latest/)
  and [`elm-3d-camera`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest/)
  for working with WebGL directly
- For physics-based simulations/games, [`elm-physics`](https://package.elm-lang.org/packages/w0rm/elm-physics/latest/)
  is based on `elm-geometry` and provides a 3D physics engine including
  collisions, gravity, and constraints (joints)
- The [`elm-1d-parameter`](https://package.elm-lang.org/packages/ianmackenzie/elm-1d-parameter/latest/)
  package is both used internally by `elm-geometry`, and is useful to combine
  with functions like [`Point2d.interpolateFrom`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Point2d#interpolateFrom)
  to generate evenly-spaced values
- Functions like [`Polygon2d.triangulate`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Polygon2d#triangulate)
  return their results as a `TriangularMesh` value from [`elm-triangular-mesh`](https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest/)

I'm hopeful that in the future there will be packages that build on
`elm-geometry` to do non-graphical things like 3D printing or CNC machining!

## Climate action

I would like for the projects I work on to be as helpful as possible in
addressing the climate crisis. If

- you are working on a project that helps address the climate crisis (clean
  energy, public transit, reforestation, sustainable agriculture etc.) either as
  an individual, as part of an non-profit organization or even as part of a
  for-profit company, and
- there is a new feature you would find helpful for that work (or a bug you need
  fixed) in any of my open-source projects, then

please [open a new issue](https://github.com/ianmackenzie/elm-geometry/issues),
describe briefly what you're working on and I will treat that issue as high
priority.

## Questions and feedback

Please [open a new issue](https://github.com/ianmackenzie/elm-geometry/issues)
if you run into a bug, if any documentation is missing/incorrect/confusing, or
if there's a new feature that you would find useful. For general questions about
using `elm-geometry`, the best place is probably the **#geometry** channel on
the friendly [Elm Slack](http://elmlang.herokuapp.com/):

![Elm Slack #geometry channel conversation](https://ianmackenzie.github.io/elm-geometry/1.0.0/README/Slack.png)

You can also try:

- Sending me (**@ianmackenzie**) a message on Slack - even if you don't have any
  particular questions right now, it would be great to know what you're hoping
  to do with the package!
- Posting to the [Elm Discourse](https://discourse.elm-lang.org/) forums

You can also find me on Twitter ([@ianemackenzie](https://twitter.com/ianemackenzie)),
where I occasionally post `elm-geometry`-related stuff like demos or new
releases. Have fun, and don't be afraid to ask for help!
