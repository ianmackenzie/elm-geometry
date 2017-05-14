# opensolid/geometry [![Travis build Status](https://travis-ci.org/opensolid/geometry.svg?branch=master)](https://travis-ci.org/opensolid/geometry)

`opensolid/geometry` is an [Elm](http://elm-lang.org) package for working with
2D and 3D geometry. It provides a wide variety of geometric data types such as
points, vectors, polygons and coordinate frames, along with functions for
transforming and combining them in many different ways. You can:

  - Rotate points around axes in 3D
  - Mirror triangles across 3D planes
  - Project 3D geometry into 2D sketch planes
  - Measure distances and angles between different objects
  - Convert objects between different coordinate systems
  - Compose complex 2D/3D transformations
  - ...and much more!

A wide variety of data types are supported:

  - `Point2d`, `Point3d`, `Vector2d`, `Vector3d`, `Direction2d`, `Direction3d`

    ![Point2d](https://opensolid.github.io/images/geometry/icons/point2d.svg)
    ![Point3d](https://opensolid.github.io/images/geometry/icons/point3d.svg)
    ![Vector2d](https://opensolid.github.io/images/geometry/icons/vector2d.svg)
    ![Vector3d](https://opensolid.github.io/images/geometry/icons/vector3d.svg)
    ![Direction2d](https://opensolid.github.io/images/geometry/icons/direction2d.svg)
    ![Direction3d](https://opensolid.github.io/images/geometry/icons/direction3d.svg)

  - `Axis2d`, `Axis3d`, `Plane3d`

    ![Axis2d](https://opensolid.github.io/images/geometry/icons/axis2d.svg)
    ![Axis3d](https://opensolid.github.io/images/geometry/icons/axis3d.svg)
    ![Plane3d](https://opensolid.github.io/images/geometry/icons/plane3d.svg)

  - `Frame2d`, `Frame3d`, `SketchPlane3d`

    ![Frame2d](https://opensolid.github.io/images/geometry/icons/frame2d.svg)
    ![Frame3d](https://opensolid.github.io/images/geometry/icons/frame3d.svg)
    ![SketchPlane3d](https://opensolid.github.io/images/geometry/icons/sketchPlane3d.svg)

  - `BoundingBox2d`, `BoundingBox3d`

    ![BoundingBox2d](https://opensolid.github.io/images/geometry/icons/boundingBox2d.svg)
    ![BoundingBox3d](https://opensolid.github.io/images/geometry/icons/boundingBox3d.svg)

  - `LineSegment2d`, `LineSegment3d`, `Triangle2d`, `Triangle3d`

    ![LineSegment2d](https://opensolid.github.io/images/geometry/icons/lineSegment2d.svg)
    ![LineSegment3d](https://opensolid.github.io/images/geometry/icons/lineSegment3d.svg)
    ![Triangle2d](https://opensolid.github.io/images/geometry/icons/triangle2d.svg)
    ![Triangle3d](https://opensolid.github.io/images/geometry/icons/triangle3d.svg)

  - `Polyline2d`, `Polyline3d`, `Polygon2d`

    ![Polyline2d](https://opensolid.github.io/images/geometry/icons/polyline2d.svg)
    ![Polyline3d](https://opensolid.github.io/images/geometry/icons/polyline3d.svg)
    ![Polygon2d](https://opensolid.github.io/images/geometry/icons/polygon2d.svg)

  - `Circle2d`, `Circle3d`, `Arc2d`, `Arc3d`

    ![Circle2d](https://opensolid.github.io/images/geometry/icons/circle2d.svg)
    ![Circle3d](https://opensolid.github.io/images/geometry/icons/circle3d.svg)
    ![Arc2d](https://opensolid.github.io/images/geometry/icons/arc2d.svg)
    ![Arc3d](https://opensolid.github.io/images/geometry/icons/arc3d.svg)

  - `QuadraticSpline2d`, `QuadraticSpline3d`, `CubicSpline2d`, `CubicSpline3d`

    ![QuadraticSpline2d](https://opensolid.github.io/images/geometry/icons/quadraticSpline2d.svg)
    ![QuadraticSpline3d](https://opensolid.github.io/images/geometry/icons/quadraticSpline3d.svg)
    ![CubicSpline2d](https://opensolid.github.io/images/geometry/icons/cubicSpline2d.svg)
    ![CubicSpline3d](https://opensolid.github.io/images/geometry/icons/cubicSpline3d.svg)

A large range of geometric functionality is included, such as various forms of
constructors...

```elm
Direction2d.fromAngle (degrees 30)
--> Direction2d ( 0.866, 0.5 )

Point3d.midpoint Point3d.origin (Point3d ( 1, 4, 5 ))
--> Point3d ( 0.5, 2, 2.5 )

Frame2d.at (Point2d ( 2, 3 ))
--> Frame2d
-->     { originPoint = Point2d ( 2, 3 )
-->     , xDirection = Direction2d ( 1, 0 )
-->     , yDirection = Direction2d ( 0, 1 )
-->     }
```

...point/vector arithmetic...

```elm
Vector3d.sum (Vector3d ( 1, 2, 3 )) (Vector3d ( 4, 5, 6 ))
--> Vector3d ( 5, 7, 9 )

Point3d.vectorFrom (Point3d ( 1, 1, 1 )) (Point3d ( 3, 5, 4 ))
--> Vector3d ( 2, 4, 3 )

Point2d.distanceFrom Point2d.origin (Point2d ( 1, 1 ))
--> 1.4142
```

...2D/3D transformations...

```elm
Point3d.mirrorAcross Plane3d.xy (Point3d ( 1, 2, 3 ))
--> Point3d ( 1, 2, -3 )

Vector2d.rotateBy (degrees 45) (Vector2d ( 1, 1 ))
--> Vector2d ( 0, 1.4142 )

Point2d.rotateAround Point2d.origin (degrees 45) (Point2d ( 1, 0 ))
--> Point2d ( 0.7071, 0.7071 )

Vector3d.projectionIn Direction3d.z (Vector3d ( 3, 1, 4 ))
--> Vector3d ( 0, 0, 4 )

rotatedTriangle =
    Triangle3d.rotateAround Axis3d.x (degrees 45) triangle

transformedLineSegment =
    lineSegment
        |> LineSegment3d.mirrorAcross Plane3d.yz
        |> LineSegment3d.projectOnto Plane3d.xy

raisedPlane =
    Plane3d.offsetBy 3 Plane3d.xy
```

...and conversions between coordinate systems:

```elm
rotatedFrame =
    Frame2d.xy |> Frame2d.rotateBy (degrees 30)

Vector2d.relativeTo rotatedFrame (Vector2d ( 2, 0 ))
--> Vector2d ( 1.7321, -1 )

Point2d.placeIn rotatedFrame (Point2d (1, 0))
--> Point2d ( 0.866, 0.5 )

Point3d.projectInto SketchPlane3d.yz (Point3d ( 2, 1, 3 ))
--> Point2d ( 1, 3 )

Point2d.placeOnto SketchPlane3d.xz (Point2d ( 4, 5 ))
--> Point3d ( 4, 0, 5 )
```

Support is also provided for encoding/decoding values to and from JSON.

## Installation

Assuming you have [installed Elm](https://guide.elm-lang.org/install.html) and
started a new project, use [elm-package](https://guide.elm-lang.org/install.html#elm-package)
to install `opensolid/geometry`, either by running

```
elm package install opensolid/geometry
```

in a command prompt inside your project directory or by adding

```json
"opensolid/geometry": "1.0.0 <= v < 2.0.0"
```

to the `dependencies` field in your project's `elm-package.json`.

## Documentation

[Full API documentation](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0)
is available for each module. Most modules are associated with a particular data
type (for example, the [`Point3d`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point3d)
module contains functions related to the [`Point3d`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Geometry-Types#Point3d)
data type). For an overview of the various types in the package, check
out the [`OpenSolid.Geometry.Types`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Geometry-Types) module.

## Usage details

Following the [Elm package design guidelines](http://package.elm-lang.org/help/design-guidelines#module-names-should-not-reappear-in-function-names),
most OpenSolid modules are designed to be imported using `as` and then used as
prefixes for the functions and values that they define:

```elm
import OpenSolid.Point3d as Point3d
import OpenSolid.Axis3d as Axis3d

rotatedPoint =
    Point3d.rotateAround Axis3d.x (degrees 30) originalPoint
```

The exception is the `Types` module containing the definitions of core data
types. The types themselves are intended to be exposed directly:

```elm
import OpenSolid.Geometry.Types exposing (..)

myPoint =
    Point2d ( 2, 1 ) -- Point2d, not Types.Point2d or OpenSolid.Point2d

negativeXDirection =
    Direction2d ( -1, 0 )

axis =
    Axis2d
        { originPoint = myPoint
        , direction = negativeXDirection
        }
```

## Related projects

  - [`opensolid/svg`](http://package.elm-lang.org/packages/opensolid/svg/latest)
    lets you create and manipulate SVG drawings using the data types from this
    package.
  - [`opensolid/linear-algebra`](http://package.elm-lang.org/packages/opensolid/linear-algebra/latest)
    provides interop support between this package and
    [`elm-community/linear-algebra`](http://package.elm-lang.org/packages/elm-community/linear-algebra/latest).

## Questions? Comments?

Please [open a new issue](https://github.com/opensolid/geometry/issues) if you
run into a bug, if any documentation is missing/incorrect/confusing, or if
there's a new feature that you would find useful. For general questions about
using OpenSolid, try posting on:

  - [Elm Slack](http://elmlang.herokuapp.com/) (mention @ianmackenzie in your
    questions so I get a notification)
  - [Stack Overflow](https://stackoverflow.com/questions/ask?tags=opensolid+elm)
    (tag your question with 'opensolid' and 'elm')
  - The [r/elm](https://reddit.com/r/elm) subreddit
  - The [elm-discuss](https://groups.google.com/forum/#!forum/elm-discuss)
    Google Group
  - Or if you happen to be in the New York area, come on out to the
    [Elm NYC meetup](https://www.meetup.com/Elm-NYC/) =)

Have fun, and don't be afraid to ask for help!
