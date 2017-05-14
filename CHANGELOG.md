## 1.2.0 - 2017-05-13

### New types/modules

  - [`Circle3d`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Circle3d)
  - [`Arc2d`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Arc2d)
  - [`Arc3d`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Arc3d)
  - [`QuadraticSpline2d`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-QuadraticSpline2d)
  - [`QuadraticSpline3d`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-QuadraticSpline3d)
  - [`CubicSpline2d`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-CubicSpline2d)
  - [`CubicSpline3d`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-CubicSpline3d)
  - plus [encoders](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Geometry-Encode)
    and [decoders](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Geometry-Decode)
    for the above

### Deprecated functions

  - `Point2d.interpolate` (replaced by `Point2d.interpolateFrom`)
  - `Point3d.interpolate` (replaced by `Point3d.interpolateFrom`)

### New functions

  - [`BoundingBox2d.hullOf`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-BoundingBox2d#hullOf)
  - [`BoundingBox3d.hullOf`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-BoundingBox3d#hullOf)
  - [`Circle2d.throughPoints`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Circle2d#throughPoints)
  - [`Circle2d.placeOnto`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Circle2d#placeOnto)
  - [`Direction2d.orthogonalize`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction2d#orthogonalize)
  - [`Direction3d.orthogonalize`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction3d#orthogonalize)
  - [`Frame2d.isRightHanded`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Frame2d#isRightHanded)
  - [`Frame3d.isRightHanded`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Frame3d#isRightHanded)
  - [`LineSegment2d.intersectionPoint`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-LineSegment2d#intersectionPoint) (thanks [Matthieu Pizenberg](https://github.com/mpizenberg)!)
  - [`Plane3d.throughPoints`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Plane3d#throughPoints)
  - [`Point2d.polar`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point2d#polar)
  - [`Point2d.in_`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point2d#in_)
  - [`Point2d.interpolateFrom`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point2d#interpolateFrom)
  - [`Point2d.directionFrom`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point2d#directionFrom)
  - [`Point3d.in_`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point3d#in_)
  - [`Point3d.on`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point3d#on)
  - [`Point3d.interpolateFrom`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point3d#interpolateFrom)
  - [`Point3d.directionFrom`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point3d#directionFrom)
  - [`Point3d.projectRadiallyOnto`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Point3d#projectRadiallyOnto)
  - [`Scalar.interpolateFrom`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Scalar#interpolateFrom)
  - [`SketchPlane3d.throughPoints`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-SketchPlane3d#throughPoints)
  - [`Triangle3d.normalDirection`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Triangle3d#normalDirection)
  - [`Vector2d.polar`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Vector2d#polar)
  - [`Vector2d.in_`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Vector2d#in_)
  - [`Vector2d.interpolateFrom`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Vector2d#interpolateFrom)
  - [`Vector2d.orthonormalize`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Vector2d#orthonormalize)
  - [`Vector3d.in_`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Vector3d#in_)
  - [`Vector3d.interpolateFrom`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Vector3d#interpolateFrom)
  - [`Vector3d.orthonormalize`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Vector3d#orthonormalize)

### New constants

  - [`Circle2d.unit`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Circle2d#unit)
  - [`Direction2d.positiveX`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction2d#positiveX)
  - [`Direction2d.negativeX`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction2d#negativeX)
  - [`Direction2d.positiveY`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction2d#positiveY)
  - [`Direction2d.negativeY`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction2d#negativeY)
  - [`Direction3d.positiveX`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction3d#positiveX)
  - [`Direction3d.negativeX`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction3d#negativeX)
  - [`Direction3d.positiveY`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction3d#positiveY)
  - [`Direction3d.negativeY`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction3d#negativeY)
  - [`Direction3d.positiveZ`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction3d#positiveZ)
  - [`Direction3d.negativeZ`](http://package.elm-lang.org/packages/opensolid/geometry/1.2.0/OpenSolid-Direction3d#negativeZ)

## 1.1.0 - 2017-02-11

### New functions

  - [`LineSegment2d.along`](http://package.elm-lang.org/packages/opensolid/geometry/1.1.0/OpenSolid-LineSegment2d#along)
  - [`LineSegment3d.along`](http://package.elm-lang.org/packages/opensolid/geometry/1.1.0/OpenSolid-LineSegment3d#along)

## 1.0.0 - 2017-01-26

  - Initial release
