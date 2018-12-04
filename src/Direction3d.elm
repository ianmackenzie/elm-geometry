--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Direction3d exposing
    ( Direction3d
    , x, y, z, positiveX, negativeX, positiveY, negativeY, positiveZ, negativeZ
    , from, on, fromAzimuthAndElevation, perpendicularTo, perpendicularBasis, orthonormalize, orthogonalize, unsafe
    , components, xComponent, yComponent, zComponent, azimuth, elevation
    , equalWithin
    , componentIn, angleFrom
    , toVector
    , reverse, rotateAround, mirrorAcross, projectOnto
    , relativeTo, placeIn, projectInto
    )

{-| A `Direction3d` represents a direction like 'up' or 'north' or 'forwards'.
They are represented using X, Y and Z components, and can be converted to
vectors if necessary, but should be thought of as conceptually different.
Directions have several uses, such as:

  - Constructing a vector from a length and direction
  - Determining the component of a vector in a particular direction (for
    example, finding the component of velocity in the up direction to get
    vertical speed)
  - Determining the angle between two directions
  - Defining the orientation of an axis, plane or reference frame

@docs Direction3d


# Constants

@docs x, y, z, positiveX, negativeX, positiveY, negativeY, positiveZ, negativeZ


# Constructors

@docs from, on, fromAzimuthAndElevation, perpendicularTo, perpendicularBasis, orthonormalize, orthogonalize, unsafe


# Properties

@docs components, xComponent, yComponent, zComponent, azimuth, elevation


# Comparison

@docs equalWithin


# Measurement

@docs componentIn, angleFrom


# Conversion

@docs toVector


# Transformations

@docs reverse, rotateAround, mirrorAcross, projectOnto


# Coordinate conversions

Like other transformations, coordinate transformations of directions depend only
on the orientations of the relevant frames, not their positions.

For the examples, assume the following definition of a local coordinate frame,
one that is rotated 30 degrees counterclockwise around the Z axis from the
global XYZ frame:

    rotatedFrame =
        Frame3d.atOrigin
            |> Frame3d.rotateAround Axis3d.z (degrees 30)

@docs relativeTo, placeIn, projectInto

-}

import Angle exposing (Angle)
import Bootstrap.SketchPlane3d as SketchPlane3d
import Direction2d exposing (Direction2d)
import Geometry.Types as Types exposing (Axis3d, Frame3d, Plane3d, Point3d, SketchPlane3d)
import Quantity exposing (Unitless)
import Quantity.Extra as Quantity
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)


{-| -}
type alias Direction3d coordinates =
    Types.Direction3d coordinates


{-| Synonym for `Direction3d.positiveX`.
-}
x : Direction3d coordinates
x =
    unsafe ( 1, 0, 0 )


{-| Synonym for `Direction3d.positiveY`.
-}
y : Direction3d coordinates
y =
    unsafe ( 0, 1, 0 )


{-| Synonym for `Direction3d.positiveZ`.
-}
z : Direction3d coordinates
z =
    unsafe ( 0, 0, 1 )


{-| The positive X direction.

    Direction3d.components Direction3d.positiveX
    --> ( 1, 0, 0 )

-}
positiveX : Direction3d coordinates
positiveX =
    unsafe ( 1, 0, 0 )


{-| The negative X direction.

    Direction3d.components Direction3d.negativeX
    --> ( -1, 0, 0 )

-}
negativeX : Direction3d coordinates
negativeX =
    unsafe ( -1, 0, 0 )


{-| The positive Y direction.

    Direction3d.components Direction3d.positiveY
    --> ( 0, 1, 0 )

-}
positiveY : Direction3d coordinates
positiveY =
    unsafe ( 0, 1, 0 )


{-| The negative Y direction.

    Direction3d.components Direction3d.negativeY
    --> ( 0, -1, 0 )

-}
negativeY : Direction3d coordinates
negativeY =
    unsafe ( 0, -1, 0 )


{-| The positive Z direction.

    Direction3d.components Direction3d.positiveZ
    --> ( 0, 0, 1 )

-}
positiveZ : Direction3d coordinates
positiveZ =
    unsafe ( 0, 0, 1 )


{-| The negative Z direction.

    Direction3d.components Direction3d.negativeZ
    --> ( 0, 0, -1 )

-}
negativeZ : Direction3d coordinates
negativeZ =
    unsafe ( 0, 0, -1 )


{-| Construct a direction directly from its X, Y and Z components. Note that
**you must ensure that the sum of the squares of the given components is exactly
one**:

    Direction3d.unsafe ( 1, 0, 0 )

    Direction3d.unsafe ( 0, -1, 0 )

    Direction3d.unsafe ( 0.6, 0, 0.8 )

are all valid but

    Direction3d.unsafe ( 2, 0, 0 )

    Direction3d.unsafe ( 1, 1, 1 )

are not. Instead of using `Direction3d.unsafe`, it may be easier to use
constructors like `Direction3d.on` or `Direction3d.fromAzimuthAndElevation`
(which will always result in a valid direction), or start with existing
directions and transform them as necessary.

-}
unsafe : ( Float, Float, Float ) -> Direction3d coordinates
unsafe givenComponents =
    Types.Direction3d givenComponents


{-| Construct a 3D direction lying _on_ a sketch plane by providing a 2D
direction specified in XY coordinates _within_ the sketch plane.

    horizontalDirection =
        Direction3d.on SketchPlane3d.xy <|
            Direction2d.fromAngle (degrees 45)

    Direction3d.components horizontalDirection
    --> ( 0.7071, 0.7071, 0 )

    thirtyDegreesFromZ =
        Direction3d.on SketchPlane3d.zx <|
            Direction2d.fromAngle (degrees 30)

    Direction3d.components thirtyDegreesFromZ
    --> ( 0.5, 0, 0.866 )

-}
on : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Direction2d coordinates2d -> Direction3d coordinates3d
on sketchPlane direction2d =
    let
        ( dx, dy ) =
            Direction2d.components direction2d

        ( ux, uy, uz ) =
            components (SketchPlane3d.xDirection sketchPlane)

        ( vx, vy, vz ) =
            components (SketchPlane3d.yDirection sketchPlane)
    in
    unsafe
        ( dx * ux + dy * vx
        , dx * uy + dy * vy
        , dx * uz + dy * vz
        )


{-| Construct a direction using azimuthal and elevation angles relative to the
global XYZ frame. The azimuth defines the direction's polar angle on the global
XY plane (from X towards Y) and the elevation defines its angle out of the XY
plane towards positive Z.

    Direction3d.components
        (Direction3d.fromAzimuthAndElevation
            (degrees 45)
            (degrees 45)
        )
    --> ( 0.5, 0.5, 0.7071 )

-}
fromAzimuthAndElevation : Angle -> Angle -> Direction3d coordinates
fromAzimuthAndElevation givenAzimuth givenElevation =
    let
        cosElevation =
            Angle.cos givenElevation
    in
    unsafe
        ( cosElevation * Angle.cos givenAzimuth
        , cosElevation * Angle.sin givenAzimuth
        , Angle.sin givenElevation
        )


{-| Attempt to construct the direction from the first given point to the second.
If the two points are coincident, returns `Nothing`.

    point =
        Point3d.fromCoordinates ( 1, 0, 1 )

    Direction3d.from Point3d.origin point
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees 0)
    -->         (degrees 45)
    -->     )

    Direction3d.from point Point3d.origin
    --> Just
    -->     (Direction3d.fromAzimuthAndElevation
    -->         (degrees 180)
    -->         (degrees -45)
    -->     )

    Direction3d.from point point
    --> Nothing

-}
from : Point3d units coordinates -> Point3d units coordinates -> Maybe (Direction3d coordinates)
from firstPoint secondPoint =
    Vector3d.direction (Vector3d.from firstPoint secondPoint)


{-| Construct an arbitrary direction perpendicular to the given direction. The
exact resulting direction is not specified, but it is guaranteed to be
perpendicular to the given direction.

    Direction3d.perpendicularTo Direction3d.x
    --> Direction3d.negativeZ

    Direction3d.perpendicularTo Direction3d.y
    --> Direction3d.positiveZ

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees 0)
            (degrees 60)

    Direction3d.perpendicularTo direction
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 0)
    -->     (degrees -30)

-}
perpendicularTo : Direction3d coordinates -> Direction3d coordinates
perpendicularTo direction =
    let
        perpendicularVector =
            Vector3d.perpendicularTo (toVector direction)

        length =
            Vector3d.length perpendicularVector

        ( vx, vy, vz ) =
            Vector3d.components perpendicularVector
    in
    unsafe
        ( Quantity.ratio vx length
        , Quantity.ratio vy length
        , Quantity.ratio vz length
        )


{-| Construct a pair of directions that are perpendicular to each other and both
perpendicular to the given direction.

The given direction and the two returned directions will form a
[right-handed](https://en.wikipedia.org/wiki/Cartesian_coordinate_system#Orientation_and_handedness)
system (that is, a right-handed `Frame3d` could be constructed by using the
given direction as the X direction and the two returned directions as the Y and
Z directions, or the given direction as the Z direction and the two returned
directions as the X and Y directions).

    Direction3d.perpendicularBasis Direction3d.x
    --> ( Direction3d.negativeZ
    --> , Direction3d.positiveY
    --> )

    Direction3d.perpendicularBasis Direction3d.y
    --> ( Direction3d.positiveZ
    --> , Direction3d.positiveX
    --> )

-}
perpendicularBasis : Direction3d coordinates -> ( Direction3d coordinates, Direction3d coordinates )
perpendicularBasis direction =
    let
        ( x0, y0, z0 ) =
            components direction

        xDirection =
            perpendicularTo direction

        ( x1, y1, z1 ) =
            components xDirection

        yDirection =
            unsafe
                ( y0 * z1 - z0 * y1
                , z0 * x1 - x0 * z1
                , x0 * y1 - y0 * x1
                )
    in
    ( xDirection, yDirection )


{-| Attempt to form a set of three mutually perpendicular directions from the
three given vectors by performing [Gram-Schmidt normalization](https://en.wikipedia.org/wiki/Gram%E2%80%93Schmidt_process):

  - The first returned direction will be equal to the direction of the first
    given vector
  - The second returned direction will be as close as possible to the second
    given vector while being perpendicular to the first returned direction
  - The third returned direction will be as close as possible to the third given
    vector while being perpendicular to the first and second returned directions

If any of the given vectors are zero, any two of them are parallel, or the three
are coplanar, returns `Nothing`.

    Direction3d.orthonormalize
        (Vector3d.fromComponents ( 3, 3, 0 ))
        (Vector3d.fromComponents ( 0, 2, 0 ))
        (Vector3d.fromComponents ( 1, 2, 3 ))
    --> Just
    -->     ( Direction3d.fromAzimuthAndElevation
    -->         (degrees 45)
    -->         (degrees 0)
    -->     , Direction3d.fromAzimuthAndElevation
    -->         (degrees 135)
    -->         (degrees 0)
    -->     , Direction3d.positiveZ
    -->     )

    -- Three vectors in the XY plane:
    Direction3d.orthonormalize
        (Vector3d.fromComponents ( 2, 0, 0 ))
        (Vector3d.fromComponents ( 3, 1, 0 ))
        (Vector3d.fromComponents ( 4, 2, 0 ))
    --> Nothing

-}
orthonormalize : Vector3d units coordinates -> Vector3d units coordinates -> Vector3d units coordinates -> Maybe ( Direction3d coordinates, Direction3d coordinates, Direction3d coordinates )
orthonormalize xVector xyVector xyzVector =
    Vector3d.direction xVector
        |> Maybe.andThen
            (\xDirection ->
                let
                    yVector =
                        xyVector
                            |> Vector3d.minus
                                (xyVector |> Vector3d.projectionIn xDirection)
                in
                Vector3d.direction yVector
                    |> Maybe.andThen
                        (\yDirection ->
                            let
                                rightHandedZVector =
                                    Vector3d.crossProduct xVector xyVector

                                tripleProduct =
                                    Vector3d.tripleProduct
                                        xVector
                                        xyVector
                                        xyzVector

                                zVector =
                                    if tripleProduct |> Quantity.greaterThan Quantity.zero then
                                        rightHandedZVector

                                    else if tripleProduct |> Quantity.lessThan Quantity.zero then
                                        Vector3d.reverse rightHandedZVector

                                    else
                                        Vector3d.zero
                            in
                            Vector3d.direction zVector
                                |> Maybe.map
                                    (\zDirection ->
                                        ( xDirection
                                        , yDirection
                                        , zDirection
                                        )
                                    )
                        )
            )


{-| Attempt to form a set of three mutually perpendicular directions from the
three given directions;

    Direction3d.orthogonalize
        xDirection
        yDirection
        zDirection

is equivalent to

    Direction3d.orthonormalize
        (Direction3d.toVector xDirection)
        (Direction3d.toVector yDirection)
        (Direction3d.toVector zDirection)

-}
orthogonalize : Direction3d coordinates -> Direction3d coordinates -> Direction3d coordinates -> Maybe ( Direction3d coordinates, Direction3d coordinates, Direction3d coordinates )
orthogonalize xDirection yDirection zDirection =
    orthonormalize
        (toVector xDirection)
        (toVector yDirection)
        (toVector zDirection)


{-| Get the components of a direction as a tuple (the components it would have
as a unit vector, also know as its direction cosines).

    ( x, y, z ) =
        Direction3d.components direction

-}
components : Direction3d coordinates -> ( Float, Float, Float )
components (Types.Direction3d directionComponents) =
    directionComponents


{-| Get the X component of a direction.

    Direction3d.xComponent Direction3d.x
    --> 1

    Direction3d.xComponent Direction3d.y
    --> 0

-}
xComponent : Direction3d coordinates -> Float
xComponent (Types.Direction3d ( xComponent_, _, _ )) =
    xComponent_


{-| Get the Y component of a direction.

    Direction3d.yComponent Direction3d.y
    --> 1

    Direction3d.yComponent Direction3d.z
    --> 0

-}
yComponent : Direction3d coordinates -> Float
yComponent (Types.Direction3d ( _, yComponent_, _ )) =
    yComponent_


{-| Get the Z component of a direction.

    Direction3d.zComponent Direction3d.z
    --> 1

    Direction3d.zComponent Direction3d.x
    --> 0

-}
zComponent : Direction3d coordinates -> Float
zComponent (Types.Direction3d ( _, _, zComponent_ )) =
    zComponent_


{-| Find the component of one direction in another direction. This is equal to
the cosine of the angle between the directions, or equivalently the dot product
of the two directions converted to unit vectors.

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees 0)
            (degrees 60)

    Direction3d.componentIn Direction3d.x direction
    --> 0.5

    Direction3d.componentIn Direction3d.z direction
    --> 0.866

    Direction3d.componentIn direction direction
    --> 1

    direction
        |> Direction3d.componentIn
            (Direction3d.reverse direction)
    --> -1

This is more general and flexible than using `xComponent`, `yComponent` or
`zComponent`, all of which can be expressed in terms of `componentIn`; for
example,

    Direction3d.zComponent direction

is equivalent to

    Direction3d.componentIn Direction3d.z direction

-}
componentIn : Direction3d coordinates -> Direction3d coordinates -> Float
componentIn firstDirection secondDirection =
    let
        ( x1, y1, z1 ) =
            components firstDirection

        ( x2, y2, z2 ) =
            components secondDirection
    in
    x1 * x2 + y1 * y2 + z1 * z2


{-| Get the angle of a direction in the XY plane, measured from the X axis
towards the Y axis (counterclockwise around the Z axis). The result will be in
the range -π to π.

    Direction3d.azimuth Direction3d.x
    --> 0

    Direction3d.azimuth Direction3d.y
    --> degrees 90

    Direction3d.azimuth Direction3d.negativeY
    --> degrees -90

    Direction3d.azimuth Direction3d.negativeX
    --> degrees 180

Vertical directions are considered to have an azimuth of zero:

    Direction3d.azimuth Direction3d.z
    --> 0

-}
azimuth : Direction3d coordinates -> Angle
azimuth direction =
    Angle.radians (atan2 (yComponent direction) (xComponent direction))


{-| Get the angle of a direction from the XY plane towards positive Z. The
result will be in the range -π/2 to π/2.

    Direction3d.elevation Direction3d.x
    --> 0

    Direction3d.elevation Direction3d.negativeY
    --> 0

    Direction3d.elevation Direction3d.z
    --> degrees 90

    Direction3d.elevation Direction3d.negativeZ
    --> degrees -90

-}
elevation : Direction3d coordinates -> Angle
elevation direction =
    Angle.asin (zComponent direction)


{-| Compare two directions within an angular tolerance. Returns true if the
angle between the two given directions is less than the given tolerance.

    rotatedDirection =
        Direction3d.x
            |> Direction3d.rotateAround Axis3d.z
                (degrees 2)

    Direction3d.equalWithin (degrees 5)
        Direction3d.x
        rotatedDirection
    --> True

    Direction3d.equalWithin (degrees 1)
        Direction3d.x
        rotatedDirection
    --> False

-}
equalWithin : Angle -> Direction3d coordinates -> Direction3d coordinates -> Bool
equalWithin angle firstDirection secondDirection =
    angleFrom firstDirection secondDirection
        |> Quantity.lessThanOrEqualTo angle


{-| Convert a direction to a unit vector.

    Direction3d.toVector Direction3d.y
    --> Vector3d.fromComponents ( 0, 1, 0 )

-}
toVector : Direction3d coordinates -> Vector3d Unitless coordinates
toVector direction =
    Vector3d.fromTuple (components direction)


{-| Find the angle from one direction to another. The result will be in the
range 0 to π.

    Direction3d.angleFrom Direction3d.x Direction3d.x
    --> degrees 0

    Direction3d.angleFrom Direction3d.x Direction3d.z
    --> degrees 90

    Direction3d.angleFrom
        Direction3d.y
        Direction3d.negativeY
    --> degrees 180

-}
angleFrom : Direction3d coordinates -> Direction3d coordinates -> Angle
angleFrom firstDirection secondDirection =
    let
        ( x1, y1, z1 ) =
            components firstDirection

        ( x2, y2, z2 ) =
            components secondDirection

        relativeX =
            x1 * x2 + y1 * y2 + z1 * z2

        cx =
            y1 * z2 - z1 * y2

        cy =
            z1 * x2 - x1 * z2

        cz =
            x1 * y2 - y1 * x2

        relativeY =
            sqrt (cx * cx + cy * cy + cz * cz)
    in
    Angle.radians (atan2 relativeY relativeX)


{-| Reverse a direction.

    Direction3d.reverse Direction3d.y
    --> Direction3d.negativeY

-}
reverse : Direction3d coordinates -> Direction3d coordinates
reverse direction =
    let
        ( dx, dy, dz ) =
            components direction
    in
    unsafe ( -dx, -dy, -dz )


{-| Rotate a direction around an axis by a given angle.

    Direction3d.y
        |> Direction3d.rotateAround Axis3d.x (degrees 90)
    --> Direction3d.z

Note that only the direction of the axis affects the result, not the position of
its origin point, since directions are position-independent:

    offsetAxis =
        Axis3d.withDirection Direction3d.z
            (Point3d.fromCoordinates ( 100, 200, 300 ))

    Direction3d.x
        |> Direction3d.rotateAround offsetAxis (degrees 90)
    --> Direction3d.y

-}
rotateAround : Axis3d units coordinates -> Angle -> Direction3d coordinates -> Direction3d coordinates
rotateAround (Types.Axis3d axis) angle direction =
    let
        ( x1, y1, z1 ) =
            components direction

        ( x0, y0, z0 ) =
            components axis.direction

        halfAngle =
            Quantity.scaleBy 0.5 angle

        sinHalfAngle =
            Angle.sin halfAngle

        qx =
            x0 * sinHalfAngle

        qy =
            y0 * sinHalfAngle

        qz =
            z0 * sinHalfAngle

        qw =
            Angle.cos halfAngle

        wx =
            qw * qx

        wy =
            qw * qy

        wz =
            qw * qz

        xx =
            qx * qx

        xy =
            qx * qy

        xz =
            qx * qz

        yy =
            qy * qy

        yz =
            qy * qz

        zz =
            qz * qz

        a00 =
            1 - 2 * (yy + zz)

        a10 =
            2 * (xy + wz)

        a20 =
            2 * (xz - wy)

        a01 =
            2 * (xy - wz)

        a11 =
            1 - 2 * (xx + zz)

        a21 =
            2 * (yz + wx)

        a02 =
            2 * (xz + wy)

        a12 =
            2 * (yz - wx)

        a22 =
            1 - 2 * (xx + yy)
    in
    unsafe
        ( a00 * x1 + a01 * y1 + a02 * z1
        , a10 * x1 + a11 * y1 + a12 * z1
        , a20 * x1 + a21 * y1 + a22 * z1
        )


{-| Mirror a direction across a plane.

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees 30)
            (degrees 60)

    Direction3d.mirrorAcross Plane3d.xy direction
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 30)
    -->     (degrees -60)

Note that only the normal direction of the plane affects the result, not the
position of its origin point, since directions are position-independent:

    Direction3d.mirrorAcross Plane3d.yz direction
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 150)
    -->     (degrees 60)

    offsetPlane =
        Plane3d.offsetBy 10 Plane3d.yz

    Direction3d.mirrorAcross offsetPlane direction
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 150)
    -->     (degrees 60)

-}
mirrorAcross : Plane3d units coordinates -> Direction3d coordinates -> Direction3d coordinates
mirrorAcross (Types.Plane3d plane) direction =
    let
        ( x0, y0, z0 ) =
            components direction

        ( nx, ny, nz ) =
            components plane.normalDirection

        a =
            1 - 2 * nx * nx

        b =
            1 - 2 * ny * ny

        c =
            1 - 2 * nz * nz

        d =
            -2 * ny * nz

        e =
            -2 * nx * nz

        f =
            -2 * nx * ny
    in
    unsafe
        ( a * x0 + f * y0 + e * z0
        , f * x0 + b * y0 + d * z0
        , e * x0 + d * y0 + c * z0
        )


{-| Find the [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of a direction onto a plane (renormalized to have unit length). If the given
direction is exactly perpendicular to the given plane, returns `Nothing`.

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees -60)
            (degrees 0)

    Direction3d.projectOnto Plane3d.xy direction
    --> Just direction

    Direction3d.projectOnto Plane3d.xz direction
    --> Just Direction3d.x

    Direction3d.projectOnto Plane3d.yz direction
    --> Just Direction3d.negativeY

    Direction3d.projectOnto Plane3d.xy Direction3d.z
    --> Nothing

-}
projectOnto : Plane3d units coordinates -> Direction3d coordinates -> Maybe (Direction3d coordinates)
projectOnto (Types.Plane3d plane) direction =
    let
        directionVector =
            toVector direction

        projectedVector =
            directionVector
                |> Vector3d.minus
                    (directionVector
                        |> Vector3d.projectionIn plane.normalDirection
                    )
    in
    Vector3d.direction projectedVector


{-| Take a direction defined in global coordinates, and return it expressed in
local coordinates relative to a given reference frame.

    Direction3d.relativeTo rotatedFrame Direction3d.x
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees -30)
    -->     (degrees 0)

    Direction3d.relativeTo rotatedFrame Direction3d.y
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 60)
    -->     (degrees 0)

    Direction3d.relativeTo rotatedFrame Direction3d.z
    --> Direction3d.z

-}
relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> Direction3d globalCoordinates -> Direction3d localCoordinates
relativeTo (Types.Frame3d frame) direction =
    unsafe
        ( componentIn frame.xDirection direction
        , componentIn frame.yDirection direction
        , componentIn frame.zDirection direction
        )


{-| Take a direction defined in local coordinates relative to a given reference
frame, and return that direction expressed in global coordinates.

    Direction3d.placeIn rotatedFrame Direction3d.x
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 30)
    -->     (degrees 0)

    Direction3d.placeIn rotatedFrame Direction3d.y
    --> Direction3d.fromAzimuthAndElevation
    -->     (degrees 120)
    -->     (degrees 0)

    Direction3d.placeIn rotatedFrame Direction3d.z
    --> Direction3d.z

-}
placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> Direction3d localCoordinates -> Direction3d globalCoordinates
placeIn (Types.Frame3d frame) direction =
    let
        ( x1, y1, z1 ) =
            components frame.xDirection

        ( x2, y2, z2 ) =
            components frame.yDirection

        ( x3, y3, z3 ) =
            components frame.zDirection

        ( x0, y0, z0 ) =
            components direction
    in
    unsafe
        ( x1 * x0 + x2 * y0 + x3 * z0
        , y1 * x0 + y2 * y0 + y3 * z0
        , z1 * x0 + z2 * y0 + z3 * z0
        )


{-| Project a direction into a given sketch plane. Conceptually, this finds the
[orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
of the direction onto the plane, re-normalizes it to have unit length, and then
expresses the projected direction in 2D sketch coordinates.

This is only possible if the direction is not perpendicular to the sketch
plane; if it is perpendicular, `Nothing` is returned.

    direction =
        Direction3d.fromAzimuthAndElevation
            (degrees -60)
            (degrees 0)

    Direction3d.projectInto SketchPlane3d.xy direction
    --> Just (Direction2d.fromAngle (degrees -60))

    Direction3d.projectInto SketchPlane3d.xz direction
    --> Just Direction2d.x

    Direction3d.projectInto SketchPlane3d.yz direction
    --> Just Direction2d.negativeX

    Direction3d.projectInto SketchPlane3d.xy Direction3d.z
    --> Nothing

-}
projectInto : SketchPlane3d units coordinates3d { defines : coordinates2d } -> Direction3d coordinates3d -> Maybe (Direction2d coordinates2d)
projectInto (Types.SketchPlane3d sketchPlane) direction =
    Vector2d.fromTuple
        ( componentIn sketchPlane.xDirection direction
        , componentIn sketchPlane.yDirection direction
        )
        |> Vector2d.direction
