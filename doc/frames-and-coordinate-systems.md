# Units and coordinate systems

2D/3D geometry is often represented using X/Y/Z coordinates. For example, in
previous versions of `elm-geometry`, we might have constructed a 2D point as

```elm
point =
    Point2d.fromCoordinates ( 200, 300 )
```

However, this leaves several things implicit. Are those numbers in meters,
pixels or something else? If they're in pixels, and this is an on-screen point,
is the X direction right or left? Is Y up or down? Is the point (0, 0) at the
upper left corner of the screen, the lower left, the center, or somewhere else
entirely?

## Units

To answer the first question, `elm-geometry` now uses the generic `Quantity`
type from `elm-units` for all coordinate values, instead of using plain `Float`s
or `Int`s. The above example might now be written as

```elm
point =
    Point2d.fromCoordinates
        ( Length.meters 200
        , Length.meters 300
        )
```

or

```elm
point =
    Point2d.fromCoordinates
        ( Pixels.pixels 200
        , Pixels.pixels 300
        )
```

This makes it explicit what units the point is in (in a way that can be type
checked by the Elm compiler!) but doesn't address the remaining questions.

## Coordinate systems

In addition to tracking what units are used, `elm-geometry` also lets you add
(optional) type annotations to specify what _coordinate system_ a particular
point is defined in. For example, we might declare a `TopLeftCoordinates` type
and then add a type annotation to `point` asserting that it is defined in
coordinates relative to the top-left corner of the screen:

```elm
{-| A coordinate system where (0, 0) is the top left corner of the screen,
positive X is to the right and positive Y is down.
-}
type TopLeftCoordinates =
    TopLeftCoordinates

point : Point2d Pixels TopLeftCoordinates
point =
    Point2d.fromCoordinates
        ( Pixels.pixels 200
        , Pixels.pixels 300
        )
```

Note that the `TopLeftCoordinates` type we declared gives us a convenient place
to document exactly how that coordinate system is defined. This combination now
gives us some nice type safety - the compiler will tell us if we try to mix two
points that have different units or are defined in different coordinate systems.
