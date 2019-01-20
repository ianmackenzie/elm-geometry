# Frames and coordinate systems

## Introduction

3D geometry is usually defined using [Cartesian coordinates][1], where points
are described by their X, Y and Z coordinates. For example, a point with
coordinates (2, 1, 3) means "start at the point (0, 0, 0), move 2 units in the X
direction, 1 unit in the Y direction and then 3 units in the Z direction".
However, to actually understand _where_ that point is, you have to know

  - Where the point (0, 0, 0) is
  - What the X, Y and Z directions are
  - What units the coordinates are given in (meters? feet? pixels?)

Additionally, as logic grows more complex, it is very common to use many
_different_ coordinate systems and have to convert back and forth between
geometry defined in different coordinate systems. For example, if you were
creating a 3D space flight simulator, you might have:

  - A coordinate system centered on a particular solar system
  - A coordinate system centered on a planet, that moves and rotates with the
    planet
  - A coordinate system centered on a space station, that moves and rotates with
    the space station
  - A coordinate system centered on the player's ship, that moves and rotates
    with the ship

Some of these coordinate systems may be defined in terms of other ones; the
planet coordinate system is likely defined relative to the solar system
coordinate system, and the space station may be defined relative to the planet.

[1]: https://en.wikipedia.org/wiki/Cartesian_coordinate_system
