module HelloWorld exposing (..)

import Html
import OpenSolid.Core.Types exposing (..)
import OpenSolid.Core.Vector2d as Vector2d


main =
    Html.text (toString (Vector2d.length (Vector2d ( 2, 3 ))))
