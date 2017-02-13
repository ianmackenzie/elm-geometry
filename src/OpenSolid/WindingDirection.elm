module WindingDirection
    exposing
        ( WindingDirection(..)
        , flip
        )


type WindingDirection
    = Clockwise
    | Counterclockwise


flip : WindingDirection -> WindingDirection
flip windingDirection =
    case windingDirection of
        Clockwise ->
            Counterclockwise

        Counterclockwise ->
            Clockwise
