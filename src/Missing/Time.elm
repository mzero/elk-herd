module Missing.Time exposing
    ( Time
    , millisecond
    , second
    , minute
    )

type alias Time = Float

millisecond : Time
millisecond = 1

second : Time
second = 1000 * millisecond

minute : Time
minute = 60 * second
