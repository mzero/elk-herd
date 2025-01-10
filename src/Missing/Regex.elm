module Missing.Regex exposing
    ( regex
    )

import Regex

{-| Use this for static regex strings in your code. No reason to write code
to handle `Regex.fromString`'s possible `Nothing`. It'll never happen in
production. So instead, just make failure return a regex that always fails.
You'll notice that if you get the static regex wrong while working on the code.
-}
regex : String -> Regex.Regex
regex = Maybe.withDefault Regex.never << Regex.fromString
