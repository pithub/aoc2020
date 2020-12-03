app "aoc2020" imports [ Day03.{ output } ] provides [ aocMain ] to "./platform"


aocMain : List (List Int)
aocMain =
    output
