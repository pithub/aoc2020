app "aoc2020" imports [ Day02.{ output } ] provides [ aocMain ] to "./platform"


aocMain : List (List Int)
aocMain =
    output
