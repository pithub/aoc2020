app "aoc2020" imports [ Day01.{ output } ] provides [ aocMain ] to "./platform"


aocMain : List (List Int)
aocMain =
    output
