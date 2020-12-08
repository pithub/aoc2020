app "aoc2020n"
    packages { base: "normal-platform" }
    imports [ Day04.{ output } ]
    provides [ aocMain ] to base


aocMain : List (List I64)
aocMain =
    output
