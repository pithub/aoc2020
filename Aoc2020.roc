app "aoc2020" imports [ Effect, Day05 ] provides [ aocMain ] to "./platform"


aocMain : Effect.Effect {} as Fx
aocMain =
    Effect.readFile "Day05.txt"
        |> Effect.after (\input -> Day05.output input
            |> Effect.writeData )


#  Day  Answers            Topic
#  ---  -----------------  -----------------------------------------------------
#   01  1007331, 48914340  pairs and triples
#   02  655, 673           valid passwords
#   03  159, 6419669520    trees on path in a map
#   04  230, 156           valid passports
#   05  963, 592           seats in airplane
