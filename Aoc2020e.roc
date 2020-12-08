app "aoc2020e"
    packages { base: "effect-platform" }
    imports [ Day08, base.Task ]
    provides [ aocMain ] to base


aocMain : Task.Task {} as Fx
aocMain =
    Task.readFile "Day08.txt"
        |> Task.after (\input -> Day08.output input
            |> Task.writeData)


#  Day  Answers            Run  Topic
#  ---  -----------------  ---  ------------------------------------------------
#   01  1007331, 48914340  ero  pairs and triples
#   02  655, 673           ---  valid passwords
#   03  159, 6419669520    er-  trees on path in a map
#   04  230, 156           nro  valid passports
#   05  963, 592           ero  seats in airplane
#   06  6387, 3039         ero  questionnaire answers
#   07  316, 11310         ero  nested bags
#   08  1475, 1270         ero  game console code
