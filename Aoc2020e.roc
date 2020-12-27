app "aoc2020e"
    packages { base: "effect-platform" }
    imports [ Day24, base.Task ]
    provides [ aocMain ] to base


aocMain : Task.Task {} as Fx
aocMain =
    Task.readFile "Day24.txt"
        |> Task.after (\input -> Day24.output input
            |> Task.writeData)


#  Day  Answers                 Run  Topic
#  ---  ----------------------  ---  -------------------------------------------
#   01  1007331, 48914340       ero  pairs and triples
#   02  655, 673                ero  valid passwords
#   03  159, 6419669520         er-  trees on path in a map
#   04  230, 156                nro  valid passports
#   05  963, 592                ero  seats in airplane
#   06  6387, 3039              ero  questionnaire answers
#   07  316, 11310              ero  nested bags
#   08  1475, 1270              ero  game console code
#   09  144381670, 20532569     ero  xmas encryption
#   10  2738, 74049191673856    ero  jolt adapter sequences
#   11  2359, 2131              er-  seats after airplane boarding
#   12  923, 24769              ero  ferry movement
#   13  2947, 526090562196173   ero  buses modulo arithmetic
#   14  9628746976360, ?        ero  memory addresses
#   15  1522, ?                 ero  saying numbers
#   16  21081, ?                ero  ticket fields
#   17
#   18
#   19
#   20
#   21
#   22  32083, 35495            ero  space cards
#   23  25398647, 363807398885  ero  crab cups
#   24  512, 4120               ero  lobby tiles
#   25
