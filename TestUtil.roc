interface TestUtil exposes [ show, verify ] imports []


verify : Int, Int, Int, Int, Int -> List Int
verify = \day, part, num, actual, expected ->
    if actual == expected then
        [ 1, day, part, num, actual ]
    else
        [ 0, day, part, num, expected, actual ]


show : Int, Int, Int -> List Int
show = \day, part, value ->
    [ 2, day, part, value ]
