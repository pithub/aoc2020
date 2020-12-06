interface TestUtil exposes [ show, verify, ints ] imports []


verify : Int, Int, Int, Int, Int -> List Int
verify = \day, part, num, actual, expected ->
    if actual == expected then
        [ 1, day, part, num, actual ]
    else
        [ 0, day, part, num, expected, actual ]


show : Int, Int, Int -> List Int
show = \day, part, value ->
    [ 2, day, part, value ]


ints : List Int -> List Int
ints = \input ->
    (List.walk input intWalker initialIntAcc).output


IntAcc : { current : Int, output : List Int }


initialIntAcc : IntAcc
initialIntAcc =
    { current: 0, output: [] }


intWalker : Int, IntAcc -> IntAcc
intWalker = \val, acc ->
    if val == 10 then
        { current: 0, output: List.append acc.output acc.current }
    else
        { acc & current: 10 * acc.current + val - 48 }
