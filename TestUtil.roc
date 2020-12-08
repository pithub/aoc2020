interface TestUtil exposes [ show, verify, ints ] imports []


verify : I64, I64, I64, I64, I64 -> List I64
verify = \day, part, num, actual, expected ->
    if actual == expected then
        [ 1, day, part, num, actual ]
    else
        [ 0, day, part, num, expected, actual ]


show : I64, I64, I64 -> List I64
show = \day, part, value ->
    [ 2, day, part, value ]


ints : List I64 -> List I64
ints = \input ->
    (List.walk input intWalker initialIntAcc).output


IntAcc : { current : I64, output : List I64 }


initialIntAcc : IntAcc
initialIntAcc =
    { current: 0, output: [] }


intWalker : I64, IntAcc -> IntAcc
intWalker = \val, acc ->
    if val == 10 then
        { current: 0, output: List.append acc.output acc.current }
    else
        { acc & current: 10 * acc.current + val - 48 }
