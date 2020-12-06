interface ListZip exposes [ Zip, new, first, last, forward, backward, move ] imports []


Zip : { len: Int, idx: Int, val: Int, default: Int }


new : List Int, Int -> Zip
new = \list, default ->
    len = List.len list
    idx = 0
    val = read list idx default

    { len, idx, val, default }


first : Zip, List Int -> Zip
first = \zip, list ->
    move zip list 0


last : Zip, List Int -> Zip
last = \zip, list ->
    move zip list (zip.len - 1)


forward : Zip, List Int -> Zip
forward = \zip, list ->
    move zip list (zip.idx + 1)


backward : Zip, List Int -> Zip
backward = \zip, list ->
    move zip list (zip.idx - 1)


move : Zip, List Int, Int -> Zip
move = \zip, list, idx ->
    val = read list idx zip.default
    
    { zip & idx, val }


read : List Int, Int, Int -> Int
read = \list, idx, default ->
    when List.get list idx is
        Ok n -> n
        _ -> default
