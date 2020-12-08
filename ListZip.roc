interface ListZip exposes [ Zip, new, first, last, forward, backward, move ] imports []


Zip : { len: I64, idx: I64, val: I64, default: I64 }


new : List I64, I64 -> Zip
new = \list, default ->
    len = List.len list
    idx = 0
    val = read list idx default

    { len, idx, val, default }


first : Zip, List I64 -> Zip
first = \zip, list ->
    move zip list 0


last : Zip, List I64 -> Zip
last = \zip, list ->
    move zip list (zip.len - 1)


forward : Zip, List I64 -> Zip
forward = \zip, list ->
    move zip list (zip.idx + 1)


backward : Zip, List I64 -> Zip
backward = \zip, list ->
    move zip list (zip.idx - 1)


move : Zip, List I64, I64 -> Zip
move = \zip, list, idx ->
    val = read list idx zip.default
    
    { zip & idx, val }


read : List I64, I64, I64 -> I64
read = \list, idx, default ->
    when List.get list idx is
        Ok n -> n
        _ -> default
