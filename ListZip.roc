interface ListZip exposes
    [ Zip
    , newAtFirst, newAtLast, newAt
    , first, beforeFirst, last, afterLast
    , forward, backward, move, moveTo
    , collect
    ]
    imports []


Zip : { len: I64, idx: I64, val: I64, default: I64 }


newAtFirst : List I64, I64 -> Zip
newAtFirst = \list, default ->
    newAt list 0 default


newAtLast : List I64, I64 -> Zip
newAtLast = \list, default ->
    lastIdx = List.len list - 1
    newAt list lastIdx default


newAt : List I64, I64, I64 -> Zip
newAt = \list, idx, default ->
    len = List.len list
    val = read list idx default

    { len, idx, val, default }


first : Zip, List I64 -> Zip
first = \zip, list ->
    moveTo zip list 0


beforeFirst : Zip -> Bool
beforeFirst = \zip ->
    zip.idx < 0


last : Zip, List I64 -> Zip
last = \zip, list ->
    moveTo zip list (zip.len - 1)


afterLast : Zip -> Bool
afterLast = \zip ->
    zip.idx >= zip.len


forward : Zip, List I64 -> Zip
forward = \zip, list ->
    move zip list 1


backward : Zip, List I64 -> Zip
backward = \zip, list ->
    move zip list -1


move : Zip, List I64, I64 -> Zip
move = \zip, list, len ->
    idx = zip.idx + len
    moveTo zip list idx


moveTo : Zip, List I64, I64 -> Zip
moveTo = \zip, list, idx ->
    if idx == zip.idx then
        zip
    else
        val = read list idx zip.default

        { zip & idx, val }


read : List I64, I64, I64 -> I64
read = \list, idx, default ->
    when List.get list idx is
        Ok n -> n
        _ -> default


collect : List I64, Zip, I64 -> List I64
collect = \list, zip, len ->
    collectHelper list zip len []


collectHelper : List I64, Zip, I64, List I64 -> List I64
collectHelper = \list, zip, len, acc ->
    if len > 1 then
        newAcc = List.append acc zip.val
        newZip = forward zip list
        newLen = len - 1
        collectHelper list newZip newLen newAcc
    else if len == 1 then
        List.append acc zip.val
    else
        acc
