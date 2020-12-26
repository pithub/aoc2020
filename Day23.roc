interface Day23 exposes [ output ] imports [ TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData   = parseData testInput
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 23 1 1 (firstResult testData  ) 67384529
    , TestUtil.show   23 1   (firstResult puzzleData)
    , TestUtil.verify 23 2 1 (secondResult testData  ) 149245887792
    , TestUtil.show   23 2   (secondResult puzzleData)
    ]


#  first part


firstResult : List I64 -> I64
firstResult = \cups ->
    len = List.len cups
    result = repeatMoves cups len 100
    joinCups result


repeatMoves : List I64, I64, I64 -> List I64
repeatMoves = \cups, len, moves ->
    if moves > 0 then
        newCups = move cups len
        newMoves = moves - 1
        repeatMoves newCups len newMoves
    else
        cups


move : List I64, I64 -> List I64
move = \cups, len ->
    current = safeGet cups 0
    taken1 = safeGet cups 1
    taken2 = safeGet cups 2
    taken3 = safeGet cups 3
    destination = destinationCup len (current - 1) taken1 taken2 taken3
    newCups = List.repeat len 0
    addCup newCups cups len current destination taken1 taken2 taken3 4 0


addCup : List I64, List I64, I64, I64, I64, I64, I64, I64, I64, I64 -> List I64
addCup = \newCups, cups, len, current, destination, taken1, taken2, taken3, from, to ->
    if to < len - 1 then
        newFrom = from + 1

        val = safeGet cups from
        if val == destination then
            newNewCups = newCups
                |> List.set to val
                |> List.set (to + 1) taken1
                |> List.set (to + 2) taken2
                |> List.set (to + 3) taken3
            newTo = to + 4
            addCup newNewCups cups len current destination taken1 taken2 taken3 newFrom newTo
        else
            newNewCups = List.set newCups to val
            newTo = to + 1
            addCup newNewCups cups len current destination taken1 taken2 taken3 newFrom newTo
    else
        List.set newCups to current


joinCups : List I64 -> I64
joinCups = \cups ->
    idx = (indexOf cups 1 0) + 1
    joinCupsHelper cups idx 0


joinCupsHelper : List I64, I64, I64 -> I64
joinCupsHelper = \cups, idx, result ->
    when List.get cups idx is
        Ok cup ->
            if cup == 1 then
                result
            else
                newIdx = idx + 1
                newResult = 10 * result + cup
                joinCupsHelper cups newIdx newResult
        _ ->
            joinCupsHelper cups 0 result


indexOf : List I64, I64, I64 -> I64
indexOf = \cups, cup, idx ->
    if safeGet cups idx == cup then
        idx
    else
        newIdx = idx + 1
        indexOf cups cup newIdx


#  second part


secondResult : List I64 -> I64
secondResult = \cups ->
    total = 1000000
    succs = cupSuccs cups total
    final = repeatMoveSucc succs total 10000000
    succ1 = safeGet final 1
    succ2 = safeGet final succ1
    succ1 * succ2


cupSuccs : List I64, I64 -> List I64
cupSuccs = \cups, total ->
    len = List.len cups
    succs = List.repeat (total + 1) 0
    cupSuccsHelper cups len succs total 0 |> setCurrent (safeGet cups 0)


cupSuccsHelper : List I64, I64, List I64, I64, I64 -> List I64
cupSuccsHelper = \cups, len, succs, total, idx ->
    newIdx = idx + 1
    if newIdx >= total then
        cup = newIdx
        succ = safeGet cups 0
        List.set succs cup succ
    else if newIdx > len then
        cup = newIdx
        succ = newIdx + 1
        newSuccs = List.set succs cup succ
        cupSuccsHelper cups len newSuccs total newIdx
    else if newIdx == len then
        cup = safeGet cups idx
        succ = newIdx + 1
        newSuccs = List.set succs cup succ
        cupSuccsHelper cups len newSuccs total newIdx
    else
        cup = safeGet cups idx
        succ = safeGet cups newIdx
        newSuccs = List.set succs cup succ
        cupSuccsHelper cups len newSuccs total newIdx


repeatMoveSucc : List I64, I64, I64 -> List I64
repeatMoveSucc = \succs, len, moves ->
    if moves > 0 then
        newSuccs = moveSucc succs len
        newMoves = moves - 1
        repeatMoveSucc newSuccs len newMoves
    else
        succs


moveSucc : List I64, I64 -> List I64
moveSucc = \succs, len ->
    current = safeGet succs 0
    taken1 = safeGet succs current
    taken2 = safeGet succs taken1
    taken3 = safeGet succs taken2
    newCurrent = safeGet succs taken3

    destination = destinationCup len (current - 1) taken1 taken2 taken3
    destSucc = safeGet succs destination

    succs
        |> List.set destination taken1
        |> List.set taken3 destSucc
        |> List.set current newCurrent
        |> setCurrent newCurrent


setCurrent : List I64, I64 -> List I64
setCurrent = \cups, current ->
    List.set cups 0 current


#  common


destinationCup : I64, I64, I64, I64, I64 -> I64
destinationCup = \len, destination, taken1, taken2, taken3 ->
    if destination < 1 then
        destinationCup len len taken1 taken2 taken3
    else if destination == taken1 || destination == taken2 || destination == taken3 then
        destinationCup len (destination - 1) taken1 taken2 taken3
    else
        destination


safeGet : List I64, I64 -> I64
safeGet = \list, idx ->
    when List.get list idx is
        Ok n -> n
        _ -> 0


#  parser


parseData : List I64 -> List I64
parseData = \input ->
    List.map input (\c -> c - 48)


#  test data


testInput : List I64
testInput =
    [ 51, 56, 57, 49, 50, 53, 52, 54, 55 ]
