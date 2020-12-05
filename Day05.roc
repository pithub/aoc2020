interface Day05 exposes [ output ] imports [ ListExtra, TestUtil ]


output : List Int -> List (List Int)
output = \input ->
    puzzleInput = toSeats input

    [ TestUtil.verify 5 1 1 (highestBoardingPassId testInput11) 357
    , TestUtil.verify 5 1 2 (highestBoardingPassId testInput12) 567
    , TestUtil.verify 5 1 3 (highestBoardingPassId testInput13) 119
    , TestUtil.verify 5 1 4 (highestBoardingPassId testInput14) 820
    , TestUtil.show   5 1   (highestBoardingPassId puzzleInput)
    , TestUtil.show   5 2   (freeBoardingPassId    puzzleInput)
    ]


Seat : [ Seat Int Int ]


highestBoardingPassId : List Seat -> Int
highestBoardingPassId = \seats ->
    List.map seats boardingPassId |> List.walk maxId 0


boardingPassId : Seat -> Int
boardingPassId = \seat ->
    when seat is
        Seat row column -> row * 8 + column


maxId : Int, Int -> Int
maxId = \id1, id2 ->
    if id1 > id2 then id1 else id2


freeBoardingPassId : List Seat -> Int
freeBoardingPassId = \seats ->
    sortedIds = List.map seats boardingPassId |> ListExtra.quicksort
    missingIdInRow sortedIds 0


missingIdInRow : List Int, Int -> Int
missingIdInRow = \ids, idx ->
    id1 = getListElement ids idx
    id2 = getListElement ids (idx + 1)
    id3 = getListElement ids (idx + 2)
    id4 = getListElement ids (idx + 3)
    id5 = getListElement ids (idx + 4)
    id6 = getListElement ids (idx + 5)
    id7 = getListElement ids (idx + 6)

    if id2 > id1 + 1 then
        id1 + 1
    else if id3 > id2 + 1 then
        id2 + 1
    else if id4 > id3 + 1 then
        id3 + 1
    else if id5 > id4 + 1 then
        id4 + 1
    else if id6 > id5 + 1 then
        id5 + 1
    else if id7 > id6 + 1 then
        id6 + 1
    else
        missingIdInRow ids (idx + 8)


getListElement : List Int, Int -> Int
getListElement = \list, idx ->
    when List.get list idx is
        Ok n -> n
        _ -> 0


testInput11 : List Seat
testInput11 = toSeats [ 70, 66, 70, 66, 66, 70, 70, 82, 76, 82, 10 ]

testInput12 : List Seat
testInput12 = toSeats [ 66, 70, 70, 70, 66, 66, 70, 82, 82, 82, 10 ]

testInput13 : List Seat
testInput13 = toSeats [ 70, 70, 70, 66, 66, 66, 70, 82, 82, 82, 10 ]

testInput14 : List Seat
testInput14 = toSeats [ 66, 66, 70, 70, 66, 66, 70, 82, 76, 76, 10 ]


toSeats : List Int -> List Seat
toSeats = \input ->
    (List.walk input seatWalker initialBuilder).seats


SeatBuilder : { row : Int, col : Int, seats : List Seat }


initialBuilder : SeatBuilder
initialBuilder =
    { row: 0, col: 0, seats: [] }


seatWalker : Int, SeatBuilder -> SeatBuilder
seatWalker = \char, builder ->
    when char is
        70 -> { builder & row: builder.row * 2 }
        66 -> { builder & row: builder.row * 2 + 1 }
        76 -> { builder & col: builder.col * 2 }
        82 -> { builder & col: builder.col * 2 + 1 }
        10 ->
            seat = Seat builder.row builder.col
            { row: 0, col: 0, seats: List.append builder.seats seat }
        _ -> builder
