interface Day05 exposes [ output ] imports [ ListExtra, TestUtil ]


output : List (List Int)
output =
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
testInput11 = [ Seat 0b0101100 0b101 ]

testInput12 : List Seat
testInput12 = [ Seat 0b1000110 0b111 ]

testInput13 : List Seat
testInput13 = [ Seat 0b0001110 0b111 ]

testInput14 : List Seat
testInput14 = [ Seat 0b1100110 0b100 ]


puzzleInput : List Seat
puzzleInput =
    List.join
        [ puzzleInput1
        , puzzleInput2
        , puzzleInput3
        , puzzleInput4
        , puzzleInput5
        , puzzleInput6
        , puzzleInput7
        , puzzleInput8
        , puzzleInput9
        ]


puzzleInput1 : List Seat
puzzleInput1 =
    [ Seat 0b0110101 0b011
    , Seat 0b0100000 0b001
    , Seat 0b0111111 0b001
    , Seat 0b0010111 0b100
    , Seat 0b0110000 0b011
    , Seat 0b1001000 0b000
    , Seat 0b0010000 0b101
    , Seat 0b0111101 0b101
    , Seat 0b1010011 0b110
    , Seat 0b0001111 0b000
    , Seat 0b1011011 0b000
    , Seat 0b0100001 0b100
    , Seat 0b1101001 0b111
    , Seat 0b1011100 0b101
    , Seat 0b0100010 0b100
    , Seat 0b1110000 0b000
    , Seat 0b0110101 0b111
    , Seat 0b0011100 0b111
    , Seat 0b1101110 0b000
    , Seat 0b1100101 0b010
    , Seat 0b0101100 0b001
    , Seat 0b1001110 0b010
    , Seat 0b0001110 0b010
    , Seat 0b0101010 0b101
    , Seat 0b0101001 0b001
    , Seat 0b1000001 0b100
    , Seat 0b0001110 0b011
    , Seat 0b0011010 0b001
    , Seat 0b0011110 0b110
    , Seat 0b1001001 0b101
    , Seat 0b0111100 0b010
    , Seat 0b1100100 0b011
    , Seat 0b0000110 0b101
    , Seat 0b1010110 0b010
    , Seat 0b1000110 0b001
    , Seat 0b1010011 0b000
    , Seat 0b0010001 0b111
    , Seat 0b1100011 0b000
    , Seat 0b0010100 0b101
    , Seat 0b0001100 0b110
    , Seat 0b0110110 0b011
    , Seat 0b1100101 0b110
    , Seat 0b0010011 0b111
    , Seat 0b1100101 0b111
    , Seat 0b1000101 0b010
    , Seat 0b0001101 0b100
    , Seat 0b1011010 0b010
    , Seat 0b0000011 0b111
    , Seat 0b0111111 0b111
    , Seat 0b0101000 0b011
    , Seat 0b0110001 0b000
    , Seat 0b1011001 0b100
    , Seat 0b1010011 0b001
    , Seat 0b0011000 0b000
    , Seat 0b0111010 0b100
    , Seat 0b0000100 0b010
    , Seat 0b0010100 0b010
    , Seat 0b1011101 0b101
    , Seat 0b1101001 0b110
    , Seat 0b1011101 0b011
    , Seat 0b0111101 0b110
    , Seat 0b1000111 0b010
    , Seat 0b1010010 0b010
    , Seat 0b0000011 0b100
    , Seat 0b1001101 0b000
    , Seat 0b1101110 0b110
    , Seat 0b1110000 0b111
    , Seat 0b0101100 0b100
    , Seat 0b1010100 0b101
    , Seat 0b1110110 0b110
    , Seat 0b1010110 0b101
    , Seat 0b0011011 0b110
    , Seat 0b0100001 0b010
    , Seat 0b1001110 0b101
    , Seat 0b1010000 0b111
    , Seat 0b1010001 0b111
    , Seat 0b0101100 0b101
    , Seat 0b0101101 0b000
    , Seat 0b1110010 0b100
    , Seat 0b1001111 0b000
    , Seat 0b1100001 0b101
    , Seat 0b0111111 0b010
    , Seat 0b1000010 0b110
    , Seat 0b0001101 0b001
    , Seat 0b0100111 0b111
    , Seat 0b1100001 0b111
    , Seat 0b0100011 0b000
    , Seat 0b0010100 0b100
    , Seat 0b0100001 0b000
    , Seat 0b0010001 0b001
    , Seat 0b0101101 0b011
    , Seat 0b0101001 0b000
    , Seat 0b1010110 0b011
    , Seat 0b0010100 0b001
    , Seat 0b0110000 0b010
    , Seat 0b0011001 0b101
    , Seat 0b0010110 0b111
    , Seat 0b1010010 0b111
    , Seat 0b0000101 0b000
    , Seat 0b0011001 0b001
    , Seat 0b1100110 0b000
    , Seat 0b0001001 0b111
    , Seat 0b1101000 0b010
    , Seat 0b1010101 0b011
    ]


puzzleInput2 : List Seat
puzzleInput2 =
    [ Seat 0b0011000 0b100
    , Seat 0b0010111 0b101
    , Seat 0b1101000 0b000
    , Seat 0b0100101 0b100
    , Seat 0b0010101 0b000
    , Seat 0b1100100 0b101
    , Seat 0b0110001 0b010
    , Seat 0b1010101 0b100
    , Seat 0b1001100 0b111
    , Seat 0b1110110 0b001
    , Seat 0b1101101 0b010
    , Seat 0b1110111 0b101
    , Seat 0b1110101 0b100
    , Seat 0b1100110 0b001
    , Seat 0b0011110 0b000
    , Seat 0b1110000 0b100
    , Seat 0b1101010 0b000
    , Seat 0b0011011 0b111
    , Seat 0b0010111 0b110
    , Seat 0b0010110 0b011
    , Seat 0b0110011 0b010
    , Seat 0b0001001 0b101
    , Seat 0b1110011 0b101
    , Seat 0b0101111 0b100
    , Seat 0b0010010 0b001
    , Seat 0b1000010 0b111
    , Seat 0b0011001 0b110
    , Seat 0b1110011 0b100
    , Seat 0b1110010 0b000
    , Seat 0b1101000 0b101
    , Seat 0b0010000 0b010
    , Seat 0b0100101 0b111
    , Seat 0b1010011 0b010
    , Seat 0b0111110 0b000
    , Seat 0b1010011 0b101
    , Seat 0b1010101 0b001
    , Seat 0b1110001 0b001
    , Seat 0b1011010 0b100
    , Seat 0b0010010 0b101
    , Seat 0b1010110 0b001
    , Seat 0b1100110 0b110
    , Seat 0b0101000 0b001
    , Seat 0b1101101 0b110
    , Seat 0b1010010 0b110
    , Seat 0b1011000 0b000
    , Seat 0b1110010 0b110
    , Seat 0b1110110 0b011
    , Seat 0b1011000 0b011
    , Seat 0b0110110 0b000
    , Seat 0b1010000 0b101
    , Seat 0b0011000 0b111
    , Seat 0b1110110 0b100
    , Seat 0b1110000 0b010
    , Seat 0b1111000 0b011
    , Seat 0b1001000 0b111
    , Seat 0b0011111 0b011
    , Seat 0b0110101 0b110
    , Seat 0b0011101 0b001
    , Seat 0b1100000 0b001
    , Seat 0b0100100 0b011
    , Seat 0b0110110 0b001
    , Seat 0b1010101 0b110
    , Seat 0b1000001 0b011
    , Seat 0b0001110 0b111
    , Seat 0b1100000 0b010
    , Seat 0b1010101 0b000
    , Seat 0b0010110 0b010
    , Seat 0b0010010 0b111
    , Seat 0b0111011 0b111
    , Seat 0b0010101 0b101
    , Seat 0b1001011 0b111
    , Seat 0b1000100 0b111
    , Seat 0b0001011 0b010
    , Seat 0b1110100 0b110
    , Seat 0b1101011 0b001
    , Seat 0b0100111 0b010
    , Seat 0b1010111 0b011
    , Seat 0b0101011 0b110
    , Seat 0b1000010 0b100
    , Seat 0b0111100 0b100
    , Seat 0b1100010 0b011
    , Seat 0b0111000 0b000
    , Seat 0b0000011 0b011
    , Seat 0b1110111 0b010
    , Seat 0b1101011 0b111
    , Seat 0b0101001 0b101
    , Seat 0b0001100 0b010
    , Seat 0b0110111 0b100
    , Seat 0b0010001 0b100
    , Seat 0b1011101 0b001
    , Seat 0b0100000 0b000
    , Seat 0b0010011 0b011
    , Seat 0b0101101 0b111
    , Seat 0b1101100 0b110
    , Seat 0b0110011 0b011
    , Seat 0b1001010 0b100
    , Seat 0b1000010 0b011
    , Seat 0b0001010 0b111
    , Seat 0b1100111 0b100
    , Seat 0b1011001 0b010
    , Seat 0b0011111 0b000
    , Seat 0b0110110 0b110
    , Seat 0b1011011 0b110
    , Seat 0b0001000 0b010
    ]


puzzleInput3 : List Seat
puzzleInput3 =
    [ Seat 0b0010111 0b010
    , Seat 0b0111010 0b111
    , Seat 0b1000100 0b010
    , Seat 0b1011100 0b110
    , Seat 0b0100010 0b010
    , Seat 0b0111001 0b110
    , Seat 0b0101010 0b100
    , Seat 0b0101110 0b100
    , Seat 0b1000000 0b011
    , Seat 0b0011001 0b000
    , Seat 0b1000011 0b100
    , Seat 0b0010011 0b000
    , Seat 0b0101010 0b010
    , Seat 0b0111010 0b000
    , Seat 0b1011000 0b101
    , Seat 0b1011000 0b001
    , Seat 0b1011110 0b000
    , Seat 0b0111000 0b101
    , Seat 0b0100111 0b000
    , Seat 0b1110010 0b001
    , Seat 0b0101111 0b011
    , Seat 0b0000100 0b011
    , Seat 0b0100010 0b110
    , Seat 0b1000101 0b000
    , Seat 0b1100000 0b100
    , Seat 0b0010000 0b011
    , Seat 0b0001100 0b101
    , Seat 0b0101110 0b001
    , Seat 0b1010100 0b001
    , Seat 0b0110101 0b001
    , Seat 0b0100010 0b101
    , Seat 0b0111000 0b100
    , Seat 0b1000010 0b000
    , Seat 0b0001111 0b111
    , Seat 0b1000001 0b101
    , Seat 0b0000111 0b010
    , Seat 0b0101110 0b111
    , Seat 0b1000101 0b110
    , Seat 0b0110011 0b101
    , Seat 0b1001111 0b100
    , Seat 0b0100110 0b000
    , Seat 0b0101000 0b111
    , Seat 0b0010010 0b011
    , Seat 0b0110100 0b101
    , Seat 0b0101101 0b010
    , Seat 0b0001010 0b000
    , Seat 0b0000100 0b110
    , Seat 0b1011000 0b100
    , Seat 0b1001111 0b011
    , Seat 0b1101001 0b100
    , Seat 0b1100001 0b011
    , Seat 0b1110000 0b101
    , Seat 0b1001101 0b101
    , Seat 0b1100100 0b001
    , Seat 0b0000110 0b010
    , Seat 0b0110110 0b101
    , Seat 0b1101100 0b100
    , Seat 0b1100011 0b100
    , Seat 0b1001110 0b100
    , Seat 0b1101101 0b001
    , Seat 0b1110011 0b110
    , Seat 0b0010010 0b100
    , Seat 0b1011010 0b110
    , Seat 0b1010111 0b111
    , Seat 0b0111001 0b100
    , Seat 0b1110010 0b011
    , Seat 0b0110011 0b110
    , Seat 0b0011000 0b010
    , Seat 0b1000100 0b011
    , Seat 0b0000111 0b111
    , Seat 0b0111011 0b001
    , Seat 0b1011011 0b010
    , Seat 0b0100101 0b010
    , Seat 0b0010100 0b011
    , Seat 0b1001010 0b011
    , Seat 0b1101000 0b100
    , Seat 0b0110001 0b110
    , Seat 0b1110100 0b011
    , Seat 0b1101111 0b100
    , Seat 0b1100110 0b101
    , Seat 0b1011111 0b010
    , Seat 0b1010101 0b111
    , Seat 0b1011010 0b000
    , Seat 0b1001111 0b010
    , Seat 0b0100101 0b110
    , Seat 0b1101010 0b011
    , Seat 0b0110000 0b110
    , Seat 0b1100001 0b110
    , Seat 0b0001010 0b100
    , Seat 0b0101101 0b100
    , Seat 0b0101100 0b011
    , Seat 0b0000101 0b100
    , Seat 0b0000110 0b001
    , Seat 0b0000111 0b001
    , Seat 0b0101110 0b110
    , Seat 0b0000111 0b000
    , Seat 0b1110100 0b100
    , Seat 0b0111110 0b111
    , Seat 0b1101001 0b011
    , Seat 0b1100110 0b100
    , Seat 0b1000110 0b100
    , Seat 0b1110110 0b000
    , Seat 0b1001001 0b010
    , Seat 0b1100011 0b110
    ]


puzzleInput4 : List Seat
puzzleInput4 =
    [ Seat 0b0101010 0b111
    , Seat 0b1100100 0b010
    , Seat 0b1101111 0b110
    , Seat 0b1001000 0b001
    , Seat 0b0101111 0b101
    , Seat 0b0110100 0b111
    , Seat 0b0010010 0b010
    , Seat 0b1000010 0b001
    , Seat 0b1000110 0b111
    , Seat 0b1000011 0b000
    , Seat 0b0001110 0b001
    , Seat 0b0100000 0b101
    , Seat 0b0101000 0b110
    , Seat 0b0101111 0b000
    , Seat 0b1100010 0b100
    , Seat 0b1101010 0b010
    , Seat 0b0110010 0b100
    , Seat 0b0001010 0b011
    , Seat 0b0011000 0b011
    , Seat 0b1010101 0b101
    , Seat 0b0010001 0b110
    , Seat 0b0110011 0b111
    , Seat 0b1110010 0b101
    , Seat 0b1011001 0b001
    , Seat 0b1110101 0b010
    , Seat 0b0101111 0b010
    , Seat 0b0100000 0b111
    , Seat 0b1100111 0b010
    , Seat 0b1100011 0b111
    , Seat 0b1011111 0b011
    , Seat 0b0011011 0b100
    , Seat 0b1011110 0b101
    , Seat 0b0000111 0b100
    , Seat 0b0010110 0b101
    , Seat 0b0101111 0b001
    , Seat 0b0010110 0b110
    , Seat 0b0111001 0b101
    , Seat 0b0010000 0b001
    , Seat 0b1001100 0b000
    , Seat 0b1100011 0b001
    , Seat 0b1000000 0b001
    , Seat 0b1101011 0b010
    , Seat 0b1000001 0b000
    , Seat 0b0011111 0b010
    , Seat 0b0101010 0b001
    , Seat 0b0100101 0b011
    , Seat 0b1010110 0b000
    , Seat 0b1001100 0b011
    , Seat 0b0101110 0b101
    , Seat 0b1100111 0b111
    , Seat 0b0011101 0b000
    , Seat 0b1011100 0b001
    , Seat 0b1100010 0b000
    , Seat 0b1010001 0b000
    , Seat 0b1010001 0b011
    , Seat 0b1101110 0b111
    , Seat 0b0011011 0b011
    , Seat 0b0101001 0b110
    , Seat 0b1011001 0b101
    , Seat 0b0001111 0b110
    , Seat 0b1010001 0b100
    , Seat 0b0011010 0b010
    , Seat 0b1011011 0b001
    , Seat 0b0111000 0b110
    , Seat 0b0011111 0b100
    , Seat 0b0101000 0b100
    , Seat 0b1111000 0b010
    , Seat 0b0110001 0b001
    , Seat 0b1100001 0b100
    , Seat 0b1100111 0b101
    , Seat 0b1000011 0b111
    , Seat 0b0011000 0b001
    , Seat 0b0111001 0b001
    , Seat 0b1011010 0b001
    , Seat 0b1110101 0b110
    , Seat 0b0111011 0b100
    , Seat 0b1001000 0b011
    , Seat 0b1010001 0b101
    , Seat 0b1011100 0b111
    , Seat 0b1010011 0b011
    , Seat 0b0101111 0b110
    , Seat 0b0001011 0b011
    , Seat 0b0010111 0b001
    , Seat 0b1100110 0b010
    , Seat 0b0001101 0b010
    , Seat 0b0110101 0b100
    , Seat 0b0100100 0b110
    , Seat 0b1000101 0b101
    , Seat 0b1010100 0b111
    , Seat 0b0000101 0b010
    , Seat 0b0111110 0b001
    , Seat 0b0111000 0b001
    , Seat 0b0001011 0b101
    , Seat 0b1110110 0b111
    , Seat 0b1110101 0b011
    , Seat 0b0110100 0b011
    , Seat 0b0100010 0b111
    , Seat 0b1110001 0b110
    , Seat 0b1100010 0b110
    , Seat 0b1011101 0b010
    , Seat 0b0011101 0b110
    , Seat 0b0011100 0b110
    , Seat 0b0111010 0b001
    , Seat 0b0110101 0b000
    ]


puzzleInput5 : List Seat
puzzleInput5 =
    [ Seat 0b1110001 0b011
    , Seat 0b1010010 0b100
    , Seat 0b0100111 0b011
    , Seat 0b1001001 0b000
    , Seat 0b0001110 0b100
    , Seat 0b1110001 0b100
    , Seat 0b0101110 0b000
    , Seat 0b0101011 0b010
    , Seat 0b0110011 0b000
    , Seat 0b0111101 0b100
    , Seat 0b0110101 0b010
    , Seat 0b0100001 0b110
    , Seat 0b0010101 0b110
    , Seat 0b0011110 0b111
    , Seat 0b1001110 0b110
    , Seat 0b0111101 0b010
    , Seat 0b1101110 0b010
    , Seat 0b1000011 0b010
    , Seat 0b1000110 0b101
    , Seat 0b0111111 0b100
    , Seat 0b0010011 0b101
    , Seat 0b0101010 0b011
    , Seat 0b0001011 0b100
    , Seat 0b1101111 0b111
    , Seat 0b0001111 0b011
    , Seat 0b1010111 0b010
    , Seat 0b0110110 0b010
    , Seat 0b1101010 0b001
    , Seat 0b0111011 0b101
    , Seat 0b0111111 0b101
    , Seat 0b1000100 0b110
    , Seat 0b1110001 0b000
    , Seat 0b0110111 0b101
    , Seat 0b1100110 0b111
    , Seat 0b0010001 0b000
    , Seat 0b0100110 0b101
    , Seat 0b1011111 0b110
    , Seat 0b1100000 0b111
    , Seat 0b1000001 0b111
    , Seat 0b0010101 0b100
    , Seat 0b0100110 0b011
    , Seat 0b0000011 0b110
    , Seat 0b0110001 0b011
    , Seat 0b1000100 0b000
    , Seat 0b1101100 0b000
    , Seat 0b1110110 0b101
    , Seat 0b1001011 0b000
    , Seat 0b0010000 0b100
    , Seat 0b0100111 0b101
    , Seat 0b1000111 0b101
    , Seat 0b1100111 0b011
    , Seat 0b0001111 0b100
    , Seat 0b0110100 0b000
    , Seat 0b1010111 0b000
    , Seat 0b0111110 0b010
    , Seat 0b0010011 0b010
    , Seat 0b1110001 0b111
    , Seat 0b0001010 0b001
    , Seat 0b1000111 0b011
    , Seat 0b1000111 0b110
    , Seat 0b1100000 0b000
    , Seat 0b0111110 0b110
    , Seat 0b1010011 0b111
    , Seat 0b1000000 0b111
    , Seat 0b0100101 0b101
    , Seat 0b1011110 0b001
    , Seat 0b0011101 0b111
    , Seat 0b1010000 0b110
    , Seat 0b1001001 0b100
    , Seat 0b0001001 0b011
    , Seat 0b1001100 0b110
    , Seat 0b1000010 0b101
    , Seat 0b0111110 0b101
    , Seat 0b1011111 0b000
    , Seat 0b0001011 0b111
    , Seat 0b0000101 0b110
    , Seat 0b0001111 0b101
    , Seat 0b0101001 0b011
    , Seat 0b1000110 0b010
    , Seat 0b0011000 0b101
    , Seat 0b0001101 0b011
    , Seat 0b0111101 0b001
    , Seat 0b0011101 0b011
    , Seat 0b0101100 0b110
    , Seat 0b0000101 0b011
    , Seat 0b1010000 0b000
    , Seat 0b0101011 0b100
    , Seat 0b0000100 0b101
    , Seat 0b0011111 0b001
    , Seat 0b1000100 0b100
    , Seat 0b0000101 0b001
    , Seat 0b0101010 0b110
    , Seat 0b1101110 0b011
    , Seat 0b1101110 0b001
    , Seat 0b1101100 0b010
    , Seat 0b1110000 0b110
    , Seat 0b1000011 0b101
    , Seat 0b1011000 0b110
    , Seat 0b1011100 0b000
    , Seat 0b0001100 0b100
    , Seat 0b0111101 0b011
    , Seat 0b1110010 0b111
    , Seat 0b0101001 0b010
    , Seat 0b0110100 0b110
    ]


puzzleInput6 : List Seat
puzzleInput6 =
    [ Seat 0b0001011 0b110
    , Seat 0b1011011 0b100
    , Seat 0b1011010 0b111
    , Seat 0b0001011 0b000
    , Seat 0b1001011 0b011
    , Seat 0b1011110 0b100
    , Seat 0b1001011 0b001
    , Seat 0b0000100 0b001
    , Seat 0b0011011 0b010
    , Seat 0b1010000 0b001
    , Seat 0b0100110 0b100
    , Seat 0b0001110 0b000
    , Seat 0b1110000 0b011
    , Seat 0b1010110 0b111
    , Seat 0b0011010 0b100
    , Seat 0b1101111 0b101
    , Seat 0b1010101 0b010
    , Seat 0b1110011 0b011
    , Seat 0b1100100 0b100
    , Seat 0b0010100 0b111
    , Seat 0b0011001 0b111
    , Seat 0b1101011 0b011
    , Seat 0b1001001 0b001
    , Seat 0b0100100 0b100
    , Seat 0b0010000 0b000
    , Seat 0b0101000 0b010
    , Seat 0b0100110 0b001
    , Seat 0b1001001 0b011
    , Seat 0b0100000 0b100
    , Seat 0b1010100 0b100
    , Seat 0b0010001 0b011
    , Seat 0b1011110 0b011
    , Seat 0b1001111 0b111
    , Seat 0b0100111 0b100
    , Seat 0b1110111 0b110
    , Seat 0b0001100 0b011
    , Seat 0b0000101 0b101
    , Seat 0b1101110 0b101
    , Seat 0b0111100 0b110
    , Seat 0b0100010 0b001
    , Seat 0b1001001 0b110
    , Seat 0b1101000 0b011
    , Seat 0b0001001 0b001
    , Seat 0b0111011 0b010
    , Seat 0b0101011 0b001
    , Seat 0b1000100 0b001
    , Seat 0b0000011 0b101
    , Seat 0b1100100 0b110
    , Seat 0b1010111 0b101
    , Seat 0b0111001 0b111
    , Seat 0b1101101 0b101
    , Seat 0b0101100 0b010
    , Seat 0b1111000 0b000
    , Seat 0b1100111 0b000
    , Seat 0b0101100 0b111
    , Seat 0b1100100 0b000
    , Seat 0b0111000 0b010
    , Seat 0b1001000 0b010
    , Seat 0b0110001 0b101
    , Seat 0b0110111 0b010
    , Seat 0b0010110 0b000
    , Seat 0b0100100 0b111
    , Seat 0b1000000 0b000
    , Seat 0b1101101 0b100
    , Seat 0b1000101 0b100
    , Seat 0b0100101 0b000
    , Seat 0b0010010 0b110
    , Seat 0b1100101 0b001
    , Seat 0b0111010 0b101
    , Seat 0b1010100 0b110
    , Seat 0b1010001 0b001
    , Seat 0b0011011 0b101
    , Seat 0b1101011 0b110
    , Seat 0b0010100 0b110
    , Seat 0b1001101 0b111
    , Seat 0b1000011 0b001
    , Seat 0b1010011 0b100
    , Seat 0b1011001 0b111
    , Seat 0b0101101 0b110
    , Seat 0b0000110 0b111
    , Seat 0b1000101 0b011
    , Seat 0b0100010 0b000
    , Seat 0b1101111 0b000
    , Seat 0b1100001 0b001
    , Seat 0b0001100 0b111
    , Seat 0b0011101 0b010
    , Seat 0b1011000 0b111
    , Seat 0b1101001 0b001
    , Seat 0b0011001 0b010
    , Seat 0b0010001 0b010
    , Seat 0b1101100 0b101
    , Seat 0b0011000 0b110
    , Seat 0b1100011 0b010
    , Seat 0b0001010 0b010
    , Seat 0b0011011 0b001
    , Seat 0b0100100 0b101
    , Seat 0b1101010 0b100
    , Seat 0b0001001 0b000
    , Seat 0b1000110 0b110
    , Seat 0b0101000 0b000
    , Seat 0b1100110 0b011
    , Seat 0b1001101 0b010
    , Seat 0b0011010 0b101
    , Seat 0b1110001 0b010
    ]


puzzleInput7 : List Seat
puzzleInput7 =
    [ Seat 0b0101111 0b111
    , Seat 0b1011101 0b000
    , Seat 0b1011001 0b000
    , Seat 0b1000100 0b101
    , Seat 0b1110100 0b101
    , Seat 0b0111001 0b000
    , Seat 0b0000100 0b100
    , Seat 0b0001001 0b110
    , Seat 0b1010111 0b110
    , Seat 0b0110110 0b100
    , Seat 0b1101111 0b001
    , Seat 0b1001110 0b001
    , Seat 0b0010111 0b111
    , Seat 0b0010001 0b101
    , Seat 0b1011001 0b110
    , Seat 0b1110100 0b001
    , Seat 0b0111001 0b011
    , Seat 0b0101100 0b000
    , Seat 0b0101000 0b101
    , Seat 0b0110000 0b100
    , Seat 0b0011010 0b110
    , Seat 0b1101111 0b010
    , Seat 0b0111111 0b000
    , Seat 0b0001000 0b101
    , Seat 0b0110000 0b000
    , Seat 0b0001101 0b111
    , Seat 0b1100101 0b011
    , Seat 0b0010011 0b001
    , Seat 0b0111001 0b010
    , Seat 0b1101111 0b011
    , Seat 0b0010100 0b000
    , Seat 0b0111101 0b111
    , Seat 0b0011100 0b001
    , Seat 0b0100010 0b011
    , Seat 0b1001011 0b101
    , Seat 0b0101001 0b111
    , Seat 0b1100111 0b001
    , Seat 0b1010110 0b110
    , Seat 0b0100001 0b001
    , Seat 0b0011010 0b111
    , Seat 0b1010000 0b100
    , Seat 0b0000110 0b100
    , Seat 0b0110111 0b011
    , Seat 0b0111100 0b000
    , Seat 0b0110001 0b111
    , Seat 0b0111010 0b011
    , Seat 0b1001011 0b100
    , Seat 0b0100001 0b111
    , Seat 0b1011111 0b101
    , Seat 0b1110011 0b001
    , Seat 0b1001000 0b100
    , Seat 0b1101001 0b101
    , Seat 0b1000101 0b111
    , Seat 0b0110010 0b011
    , Seat 0b1000000 0b101
    , Seat 0b0110010 0b000
    , Seat 0b1110100 0b111
    , Seat 0b0110011 0b001
    , Seat 0b0110000 0b101
    , Seat 0b0001000 0b111
    , Seat 0b1000000 0b110
    , Seat 0b0001000 0b011
    , Seat 0b1000001 0b110
    , Seat 0b1000111 0b100
    , Seat 0b1011110 0b110
    , Seat 0b1110110 0b010
    , Seat 0b1110011 0b010
    , Seat 0b0101011 0b011
    , Seat 0b1100010 0b010
    , Seat 0b1010000 0b011
    , Seat 0b0101101 0b001
    , Seat 0b1101010 0b110
    , Seat 0b0011101 0b101
    , Seat 0b0011010 0b000
    , Seat 0b0110101 0b101
    , Seat 0b0101110 0b011
    , Seat 0b1010111 0b001
    , Seat 0b0011110 0b100
    , Seat 0b0001111 0b010
    , Seat 0b1010111 0b100
    , Seat 0b1011011 0b111
    , Seat 0b1011111 0b111
    , Seat 0b0010110 0b001
    , Seat 0b0111010 0b010
    , Seat 0b1100010 0b101
    , Seat 0b0111011 0b011
    , Seat 0b1100000 0b101
    , Seat 0b1010000 0b010
    , Seat 0b1010010 0b101
    , Seat 0b0111110 0b100
    , Seat 0b0001010 0b101
    , Seat 0b1010100 0b010
    , Seat 0b0100101 0b001
    , Seat 0b1001111 0b101
    , Seat 0b0100011 0b001
    , Seat 0b0110100 0b001
    , Seat 0b0110000 0b001
    , Seat 0b1011101 0b111
    , Seat 0b0011100 0b000
    , Seat 0b0011001 0b100
    , Seat 0b0110111 0b110
    , Seat 0b1101000 0b110
    , Seat 0b0011110 0b001
    , Seat 0b0110001 0b100
    ]


puzzleInput8 : List Seat
puzzleInput8 =
    [ Seat 0b0001111 0b001
    , Seat 0b0100000 0b010
    , Seat 0b0000101 0b111
    , Seat 0b0101011 0b101
    , Seat 0b1001101 0b011
    , Seat 0b0000100 0b111
    , Seat 0b1101100 0b111
    , Seat 0b1101010 0b111
    , Seat 0b0100000 0b110
    , Seat 0b1000010 0b010
    , Seat 0b1000000 0b100
    , Seat 0b1010010 0b011
    , Seat 0b1100101 0b101
    , Seat 0b0001110 0b101
    , Seat 0b0011011 0b000
    , Seat 0b0111101 0b000
    , Seat 0b0011100 0b011
    , Seat 0b0010011 0b100
    , Seat 0b1001101 0b110
    , Seat 0b0010101 0b011
    , Seat 0b0010110 0b100
    , Seat 0b0110111 0b000
    , Seat 0b0101101 0b101
    , Seat 0b1100011 0b011
    , Seat 0b0001000 0b110
    , Seat 0b0011101 0b100
    , Seat 0b0111010 0b110
    , Seat 0b0100011 0b101
    , Seat 0b0001000 0b000
    , Seat 0b1011011 0b011
    , Seat 0b1001010 0b110
    , Seat 0b0011110 0b010
    , Seat 0b1110010 0b010
    , Seat 0b0100110 0b010
    , Seat 0b0111000 0b011
    , Seat 0b1100111 0b110
    , Seat 0b0011010 0b011
    , Seat 0b1001111 0b110
    , Seat 0b1100101 0b100
    , Seat 0b1100000 0b110
    , Seat 0b0100011 0b111
    , Seat 0b0001011 0b001
    , Seat 0b0010101 0b010
    , Seat 0b0100100 0b001
    , Seat 0b0000111 0b110
    , Seat 0b1011100 0b010
    , Seat 0b1110111 0b000
    , Seat 0b1101000 0b001
    , Seat 0b0011100 0b101
    , Seat 0b1001010 0b111
    , Seat 0b0111111 0b110
    , Seat 0b1110111 0b111
    , Seat 0b0100011 0b100
    , Seat 0b1101001 0b010
    , Seat 0b1000000 0b010
    , Seat 0b1010100 0b000
    , Seat 0b0011110 0b101
    , Seat 0b1001010 0b010
    , Seat 0b0110100 0b010
    , Seat 0b1011000 0b010
    , Seat 0b0001001 0b100
    , Seat 0b1011101 0b110
    , Seat 0b0010101 0b001
    , Seat 0b1001011 0b110
    , Seat 0b0111100 0b011
    , Seat 0b1000111 0b001
    , Seat 0b0100011 0b110
    , Seat 0b0110100 0b100
    , Seat 0b1101101 0b111
    , Seat 0b1000011 0b011
    , Seat 0b0000111 0b101
    , Seat 0b0110010 0b101
    , Seat 0b1000110 0b011
    , Seat 0b1001100 0b101
    , Seat 0b1001011 0b010
    , Seat 0b0100111 0b001
    , Seat 0b1011100 0b011
    , Seat 0b1000001 0b010
    , Seat 0b1110101 0b101
    , Seat 0b1010001 0b010
    , Seat 0b1101100 0b001
    , Seat 0b0011001 0b011
    , Seat 0b1101101 0b011
    , Seat 0b1000001 0b001
    , Seat 0b1001100 0b010
    , Seat 0b1001111 0b001
    , Seat 0b0111000 0b111
    , Seat 0b1110000 0b001
    , Seat 0b1001100 0b100
    , Seat 0b0111100 0b111
    , Seat 0b0010111 0b011
    , Seat 0b0111011 0b110
    , Seat 0b1000110 0b000
    , Seat 0b0100011 0b011
    , Seat 0b0110000 0b111
    , Seat 0b1101010 0b101
    , Seat 0b0100001 0b101
    , Seat 0b0011100 0b010
    , Seat 0b0000111 0b011
    , Seat 0b1001101 0b100
    , Seat 0b0110110 0b111
    , Seat 0b1110101 0b000
    , Seat 0b0101001 0b100
    , Seat 0b1111000 0b001
    ]


puzzleInput9 : List Seat
puzzleInput9 =
    [ Seat 0b1001110 0b000
    , Seat 0b0110111 0b111
    , Seat 0b1100000 0b011
    , Seat 0b1001010 0b001
    , Seat 0b1110011 0b000
    , Seat 0b1110111 0b001
    , Seat 0b1100001 0b010
    , Seat 0b0111100 0b001
    , Seat 0b0001101 0b000
    , Seat 0b0110010 0b111
    , Seat 0b0111100 0b101
    , Seat 0b0101010 0b000
    , Seat 0b1110111 0b011
    , Seat 0b0001010 0b110
    , Seat 0b1000111 0b111
    , Seat 0b1110101 0b001
    , Seat 0b0101011 0b000
    , Seat 0b1101001 0b000
    , Seat 0b0010101 0b111
    , Seat 0b1101011 0b100
    , Seat 0b0100001 0b011
    , Seat 0b1100011 0b101
    , Seat 0b0110010 0b001
    , Seat 0b1101110 0b100
    , Seat 0b0001100 0b001
    , Seat 0b0110010 0b010
    , Seat 0b1100010 0b111
    , Seat 0b0111111 0b011
    , Seat 0b1110111 0b100
    , Seat 0b1101000 0b111
    , Seat 0b1000011 0b110
    , Seat 0b0001101 0b101
    , Seat 0b1001001 0b111
    , Seat 0b1011010 0b011
    , Seat 0b0011111 0b111
    , Seat 0b1000101 0b001
    , Seat 0b1001000 0b101
    , Seat 0b1001010 0b101
    , Seat 0b0001110 0b110
    , Seat 0b0001001 0b010
    , Seat 0b0000110 0b000
    , Seat 0b1011011 0b101
    , Seat 0b0011110 0b011
    , Seat 0b1101011 0b000
    , Seat 0b1011111 0b100
    , Seat 0b1011100 0b100
    , Seat 0b0111110 0b011
    , Seat 0b0100110 0b111
    , Seat 0b0100111 0b110
    , Seat 0b1011110 0b010
    , Seat 0b1001101 0b001
    , Seat 0b0100011 0b010
    , Seat 0b0110111 0b001
    , Seat 0b1011001 0b011
    , Seat 0b0110010 0b110
    , Seat 0b0011111 0b101
    , Seat 0b0010011 0b110
    , Seat 0b0110011 0b100
    , Seat 0b0000110 0b110
    , Seat 0b1100010 0b001
    , Seat 0b1101100 0b011
    , Seat 0b1011111 0b001
    , Seat 0b0100000 0b011
    , Seat 0b0010010 0b000
    , Seat 0b1010010 0b000
    , Seat 0b1110011 0b111
    , Seat 0b0101110 0b010
    , Seat 0b0101011 0b111
    , Seat 0b1110001 0b101
    , Seat 0b1110101 0b111
    , Seat 0b0000100 0b000
    , Seat 0b1010010 0b001
    , Seat 0b1010001 0b110
    , Seat 0b0010111 0b000
    , Seat 0b1000111 0b000
    , Seat 0b1011110 0b111
    , Seat 0b1101101 0b000
    , Seat 0b0011111 0b110
    , Seat 0b1110100 0b000
    , Seat 0b1001000 0b110
    , Seat 0b0001000 0b100
    , Seat 0b1010110 0b100
    , Seat 0b1011010 0b101
    , Seat 0b0001100 0b000
    , Seat 0b0011100 0b100
    , Seat 0b0010000 0b110
    , Seat 0b1010100 0b011
    , Seat 0b1101011 0b101
    , Seat 0b1100001 0b000
    , Seat 0b1001100 0b001
    , Seat 0b0001101 0b110
    , Seat 0b1001110 0b111
    , Seat 0b1001110 0b011
    , Seat 0b1011101 0b100
    , Seat 0b0100110 0b110
    , Seat 0b1100100 0b111
    , Seat 0b1100101 0b000
    , Seat 0b0001000 0b001
    , Seat 0b0100100 0b010
    , Seat 0b0100100 0b000
    , Seat 0b0000110 0b011
    , Seat 0b1110100 0b010
    , Seat 0b0111011 0b000
    , Seat 0b0010000 0b111
    ]