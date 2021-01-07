interface Day25 exposes [ output ] imports [ TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData   = parseData testInput
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 25 1 1 (firstResult testData  ) 14897079
    , TestUtil.show   25 1   (firstResult puzzleData)
    ]


#  first part


firstResult : [ Pair I64 I64 ] -> I64
firstResult = \data ->
    when data is
        Pair key1 key2 ->
            size = loopSize 7 key1 1 0
            transform key2 1 size


loopSize : I64, I64, I64, I64 -> I64
loopSize = \subject, publicKey, value, size ->
    if value == publicKey then
        size
    else
        when (value * subject) % 20201227 is
            Ok newValue ->
                loopSize subject publicKey newValue (size + 1)
            _ ->
                0


transform : I64, I64, I64 -> I64
transform = \subject, value, size ->
    if size > 0 then
        when (value * subject) % 20201227 is
            Ok newValue ->
                transform subject newValue (size - 1)
            _ ->
                0
    else
        value


#  parser


parseData : List I64 -> [ Pair I64 I64 ]
parseData = \input ->
    parseDataHelper input 0 0 -1


parseDataHelper : List I64, I64, I64, I64 -> [ Pair I64 I64 ]
parseDataHelper = \input, pos, num1, num2 ->
    when List.get input pos is
        Ok 10 ->
            parseDataHelper input (pos + 1) num1 0
        Ok val ->
            if num2 < 0 then
                parseDataHelper input (pos + 1) (num1 * 10 + val - 48) num2
            else
                parseDataHelper input (pos + 1) num1 (num2 * 10 + val - 48)
        _ ->
            Pair num1 num2


#  test data


testInput : List I64
testInput =
    [ 53, 55, 54, 52, 56, 48, 49, 10
    , 49, 55, 56, 48, 55, 55, 50, 52
    ]
