interface Day02 exposes [ output ] imports [ Parser, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testLines   = parseLines testInput
    puzzleLines = parseLines puzzleInput

    [ TestUtil.verify 2 1 1 (validCount isValid1 testLines  ) 2
    , TestUtil.show   2 1   (validCount isValid1 puzzleLines)
    , TestUtil.verify 2 2 1 (validCount isValid2 testLines  ) 1
    , TestUtil.show   2 2   (validCount isValid2 puzzleLines)
    ]


Line : { num1 : I64, num2 : I64, val : I64, vals : List I64 }


#  count valid passwords


validCount : (Line -> Bool), List Line -> I64
validCount = \predicate, lines ->
    List.keepIf lines predicate |> List.len


isValid1 : Line -> Bool
isValid1 = \line ->
    given = countMember line.vals line.val
    (line.num1 <= given) && (given <= line.num2)


countMember : List I64, I64 -> I64
countMember = \list, element ->
    mapper = \e -> if e == element then 1 else 0
    list |> List.map mapper |> List.sum


isValid2 : Line -> Bool
isValid2 = \line ->
    val1 = getPasswordChar line.vals line.num1
    val2 = getPasswordChar line.vals line.num2

    (val1 == line.val) != (val2 == line.val)


getPasswordChar : List I64, I64 -> I64
getPasswordChar = \list, position ->
    when List.get list (position - 1) is
        Ok n -> n
        _ -> 0


#  parse input


parseLines : List I64 -> List Line
parseLines = \input ->
    init = Parser.initial input
    linesParser = Parser.repeated lineParser
    res = linesParser input init
    res.val


lineParser : Parser.Parser Line
lineParser =
    (\input, state ->
        pUint = Parser.uint
        pIgn1 = Parser.fixedIgnore 1
        pIgn2 = Parser.fixedIgnore 2
        pVal1 = Parser.singleVal
        pVals = Parser.remainingVals

        num1 = pUint input state
        ign1 = pIgn1 input num1.state
        num2 = pUint input ign1.state
        ign2 = pIgn1 input num2.state
        val1 = pVal1 input ign2.state
        ign3 = pIgn2 input val1.state
        vals = pVals input ign3.state

        line = { num1: num1.val, num2: num2.val, val: val1.val, vals: vals.val }
        { val: line, state: vals.state }
    )


#  test data


testInput : List I64
testInput =
    [ 49, 45, 51, 32, 97, 58, 32, 97, 98, 99, 100, 101, 10
    , 49, 45, 51, 32, 98, 58, 32, 99, 100, 101, 102, 103, 10
    , 50, 45, 57, 32, 99, 58, 32, 99, 99, 99, 99, 99, 99, 99, 99, 99
    ]
