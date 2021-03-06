interface Day06 exposes [ output ] imports [ TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    [ TestUtil.verify 6 1 1 (groupSum Any testInput1 ) 6
    , TestUtil.verify 6 1 2 (groupSum Any testInput2 ) 11
    , TestUtil.show   6 1   (groupSum Any puzzleInput)
    , TestUtil.verify 6 2 1 (groupSum All testInput2 ) 6
    , TestUtil.show   6 2   (groupSum All puzzleInput)
    ]


Mode : [ Any, All ]


groupSum : Mode, List I64 -> I64
groupSum = \mode, input ->
    initial = initialAcc mode
    last = List.walk input groupSumWalker initial
    final = finishGroup last
    final.sum


Acc : { sum : I64, answers : List I64, passengers : I64, inGroup : Bool, mode : Mode }


initialAcc : Mode -> Acc
initialAcc = \mode ->
    { sum: 0, answers: initialAnswers {}, passengers : 0, inGroup: False, mode }


initialAnswers : {} -> List I64
initialAnswers = \_ ->
    [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]


groupSumWalker : I64, Acc -> Acc
groupSumWalker = \val, acc ->
    if val == 10 then
        if acc.inGroup then
            finishPassenger acc
        else
            finishGroup acc
    else
        addAnswer acc val


addAnswer : Acc, I64 -> Acc
addAnswer = \acc, val ->
    index = val - 97

    newCount =
        when List.get acc.answers index is
            Ok oldCount -> oldCount + 1
            _ -> 0

    newAnswers =
        List.set acc.answers index newCount

    { acc & answers: newAnswers, inGroup: True }


finishPassenger : Acc -> Acc
finishPassenger = \acc ->
    { acc & passengers: acc.passengers + 1, inGroup: False }


finishGroup : Acc -> Acc
finishGroup = \acc ->
    refCount =
        when acc.mode is
            Any -> 1
            All -> acc.passengers

    validAnswers =
        acc.answers
            |> List.map (\count -> if count >= refCount then 1 else 0)
            |> List.sum

    { sum: acc.sum + validAnswers
    , answers: initialAnswers {}
    , passengers: 0
    , inGroup: False
    , mode: acc.mode
    }


testInput1 : List I64
testInput1 =
    [ 97, 98, 99, 120, 10
    , 97, 98, 99, 121, 10
    , 97, 98, 99, 122, 10
    ]


testInput2 : List I64
testInput2 =
    [ 97, 98, 99, 10
    , 10
    , 97, 10
    , 98, 10
    , 99, 10
    , 10
    , 97, 98, 10
    , 97, 99, 10
    , 10
    , 97, 10
    , 97, 10
    , 97, 10
    , 97, 10
    , 10
    , 98, 10
    ]
