interface Day08 exposes [ output ] imports [ TestUtil ]


output : List Int -> List (List Int)
output = \puzzleInput ->
    testCode   = parseInput testInput
    puzzleCode = parseInput puzzleInput

    [ TestUtil.verify 8 1 1 (accBeforeLoop testCode  ) 5
    , TestUtil.show   8 1   (accBeforeLoop puzzleCode)
    , TestUtil.verify 8 2 1 (accAtEnd testCode  ) 8
    , TestUtil.show   8 2   (accAtEnd puzzleCode)
    ]


# code instructions


Inst : [ Nop Int, Acc Int, Jmp Int ]


# find loop in code


accBeforeLoop : List Inst -> Int
accBeforeLoop = \code ->
    initial = initialRunState code
    final = runUntilLoopOrEnd initial
    final.acc


RunState : { length : Int, code: List Inst, ip: Int, acc: Int, visited: List Int }


initialRunState : List Inst -> RunState
initialRunState = \code ->
    length = List.len code
    visited = List.repeat length 0
    { length, code, ip: 0, acc:0, visited }


runUntilLoopOrEnd : RunState -> RunState
runUntilLoopOrEnd = \state ->
    if state.ip < state.length then
        when List.get state.visited state.ip is
            Ok 0 ->
                newVisited = List.set state.visited state.ip 1
                newState = step { state & visited: newVisited }
                runUntilLoopOrEnd newState
            _ ->
                state
    else
        state


step : RunState -> RunState
step = \state ->
    when List.get state.code state.ip is
        Ok inst ->
            when inst is
                Nop _ ->
                    { state & ip: state.ip + 1 }

                Acc op ->
                    { state & ip: state.ip + 1, acc: state.acc + op }

                Jmp op ->
                    { state & ip: state.ip + op }

        _ ->
            state


# correct code and run until end


accAtEnd : List Inst -> Int
accAtEnd = \code ->
    accAtEndHelper code code 0


accAtEndHelper : List Inst, List Inst, Int -> Int
accAtEndHelper = \originalCode, codeToExecute, chg ->
    initial = initialRunState codeToExecute
    final = runUntilLoopOrEnd initial
    if final.ip < final.length then
        when List.get originalCode chg is
            Ok oldInst ->
                newInst =
                    when oldInst is
                        Nop op -> Jmp op
                        Jmp op -> Nop op
                        inst -> inst
                newCode = List.set originalCode chg newInst
                newChg = chg + 1
                accAtEndHelper originalCode newCode newChg
            _ ->
                -1
    else
        final.acc


# parse input to code instructions


parseInput : List Int -> List Inst
parseInput = \input ->
    (List.walk input parseWalker initialParseAcc).output

ParseAcc : { inst : Int, sign : Int, op : Int, output : List Inst }


initialParseAcc : ParseAcc
initialParseAcc =
    { inst: 0, sign: 0, op: 0, output: [] }


parseWalker : Int, ParseAcc -> ParseAcc
parseWalker = \val, acc ->
    if val == 10 then
        instOp = acc.sign * acc.op
        parsed =
            when acc.inst is
                97 -> Acc instOp
                106 -> Jmp instOp
                _ -> Nop instOp
        newOutput = List.append acc.output parsed
        { inst: 0, sign: 0, op: 0, output: newOutput}

    else if acc.inst == 0 then
        { acc & inst: val }

    else if acc.sign == 0 then
        { acc & sign: if val == 45 then -1 else 1 }

    else
        { acc & op: 10 * acc.op + val - 48 }


#  test data


testInput : List Int
testInput =
    [ 110, 43, 48, 10
    , 97, 43, 49, 10
    , 106, 43, 52, 10
    , 97, 43, 51, 10
    , 106, 45, 51, 10
    , 97, 45, 57, 57, 10
    , 97, 43, 49, 10
    , 106, 45, 52, 10
    , 97, 43, 54, 10
    ]
