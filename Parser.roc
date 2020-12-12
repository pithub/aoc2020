interface Parser
    exposes
        [ Parser, Res, State, initial, repeated
        , singleVal, fixedVals, remainingVals
        , fixedStr, remainingStr
        , fixedIgnore, remainingIgnore
        , uint, int
        ]
    imports [ ListZip ]


#  Types


State : ListZip.Zip

Res a : { val : a, state : State }

Parser a : List I64, State -> Res a


#  Building Blocks


initial : List I64 -> State
initial = \input ->
    ListZip.newAtFirst input 0


repeated : Parser a -> Parser (List a)
repeated = \parser ->
    (\input, state ->
        repeatedHelper input state parser [])


repeatedHelper : List I64, State, Parser a, List a -> Res (List a)
repeatedHelper = \input, state, parser, val ->
    if ListZip.afterLast state then
        { val, state }
    else if state.val == 10 then
        newState = ListZip.forward state input
        repeatedHelper input newState parser val
    else
        res = parser input state
        newVal = List.join [ val, [ res.val ] ]
        repeatedHelper input res.state parser newVal


singleVal : Parser I64
singleVal =
    (\input, state ->
        newState = ListZip.forward state input
        { val: state.val, state: newState }
    )


fixedVals : I64 -> Parser (List I64)
fixedVals = \len ->
    (\input, state ->
        fixedValsHelper input state len [])


fixedValsHelper : List I64, State, I64, List I64 -> Res (List I64)
fixedValsHelper = \input, state, len, val ->
    if len > 0 then
        newState = ListZip.forward state input
        newLen = len - 1
        newVal = List.append val state.val
        fixedValsHelper input newState newLen newVal
    else
        { val, state }


remainingVals : Parser (List I64)
remainingVals =
    (\input, state ->
        remainingValsHelper input state [])


remainingValsHelper : List I64, State, List I64 -> Res (List I64)
remainingValsHelper = \input, state, val ->
    if ListZip.afterLast state then
        { val, state }
    else
        newState = ListZip.forward state input
        if state.val == 10 then
            { val, state: newState }
        else
            newVal = List.append val state.val
            remainingValsHelper input newState newVal


#  Parse Strings as Hashes


fixedStr : I64 -> Parser I64
fixedStr = \len ->
    (\input, state ->
        fixedStrHelper input state len 5381)


fixedStrHelper : List I64, State, I64, I64 -> Res I64
fixedStrHelper = \input, state, len, val ->
    if len > 0 then
        newState = ListZip.forward state input
        newLen = len - 1
        newVal = val * 33 + state.val
        fixedStrHelper input newState newLen newVal
    else
        { val, state }


remainingStr : Parser I64
remainingStr =
    (\input, state ->
        remainingStrHelper input state 5381)


remainingStrHelper : List I64, State, I64 -> Res I64
remainingStrHelper = \input, state, val ->
    if ListZip.afterLast state then
        { val, state }
    else
        newState = ListZip.forward state input
        if state.val == 10 then
            { val, state: newState }
        else
            newVal = val * 33 + state.val
            remainingStrHelper input newState newVal


#  Skipping Parsers


fixedIgnore : I64 -> Parser I64
fixedIgnore = \len ->
    (\input, state ->
        newState = ListZip.move state input len
        { val: 0, state: newState }
    )


remainingIgnore : Parser I64
remainingIgnore =
    (\input, state ->
        remainingIgnoreHelper input state)


remainingIgnoreHelper : List I64, State -> Res I64
remainingIgnoreHelper = \input, state ->
    if ListZip.afterLast state then
        { val: 0, state }
    else
        newState = ListZip.forward state input
        if state.val == 10 then
            { val: 0, state: newState }
        else
            remainingIgnoreHelper input newState


#  Integer Parsers


uint : Parser I64
uint =
    (\input, state ->
        uintHelper input state 0)


uintHelper : List I64, State, I64 -> Res I64
uintHelper = \input, state, val ->
    if 48 <= state.val && state.val <= 57 then
        newState = ListZip.forward state input
        newVal = val * 10 + state.val - 48
        uintHelper input newState newVal
    else
        { val, state }


int : Parser I64
int =
    (\input, state ->
        if state.val == 45 then
            newState = ListZip.forward state input
            res = uint input newState
            { res & val: -res.val }
        else
            uint input state
    )
