interface Day19 exposes [ output ] imports [ ListZip ]


output : List I64 -> List (List I64)
output = \_puzzleInput ->
    testData   = parseData testInput
    #puzzleData = parseData puzzleInput

    #[ TestUtil.verify 19 1 1 (firstResult testData  ) 2
    #, TestUtil.show   19 1   (firstResult puzzleData)
    #, TestUtil.verify 19 2 1 (secondResult testData  ) 0
    #, TestUtil.show   19 2   (secondResult puzzleData)
    #]

    when List.get testData.msgs 0 is
        Ok msg ->
            zip = ListZip.newAtFirst msg 0
            res = checkRuleNum testData zip msg 4
            [ [ 4, res.zip.idx, res.zip.val, if res.val then 1 else 0 ] ]
        _ ->
            [ [ 0 ] ]


Rule : { num : I64, type : I64, char : I64, rules1 : List I64, rules2 : List I64  }

Message : List I64

PuzzleData : { rules : List Rule, ruleIdxs : List I64, msgs : List Message }


#  first part


#firstResult : PuzzleData -> I64
#firstResult = \data ->
#    valid = firstResultHelper data 0 []
#    List.len valid


#firstResultHelper : PuzzleData, I64, List Message -> List Message
#firstResultHelper = \data, idx, result ->
#    when List.get data.msgs idx is
#        Ok msg ->
#            newIdx = idx + 1
#            newResult =
#                if isValid data msg then
#                    List.join [ result, [ msg ] ]
#                else
#                    result
#            firstResultHelper data newIdx newResult
#        _ ->
#            result


#isValid : PuzzleData, Message -> Bool
#isValid = \data, msg ->
#    zip = ListZip.newAtFirst msg 0
#    checked = checkRuleNum data zip msg 0
#    checked.val && ListZip.afterLast checked.zip


checkRuleNum : PuzzleData, ListZip.Zip, Message, I64 -> Res Bool
checkRuleNum = \data, zip, msg, ruleNum ->
    when List.get data.ruleIdxs ruleNum is
        Ok ruleIdx -> checkRuleIdx data zip msg ruleIdx
        _ -> { zip, val: False }


checkRuleIdx : PuzzleData, ListZip.Zip, Message, I64 -> Res Bool
checkRuleIdx = \data, zip, msg, ruleIdx ->
    when List.get data.rules ruleIdx is
        Ok rule ->
            when rule.type is
                1 ->
                    #checkSequence data zip msg rule.rules1 0
                    { zip, val: False }
                2 ->
                    #res1 = checkSequence data zip msg rule.rules1 0
                    #if res1.val then
                    #    res1
                    #else
                    #    checkSequence data zip msg rule.rules2 0
                    { zip, val: False }
                3 ->
                    checkChar zip msg rule.char
                _ ->
                    { zip, val: False }
        _ ->
            { zip, val: False }


#checkSequence : PuzzleData, ListZip.Zip, Message, List I64, I64 -> Res Bool
#checkSequence = \data, zip, msg, seq, seqIdx ->
#    when List.get seq seqIdx is
#        Ok ruleNum ->
#            res = checkRuleNum data zip msg ruleNum
#            if res.val then
#                newSeqIdx = seqIdx + 1
#                checkSequence data res.zip msg seq newSeqIdx
#            else
#                res
#        _ ->
#            { zip, val: True }


checkChar : ListZip.Zip, Message, I64 -> Res Bool
checkChar = \zip, msg, ruleChar ->
    if ruleChar > 0 && zip.val == ruleChar then
        newZip = ListZip.forward zip msg
        { zip: newZip, val: True }
    else
        { zip, val: False }


#  second part


#secondResult : PuzzleData -> I64
#secondResult = \data ->
#    2 * List.len data.rules


#  parser


parseData : List I64 -> PuzzleData
parseData = \input ->
    start = ListZip.newAtFirst input 0
    rules = parseRules start input
    ruleIdxs = buildRuleIdxs rules.val
    msgs = parseMessages rules.zip input
    { rules: rules.val, ruleIdxs, msgs: msgs.val }


Res a : { zip : ListZip.Zip, val : a }


parseRules : ListZip.Zip, List I64 -> Res (List Rule)
parseRules = \zip, input ->
    parseRulesHelper zip input []


parseRulesHelper : ListZip.Zip, List I64, List Rule -> Res (List Rule)
parseRulesHelper = \zip, input, rules ->
    if zip.val == 10 then
        newZip = ListZip.forward zip input
        { zip: newZip, val: rules }
    else
        rule = parseRule zip input
        newRules = List.join [ rules, [ rule.val ] ]
        parseRulesHelper rule.zip input newRules


parseRule : ListZip.Zip, List I64 -> Res Rule
parseRule = \zip, input ->
    num = parseInt zip input
    col = ListZip.move num.zip input 2
    if col.val == 34 then
        char = ListZip.forward col input
        rule = { num: num.val, type: 3, char: char.val, rules1: [], rules2: [] }
        newZip = ListZip.move char input 3
        { zip: newZip, val: rule }
    else
        rule = { num: num.val, type: 1, char: 0, rules1: [], rules2: [] }
        parseRuleData col input rule []


parseRuleData : ListZip.Zip, List I64, Rule, List I64 -> Res Rule
parseRuleData = \zip, input, rule, data ->
    when zip.val is
        32 ->
            newZip = ListZip.forward zip input
            parseRuleData newZip input rule data
        124 ->
            newZip = ListZip.move zip input 2
            newRule = { rule & type: 2, rules1: data }
            parseRuleData newZip input newRule []
        10 ->
            newZip = ListZip.forward zip input
            newRule =
                if rule.type == 2 then
                    { rule & rules2: data }
                else
                    { rule & rules1: data }
            { zip: newZip, val: newRule }
        _ ->
            num = parseInt zip input
            newData = List.append data num.val
            parseRuleData num.zip input rule newData


parseInt : ListZip.Zip, List I64 -> Res I64
parseInt = \zip, input ->
    parseIntHelper zip input 0


parseIntHelper : ListZip.Zip, List I64, I64 -> Res I64
parseIntHelper = \zip, input, num ->
    if 48 <= zip.val && zip.val <= 57 then
        newZip = ListZip.forward zip input
        newNum = num * 10 + zip.val - 48
        parseIntHelper newZip input newNum
    else
        { zip, val: num }


buildRuleIdxs : List Rule -> List I64
buildRuleIdxs = \rules ->
    ruleIdxs = List.repeat (List.len rules) 0
    buildRuleIdxsHelper rules 0 ruleIdxs


buildRuleIdxsHelper : List Rule, I64, List I64 -> List I64
buildRuleIdxsHelper = \rules, idx, ruleIdxs ->
    when List.get rules idx is
        Ok rule ->
            newIdx = idx + 1
            newRuleIdx = List.set ruleIdxs rule.num idx
            buildRuleIdxsHelper rules newIdx newRuleIdx
        _ ->
            ruleIdxs


parseMessages : ListZip.Zip, List I64 -> Res (List Message)
parseMessages = \zip, input ->
    parseMessagesHelper zip input []


parseMessagesHelper : ListZip.Zip, List I64, List Message -> Res (List Message)
parseMessagesHelper = \zip, input, messages ->
    if ListZip.afterLast zip then
        { zip, val: messages }
    else
        message = parseMessage zip input []
        newMessages = List.join [ messages, [ message.val ] ]
        parseMessagesHelper message.zip input newMessages


parseMessage : ListZip.Zip, List I64, Message -> Res Message
parseMessage = \zip, input, message ->
    if ListZip.afterLast zip then
        { zip, val: message }
    else
        newZip = ListZip.forward zip input
        if zip.val == 10 then
            { zip: newZip, val: message }
        else
            newMessage = List.append message zip.val
            parseMessage newZip input newMessage


#  test data


testInput : List I64
testInput =
    [ 48, 58, 32, 52, 32, 49, 32, 53, 10
    , 49, 58, 32, 50, 32, 51, 32, 124, 32, 51, 32, 50, 10
    , 50, 58, 32, 52, 32, 52, 32, 124, 32, 53, 32, 53, 10
    , 51, 58, 32, 52, 32, 53, 32, 124, 32, 53, 32, 52, 10
    , 52, 58, 32, 34, 97, 34, 10
    , 53, 58, 32, 34, 98, 34, 10
    , 10
    , 97, 98, 97, 98, 98, 98, 10
    , 98, 97, 98, 97, 98, 97, 10
    , 97, 98, 98, 98, 97, 98, 10
    , 97, 97, 97, 98, 98, 98, 10
    , 97, 97, 97, 97, 98, 98, 98
    ]
