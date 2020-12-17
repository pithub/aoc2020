interface Day04 exposes [ output ] imports [ Hash, ListZip, Parser, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testPassports        = parsePassports testInput
    testPassportsInvalid = parsePassports testInputInvalid
    testPassportsValid   = parsePassports testInputValid
    puzzlePassports      = parsePassports puzzleInput

    [ TestUtil.verify 4 1 1 (countValidPassports1 testPassports       ) 2
    , TestUtil.show   4 1   (countValidPassports1 puzzlePassports     )
    , TestUtil.verify 4 2 1 (countValidPassports2 testPassportsInvalid) 0
    , TestUtil.verify 4 2 2 (countValidPassports2 testPassportsValid  ) 4
    , TestUtil.show   4 2   (countValidPassports2 puzzlePassports     )
    ]


Entry : { fld: I64, val: List I64 }

Passport : List Entry


#  First Part


countValidPassports1 : List Passport -> I64
countValidPassports1 = \passports ->
    List.keepIf passports validPassport1 |> List.len


validPassport1 : Passport -> Bool
validPassport1 = \passport ->
    List.len passport == 8 || (List.len passport == 7 && missingCid passport)


missingCid : Passport -> Bool
missingCid = \passport ->
    (List.keepIf passport cidEntry |> List.len) == 0


cidEntry : Entry -> Bool
cidEntry = \entry ->
    entry.fld == s.cid


#  Second Part


countValidPassports2 : List Passport -> I64
countValidPassports2 = \passports ->
    List.keepIf passports validPassport2 |> List.len


validPassport2 : Passport -> Bool
validPassport2 = \passport ->
    validEntries = List.keepIf passport validEntry
    List.len validEntries == 8 || (List.len validEntries == 7 && missingCid validEntries)


validEntry : Entry -> Bool
validEntry = \entry ->
    if      entry.fld == s.byr then False
    else if entry.fld == s.iyr then False
    else if entry.fld == s.eyr then False
    else if entry.fld == s.hgt then False
    else if entry.fld == s.hcl then False
    else if entry.fld == s.ecl then False
    else if entry.fld == s.pid then False
    else if entry.fld == s.cid then False
    else False


#    Byr n -> (1920 <= n) && (n <= 2002)
#    Iyr n -> (2010 <= n) && (n <= 2020)
#    Eyr n -> (2020 <= n) && (n <= 2030)
#    Hgt n -> ((150 <= n) && (n <= 193)) || ((-76 <= n) && (n <= -59))
#    Hcl 1 -> True
#    Ecl 1 -> True
#    Pid 1 -> True
#    Cid   -> True


#  Parse Passports


parsePassports : List I64 -> List Passport
parsePassports = \input ->
    init = Parser.initial input
    passportsParser = Parser.repeated passportParser
    res = passportsParser input init
    res.val


passportParser : Parser.Parser Passport
passportParser =
    (\input, state ->
        passportHelper input state [])


passportHelper : List I64, Parser.State, Passport -> Parser.Res Passport
passportHelper = \input, state, val ->
    if ListZip.afterLast state then
        { val, state }
    else if state.val == 10 then
        { val, state }
    else
        res = entryParser input state
        newVal = List.join [ val, [ res.val ] ]
        passportHelper input res.state newVal


entryParser : Parser.Parser Entry
entryParser =
    (\input, state ->
        pStr3 = Parser.fixedStr 3
        pIgn1 = Parser.fixedIgnore 1
        pWord = Parser.singleWord

        fld = pStr3 input state
        ign = pIgn1 input fld.state
        wrd = pWord input ign.state

        entry = { fld: fld.val, val: wrd.val }
        { val: entry, state: wrd.state }
    )


s =
    { byr: Hash.str [ 98, 121, 114 ]
    , iyr: Hash.str [ 105, 121, 114 ]
    , eyr: Hash.str [ 101, 121, 114 ]
    , hgt: Hash.str [ 104, 103, 116 ]
    , hcl: Hash.str [ 104, 99, 108 ]
    , ecl: Hash.str [ 101, 99, 108 ]
    , pid: Hash.str [ 112, 105, 100 ]
    , cid: Hash.str [ 99, 105, 100 ]
    }


#  Test Data


testInput : List I64
testInput =
    [ 101, 99, 108, 58, 103, 114, 121, 32
    , 112, 105, 100, 58, 56, 54, 48, 48, 51, 51, 51, 50, 55, 32
    , 101, 121, 114, 58, 50, 48, 50, 48, 32
    , 104, 99, 108, 58, 35, 102, 102, 102, 102, 102, 100, 10
    , 98, 121, 114, 58, 49, 57, 51, 55, 32
    , 105, 121, 114, 58, 50, 48, 49, 55, 32
    , 99, 105, 100, 58, 49, 52, 55, 32
    , 104, 103, 116, 58, 49, 56, 51, 99, 109, 10
    , 10
    , 105, 121, 114, 58, 50, 48, 49, 51, 32
    , 101, 99, 108, 58, 97, 109, 98, 32
    , 99, 105, 100, 58, 51, 53, 48, 32
    , 101, 121, 114, 58, 50, 48, 50, 51, 32
    , 112, 105, 100, 58, 48, 50, 56, 48, 52, 56, 56, 56, 52, 10
    , 104, 99, 108, 58, 35, 99, 102, 97, 48, 55, 100, 32
    , 98, 121, 114, 58, 49, 57, 50, 57, 10
    , 10
    , 104, 99, 108, 58, 35, 97, 101, 49, 55, 101, 49, 32
    , 105, 121, 114, 58, 50, 48, 49, 51, 10
    , 101, 121, 114, 58, 50, 48, 50, 52, 10
    , 101, 99, 108, 58, 98, 114, 110, 32
    , 112, 105, 100, 58, 55, 54, 48, 55, 53, 51, 49, 48, 56, 32
    , 98, 121, 114, 58, 49, 57, 51, 49, 10
    , 104, 103, 116, 58, 49, 55, 57, 99, 109, 10
    , 10
    , 104, 99, 108, 58, 35, 99, 102, 97, 48, 55, 100, 32
    , 101, 121, 114, 58, 50, 48, 50, 53, 32
    , 112, 105, 100, 58, 49, 54, 54, 53, 53, 57, 54, 52, 56, 10
    , 105, 121, 114, 58, 50, 48, 49, 49, 32
    , 101, 99, 108, 58, 98, 114, 110, 32
    , 104, 103, 116, 58, 53, 57, 105, 110
    ]


testInputInvalid : List I64
testInputInvalid =
    [ 101, 121, 114, 58, 49, 57, 55, 50, 32
    , 99, 105, 100, 58, 49, 48, 48, 10
    , 104, 99, 108, 58, 35, 49, 56, 49, 55, 49, 100, 32
    , 101, 99, 108, 58, 97, 109, 98, 32
    , 104, 103, 116, 58, 49, 55, 48, 32
    , 112, 105, 100, 58, 49, 56, 54, 99, 109, 32
    , 105, 121, 114, 58, 50, 48, 49, 56, 32
    , 98, 121, 114, 58, 49, 57, 50, 54, 10
    , 10
    , 105, 121, 114, 58, 50, 48, 49, 57, 10
    , 104, 99, 108, 58, 35, 54, 48, 50, 57, 50, 55, 32
    , 101, 121, 114, 58, 49, 57, 54, 55, 32
    , 104, 103, 116, 58, 49, 55, 48, 99, 109, 10
    , 101, 99, 108, 58, 103, 114, 110, 32
    , 112, 105, 100, 58, 48, 49, 50, 53, 51, 51, 48, 52, 48, 32
    , 98, 121, 114, 58, 49, 57, 52, 54, 10
    , 10
    , 104, 99, 108, 58, 100, 97, 98, 50, 50, 55, 32
    , 105, 121, 114, 58, 50, 48, 49, 50, 10
    , 101, 99, 108, 58, 98, 114, 110, 32
    , 104, 103, 116, 58, 49, 56, 50, 99, 109, 32
    , 112, 105, 100, 58, 48, 50, 49, 53, 55, 50, 52, 49, 48, 32
    , 101, 121, 114, 58, 50, 48, 50, 48, 32
    , 98, 121, 114, 58, 49, 57, 57, 50, 32
    , 99, 105, 100, 58, 50, 55, 55, 10
    , 10
    , 104, 103, 116, 58, 53, 57, 99, 109, 32
    , 101, 99, 108, 58, 122, 122, 122, 10
    , 101, 121, 114, 58, 50, 48, 51, 56, 32
    , 104, 99, 108, 58, 55, 52, 52, 53, 52, 97, 32
    , 105, 121, 114, 58, 50, 48, 50, 51, 10
    , 112, 105, 100, 58, 51, 53, 53, 54, 52, 49, 50, 51, 55, 56, 32
    , 98, 121, 114, 58, 50, 48, 48, 55
    ]


testInputValid : List I64
testInputValid =
    [ 112, 105, 100, 58, 48, 56, 55, 52, 57, 57, 55, 48, 52, 32
    , 104, 103, 116, 58, 55, 52, 105, 110, 32
    , 101, 99, 108, 58, 103, 114, 110, 32
    , 105, 121, 114, 58, 50, 48, 49, 50, 32
    , 101, 121, 114, 58, 50, 48, 51, 48, 32
    , 98, 121, 114, 58, 49, 57, 56, 48, 10
    , 104, 99, 108, 58, 35, 54, 50, 51, 97, 50, 102, 10
    , 10
    , 101, 121, 114, 58, 50, 48, 50, 57, 32
    , 101, 99, 108, 58, 98, 108, 117, 32
    , 99, 105, 100, 58, 49, 50, 57, 32
    , 98, 121, 114, 58, 49, 57, 56, 57, 10
    , 105, 121, 114, 58, 50, 48, 49, 52, 32
    , 112, 105, 100, 58, 56, 57, 54, 48, 53, 54, 53, 51, 57, 32
    , 104, 99, 108, 58, 35, 97, 57, 55, 56, 52, 50, 32
    , 104, 103, 116, 58, 49, 54, 53, 99, 109, 10
    , 10
    , 104, 99, 108, 58, 35, 56, 56, 56, 55, 56, 53, 10
    , 104, 103, 116, 58, 49, 54, 52, 99, 109, 32
    , 98, 121, 114, 58, 50, 48, 48, 49, 32
    , 105, 121, 114, 58, 50, 48, 49, 53, 32
    , 99, 105, 100, 58, 56, 56, 10
    , 112, 105, 100, 58, 53, 52, 53, 55, 54, 54, 50, 51, 56, 32
    , 101, 99, 108, 58, 104, 122, 108, 10
    , 101, 121, 114, 58, 50, 48, 50, 50, 10
    , 10
    , 105, 121, 114, 58, 50, 48, 49, 48, 32
    , 104, 103, 116, 58, 49, 53, 56, 99, 109, 32
    , 104, 99, 108, 58, 35, 98, 54, 54, 53, 50, 97, 32
    , 101, 99, 108, 58, 98, 108, 117, 32
    , 98, 121, 114, 58, 49, 57, 52, 52, 32
    , 101, 121, 114, 58, 50, 48, 50, 49, 32
    , 112, 105, 100, 58, 48, 57, 51, 49, 53, 52, 55, 49, 57
    ]
