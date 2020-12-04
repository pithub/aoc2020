interface Day04 exposes [ output ] imports [ TestUtil ]


output : List (List Int)
output =
    [ TestUtil.verify 4 1 1 (countValidPassports1 testInput       ) 2
    , TestUtil.show   4 1   (countValidPassports1 puzzleInput     )
    , TestUtil.verify 4 2 1 (countValidPassports2 testInputInvalid) 0
    , TestUtil.verify 4 2 2 (countValidPassports2 testInputValid  ) 4
    , TestUtil.show   4 2   (countValidPassports2 puzzleInput     )
    ]


Field : [ Byr Int, Iyr Int, Eyr Int, Hgt Int, Hcl Int, Ecl Int, Pid Int, Cid ]

Passport : List Field


countValidPassports1 : List Passport -> Int
countValidPassports1 = \passports ->
    List.keepIf passports validPassport1 |> List.len


validPassport1 : Passport -> Bool
validPassport1 = \passport ->
    List.len passport == 8 || (List.len passport == 7 && missingCid passport)


missingCid : Passport -> Bool
missingCid = \passport ->
    (List.keepIf passport cidField |> List.len) == 0


cidField : Field -> Bool
cidField = \field ->
    when field is
        Cid -> True
        _ -> False


countValidPassports2 : List Passport -> Int
countValidPassports2 = \passports ->
    List.keepIf passports validPassport2 |> List.len


validPassport2 : Passport -> Bool
validPassport2 = \passport ->
    validFields = List.keepIf passport validField
    List.len validFields == 8 || (List.len validFields == 7 && missingCid validFields)


validField : Field -> Bool
validField = \field ->
    when field is
        Byr n -> (1920 <= n) && (n <= 2002)
        Iyr n -> (2010 <= n) && (n <= 2020)
        Eyr n -> (2020 <= n) && (n <= 2030)
        Hgt n -> ((150 <= n) && (n <= 193)) || ((-76 <= n) && (n <= -59))
        Hcl 1 -> True
        Ecl 1 -> True
        Pid 1 -> True
        Cid   -> True
        _ -> False


testInput : List Passport
testInput =
    [ [ Ecl 1, Pid 1, Eyr 2020, Hcl 1
      , Byr 1937, Iyr 2017, Cid, Hgt 183
      ]
    , [ Iyr 2013, Ecl 1, Cid, Eyr 2023, Pid 1
      , Hcl 1, Byr 1929
      ]
    , [ Hcl 1, Iyr 2013
      , Eyr 2024
      , Ecl 1, Pid 1, Byr 1931
      , Hgt 179
      ]
    , [ Hcl 1, Eyr 2025, Pid 1
      , Iyr 2011, Ecl 1, Hgt -59
      ]
    ]


testInputInvalid : List Passport
testInputInvalid =
    [ [ Eyr 1972, Cid
      , Hcl 1, Ecl 1, Hgt 0, Pid 0, Iyr 2018, Byr 1926
      ]
    , [ Iyr 2019
      , Hcl 1, Eyr 1967, Hgt 170
      , Ecl 1, Pid 1, Byr 1946
      ]
    , [ Hcl 0, Iyr 2012
      , Ecl 1, Hgt 182, Pid 1, Eyr 2020, Byr 1992, Cid
      ]
    , [ Hgt 59, Ecl 0
      , Eyr 2038, Hcl 0, Iyr 2023
      , Pid 18, Byr 2007
      ]
    ]


testInputValid : List Passport
testInputValid =
    [ [ Pid 1, Hgt -74, Ecl 1, Iyr 2012, Eyr 2030, Byr 1980
      , Hcl 1
      ]
    , [ Eyr 2029, Ecl 1, Cid, Byr 1989
      , Iyr 2014, Pid 1, Hcl 1, Hgt 165
      ]
    , [ Hcl 1
      , Hgt 164, Byr 2001, Iyr 2015, Cid
      , Pid 1, Ecl 1
      , Eyr 2022
      ]
    , [ Iyr 2010, Hgt 158, Hcl 1, Ecl 1, Byr 1944, Eyr 2021, Pid 1
      ]
    ]


puzzleInput : List Passport
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
        , puzzleInput10
        ]


puzzleInput1 : List Passport
puzzleInput1 =
    [ [ Hgt 176
      , Iyr 2013
      , Hcl 1, Ecl 1
      , Byr 2000
      , Eyr 2034
      , Cid, Pid 1
      ]
    , [ Hcl 1, Ecl 1, Hgt 155, Pid 0, Iyr 2017
      , Byr 1939
      , Eyr 2020
      ]
    , [ Pid 1, Eyr 1972
      , Hgt 152, Ecl 0, Byr 1960, Hcl 0, Iyr 2023
      ]
    , [ Eyr 2028, Hcl 1, Hgt -73, Byr 1926, Ecl 0, Iyr 2016, Pid 1
      ]
    , [ Pid 1, Ecl 1, Iyr 2019
      , Cid, Byr 1940
      , Eyr 2030, Hgt 170
      , Hcl 1
      ]
    , [ Ecl 1
      , Iyr 2017
      , Pid 1, Hcl 1, Byr 1959, Hgt 159, Eyr 2022
      ]
    , [ Iyr 2011, Eyr 2021, Hcl 0
      , Ecl 1, Byr 2002, Pid 0, Cid
      , Hgt 186
      ]
    , [ Byr 2022, Pid 17, Iyr 1984
      , Hgt 76, Hcl 0
      , Ecl 0
      , Eyr 2037
      ]
    , [ Hcl 0, Eyr 1945
      , Pid 17, Hgt 75
      , Iyr 1934, Cid, Ecl 0
      , Byr 2005
      ]
    , [ Byr 2005
      , Ecl 0
      , Eyr 2021, Pid 0
      , Cid, Iyr 2020, Hcl 0, Hgt 157
      ]
    , [ Iyr 2020, Eyr 2020, Hcl 1, Ecl 1, Pid 1, Hgt 168
      , Byr 2002
      ]
    , [ Hcl 1, Hgt 160, Eyr 2020, Iyr 2015
      , Pid 1, Ecl 1, Byr 2023
      ]
    , [ Hcl 1, Hgt 157
      , Byr 1994, Eyr 2027, Pid 1
      , Iyr 2016
      ]
    , [ Pid 1
      , Iyr 2014, Hcl 1, Ecl 1, Hgt 180, Byr 1927, Eyr 2021
      ]
    , [ Pid 14, Hcl 1, Byr 2030
      , Eyr 2032, Hgt 158, Iyr 2012
      , Ecl 0
      ]
    , [ Eyr 2022
      , Ecl 1
      , Byr 1986
      , Hgt 161, Cid, Pid 1, Hcl 1, Iyr 2019
      ]
    , [ Cid
      , Pid 1
      , Hgt 184, Byr 1937, Ecl 1
      , Hcl 1
      , Iyr 2015, Eyr 2027
      ]
    , [ Iyr 2016, Hcl 1, Hgt 170, Eyr 2022, Ecl 1, Pid 1
      , Byr 1952
      ]
    , [ Hcl 1, Iyr 2018, Hgt 189
      , Byr 1971, Ecl 1, Eyr 2029
      , Pid 1
      ]
    , [ Eyr 2022, Hcl 1, Pid 1
      , Byr 1978, Iyr 2020, Hgt 186
      , Ecl 1
      ]
    , [ Hgt 171, Byr 1949, Hcl 1
      , Ecl 1, Eyr 2030, Pid 1, Iyr 2013
      ]
    , [ Pid 1, Eyr 2024, Byr 1921
      , Iyr 2012, Ecl 1, Hcl 1, Hgt 154
      ]
    , [ Pid 0
      , Byr 2009
      , Hgt 0, Iyr 2019, Hcl 1
      , Eyr 2036, Ecl 1
      ]
    , [ Eyr 2040, Hcl 1, Cid
      , Byr 2027
      , Pid 18, Ecl 1, Iyr 2026
      , Hgt 63
      ]
    ]


puzzleInput2 : List Passport
puzzleInput2 =
    [ [ Iyr 2011, Cid, Pid 1
      , Ecl 1, Hgt 160, Eyr 2029, Hcl 1, Byr 1996
      ]
    , [ Hgt -161, Byr 2025, Cid, Iyr 2024, Eyr 2040, Pid 1
      , Hcl 1, Ecl 1
      ]
    , [ Ecl 0, Iyr 1935
      , Hgt 60
      , Byr 2003, Eyr 1987
      , Hcl 0, Pid 0
      ]
    , [ Iyr 2018, Pid 1, Ecl 1, Eyr 2023, Hgt 183, Hcl 1
      , Byr 1967, Cid
      ]
    , [ Eyr 2022, Ecl 1
      , Pid 1, Iyr 2012
      , Hgt 165
      , Byr 1955, Hcl 1
      ]
    , [ Byr 2014, Hcl 0, Iyr 2029, Cid, Pid 0, Hgt 75, Ecl 0
      ]
    , [ Hgt 156, Eyr 2023, Iyr 2011, Ecl 1, Hcl 1, Pid 1, Byr 1952
      ]
    , [ Iyr 2011, Byr 1935
      , Hcl 1, Ecl 1, Pid 1
      , Eyr 2028, Hgt 173
      ]
    , [ Iyr 2012, Cid, Eyr 2022
      , Pid 1, Hcl 1, Ecl 1, Byr 1960, Hgt 152
      ]
    , [ Eyr 2026, Pid 1, Hgt -75, Iyr 2019, Ecl 1, Byr 1947
      , Hcl 1
      ]
    , [ Cid, Iyr 2012
      , Eyr 2024, Byr 1934, Hcl 1
      , Hgt 165, Ecl 1, Pid 1
      ]
    , [ Cid, Iyr 2020
      , Byr 1971, Hcl 1, Pid 1, Hgt 179, Eyr 2027, Ecl 1
      ]
    , [ Byr 1987, Hcl 0, Eyr 2032, Ecl 0, Pid 1, Cid, Hgt -150
      , Iyr 2015
      ]
    , [ Hcl 0, Ecl 0
      , Pid 1, Byr 1944, Hgt 158, Iyr 2017, Eyr 1924
      ]
    , [ Iyr 2016, Pid 11, Byr 2014, Hgt 0, Eyr 2032, Ecl 1, Hcl 0
      ]
    , [ Byr 1979, Eyr 2030, Iyr 1978, Hgt 0, Pid 18, Hcl 0, Ecl 1
      ]
    , [ Hgt 70, Hcl 0, Iyr 2020, Eyr 1977, Ecl 0, Pid 17, Byr 1973
      ]
    , [ Iyr 2003
      , Hcl 1
      , Pid 1
      , Byr 2030, Hgt 175, Eyr 2020
      , Ecl 1
      ]
    , [ Pid 1, Eyr 2022
      , Cid, Iyr 2016, Hgt 163
      , Byr 1922
      , Hcl 1, Ecl 1
      ]
    , [ Hgt 167, Byr 2009, Eyr 1975, Cid, Pid 0, Iyr 2029
      , Hcl 0
      ]
    , [ Hgt -67, Ecl 1
      , Eyr 2023
      , Cid, Pid 1, Byr 1990, Iyr 2011, Hcl 1
      ]
    , [ Ecl 0, Byr 1922, Cid, Hcl 0, Iyr 1932, Eyr 1996, Pid 0, Hgt 62
      ]
    , [ Byr 1949
      , Cid, Iyr 2017, Ecl 1
      , Hgt 164, Eyr 2027, Hcl 1
      , Pid 1
      ]
    , [ Ecl 1, Hgt 162
      , Pid 1, Byr 1923, Eyr 2029, Iyr 2011, Hcl 1, Cid
      ]
    , [ Iyr 2020
      , Hcl 1, Pid 1, Ecl 1
      , Hgt -67
      , Eyr 2029, Byr 1937
      ]
    , [ Iyr 2012
      , Hcl 1, Pid 1, Ecl 1, Hgt 185, Eyr 2024, Byr 1971
      ]
    , [ Hcl 1, Pid 1
      , Ecl 1, Hgt 155, Eyr 2028
      , Iyr 2019
      ]
    , [ Pid 1, Hgt 0, Cid, Eyr 2027
      , Iyr 2020, Ecl 1, Byr 1935, Hcl 1
      ]
    , [ Ecl 1
      , Hcl 1, Byr 1977, Hgt 165
      , Pid 1, Eyr 2030
      , Iyr 2012
      ]
    ]


puzzleInput3 : List Passport
puzzleInput3 =
    [ [ Byr 1989, Ecl 1, Eyr 2026, Pid 1, Iyr 2016
      , Hcl 1, Cid, Hgt 161
      ]
    , [ Iyr 2017
      , Byr 1973, Pid 1, Hcl 1, Cid, Ecl 1
      , Hgt 166, Eyr 2022
      ]
    , [ Pid 1
      , Hcl 0
      , Eyr 1933, Iyr 2019, Hgt 164
      , Byr 2017, Ecl 0
      ]
    , [ Eyr 2023, Pid 1, Hcl 1, Cid
      , Hgt 156
      , Ecl 1, Iyr 2015, Byr 1926
      ]
    , [ Eyr 2022
      , Iyr 2020
      , Hgt 158, Ecl 1
      , Byr 1988
      , Pid 1, Hcl 1
      ]
    , [ Eyr 2039
      , Pid 12, Byr 1936, Ecl 0, Iyr 2022, Hcl 0
      , Hgt 0, Cid
      ]
    , [ Iyr 2015, Eyr 2026
      , Hcl 1, Pid 1
      , Cid
      , Ecl 1
      , Byr 1920, Hgt 163
      ]
    , [ Cid, Iyr 2020
      , Pid 1, Eyr 2022, Hgt 181
      , Byr 1997, Ecl 1, Hcl 1
      ]
    , [ Byr 2016
      , Iyr 2012
      , Ecl 0
      , Hgt -68, Eyr 1993
      , Pid 12, Hcl 0
      , Cid
      ]
    , [ Iyr 2018
      , Hgt 154, Ecl 1, Byr 1970
      , Eyr 2021, Pid 1, Hcl 1
      ]
    , [ Iyr 2012
      , Eyr 2027, Hgt 67, Hcl 1, Ecl 0, Pid 1, Byr 2020
      ]
    , [ Hcl 1, Ecl 1, Eyr 2023, Iyr 2012, Pid 1
      , Hgt 159
      ]
    , [ Hgt -162, Hcl 0
      , Byr 2029
      , Eyr 2023, Ecl 0, Iyr 2016, Pid 0
      ]
    , [ Cid, Hgt -71, Ecl 1, Pid 1
      , Eyr 2020, Iyr 2014
      ]
    , [ Eyr 2028, Hgt 181
      , Ecl 1, Pid 1, Hcl 1, Byr 1994, Iyr 2011, Cid
      ]
    , [ Ecl 1
      , Byr 2029, Iyr 2017, Pid 1, Eyr 2020, Hcl 0, Hgt 163
      ]
    , [ Hgt 158
      , Eyr 2025, Ecl 1, Cid, Pid 1
      , Hcl 1, Byr 1991, Iyr 2013
      ]
    , [ Eyr 2028
      , Iyr 2018, Pid 13
      , Hgt 172
      , Ecl 0, Hcl 1, Cid, Byr 1930
      ]
    , [ Ecl 1, Hcl 1
      , Eyr 2020, Pid 1, Iyr 2016
      ]
    , [ Iyr 2014, Ecl 1
      , Cid, Eyr 2023
      , Hgt 183
      , Byr 1976
      , Pid 1, Hcl 1
      ]
    , [ Eyr 2034
      , Cid, Hcl 0
      , Byr 2027
      , Hgt -161, Pid 14, Ecl 0, Iyr 2026
      ]
    , [ Eyr 1978, Byr 1925, Iyr 2018, Hgt 170, Ecl 0
      , Pid 1, Hcl 1
      ]
    , [ Eyr 2023, Hcl 1, Hgt 179
      , Pid 1, Iyr 2014, Cid, Ecl 1, Byr 1939
      ]
    , [ Hgt 187
      , Pid 0, Ecl 1, Eyr 2023, Cid, Hcl 1, Iyr 2011, Byr 1958
      ]
    , [ Hgt 162, Byr 2028, Ecl 0
      , Eyr 2037, Hcl 0
      , Iyr 2021
      , Pid 0
      ]
    ]


puzzleInput4 : List Passport
puzzleInput4 =
    [ [ Eyr 2027, Hcl 1
      , Byr 2002, Ecl 1, Iyr 2014
      , Pid 1, Hgt 177
      ]
    , [ Hgt 64, Pid 0
      , Eyr 2029, Cid
      , Byr 2026
      , Iyr 2017, Hcl 0, Ecl 1
      ]
    , [ Eyr 2028, Byr 2007, Hgt 155, Ecl 0, Hcl 1, Pid 1
      , Iyr 2019
      ]
    , [ Pid 1
      , Byr 1992
      , Hcl 1, Hgt 163, Ecl 1
      , Iyr 2011, Eyr 2021
      ]
    , [ Eyr 2020, Byr 1994, Iyr 2011, Hgt 186, Pid 1, Hcl 1, Ecl 1
      ]
    , [ Byr 1923, Iyr 2015, Ecl 1, Pid 1
      , Hcl 1
      , Hgt 159
      , Eyr 2026
      ]
    , [ Hgt 171, Ecl 1, Pid 1, Iyr 2020
      , Cid, Hcl 1, Eyr 2021
      , Byr 1960
      ]
    , [ Eyr 2002
      , Hcl 0, Ecl 0, Pid 0, Byr 2024, Iyr 2020
      , Hgt -186
      ]
    , [ Iyr 2011, Byr 1924, Eyr 2024
      , Hcl 1, Ecl 1
      , Pid 1, Hgt -162
      ]
    , [ Hcl 0, Hgt 67
      , Byr 2025
      , Pid 1
      , Iyr 2013
      , Ecl 1, Eyr 2025
      ]
    , [ Cid, Hcl 0
      , Hgt -172, Eyr 1994, Iyr 2023
      , Ecl 0
      , Byr 2015
      , Pid 0
      ]
    , [ Hgt 173, Eyr 2028
      , Ecl 1, Pid 1
      , Byr 1942
      , Iyr 2019
      , Cid
      , Hcl 1
      ]
    , [ Cid
      , Ecl 1, Eyr 1933, Byr 1982, Pid 0, Hcl 1, Hgt 174
      , Iyr 2023
      ]
    , [ Cid, Hcl 1, Pid 1
      , Byr 1968, Ecl 1
      , Iyr 2012, Hgt 156
      , Eyr 2020
      ]
    , [ Hgt 176
      , Byr 1954, Ecl 1
      , Eyr 2020
      , Pid 1, Iyr 2019
      , Hcl 1
      ]
    , [ Iyr 2026, Hgt -193
      , Byr 2018, Pid 0, Hcl 0, Eyr 1948, Ecl 0
      ]
    , [ Byr 1962
      , Eyr 2022, Pid 1, Iyr 2019, Hgt 158, Hcl 1, Ecl 1
      ]
    , [ Cid, Hgt 187, Eyr 2024, Iyr 2016, Byr 1964
      , Ecl 1, Pid 1, Hcl 1
      ]
    , [ Ecl 1, Byr 2000, Eyr 2023
      , Pid 1, Hcl 1, Iyr 2011, Hgt 177
      ]
    , [ Iyr 2015, Hgt 180, Cid, Hcl 1, Pid 1, Eyr 2029, Byr 2002, Ecl 1
      ]
    , [ Eyr 2027, Iyr 2015, Ecl 1
      , Pid 1
      , Byr 1975, Hgt 191, Cid, Hcl 1
      ]
    , [ Hcl 1, Iyr 2015
      , Hgt 167, Byr 1990, Ecl 1, Pid 0, Eyr 2023
      ]
    , [ Ecl 1, Eyr 2028
      , Byr 1934, Iyr 2013, Hcl 1
      , Pid 1, Hgt 173
      ]
    , [ Pid 1
      , Eyr 2024
      , Iyr 2013, Byr 1926, Hcl 1
      , Ecl 1, Hgt 180
      ]
    ]


puzzleInput5 : List Passport
puzzleInput5 =
    [ [ Hcl 1, Hgt -70, Ecl 1, Iyr 2019
      , Byr 1937
      , Eyr 2030, Pid 1
      ]
    , [ Ecl 1
      , Hgt -64, Pid 1, Hcl 1
      , Byr 1929, Eyr 2027
      ]
    , [ Ecl 1, Hcl 1
      , Pid 1, Cid, Byr 1982
      , Iyr 2019, Eyr 2030, Hgt 193
      ]
    , [ Eyr 1927
      , Hcl 0, Hgt 158, Byr 1930
      , Ecl 0, Iyr 2018
      , Cid
      , Pid 12
      ]
    , [ Ecl 1, Byr 1970, Hgt 181
      , Pid 1, Eyr 2030, Iyr 2017, Cid, Hcl 1
      ]
    , [ Iyr 2018, Hgt -73, Pid 1, Hcl 1
      , Ecl 1, Byr 1973
      ]
    , [ Cid, Hcl 1, Byr 2011
      , Ecl 1, Iyr 2025, Pid 0, Hgt 67, Eyr 2016
      ]
    , [ Hgt 192, Ecl 1, Eyr 2026, Pid 1, Hcl 1, Byr 1966, Iyr 2019
      ]
    , [ Iyr 2013, Byr 1995, Eyr 2028, Hcl 1, Ecl 1, Cid, Pid 1, Hgt 176
      ]
    , [ Hcl 1, Byr 1951
      , Hgt 161, Pid 1, Iyr 2015, Ecl 1
      , Eyr 2030
      ]
    , [ Cid, Ecl 1, Iyr 2017, Eyr 2030
      , Hgt 176, Hcl 1
      , Byr 1965
      ]
    , [ Eyr 2020, Hcl 1
      , Pid 1, Ecl 1, Iyr 2017, Byr 1920
      , Hgt 180
      ]
    , [ Hcl 1, Iyr 2019
      , Byr 1938
      , Hgt 153, Ecl 1, Pid 1
      , Eyr 2020
      ]
    , [ Eyr 2020, Hgt 184, Iyr 2019
      , Pid 1, Ecl 1, Byr 1977, Hcl 1
      ]
    , [ Eyr 2025
      , Ecl 1, Hcl 1, Byr 1970, Iyr 2010, Pid 1, Hgt 184
      ]
    , [ Ecl 1, Byr 1992, Hgt -71
      , Iyr 2014, Cid, Hcl 1, Pid 1
      , Eyr 2026
      ]
    , [ Cid, Ecl 1, Eyr 2022
      , Hgt 169, Pid 1
      , Byr 1937, Iyr 2014, Hcl 1
      ]
    , [ Hgt 192
      , Iyr 2015
      , Eyr 2028, Ecl 1, Pid 13, Hcl 1
      , Byr 1930
      ]
    , [ Byr 1938, Hcl 1, Hgt 178, Iyr 1953, Eyr 2038
      , Ecl 1, Pid 0
      ]
    , [ Hgt -66, Byr 1951, Iyr 2016, Hcl 1
      , Eyr 2027
      , Ecl 0, Pid 1
      ]
    , [ Iyr 2012, Eyr 2025
      , Hcl 1, Pid 1, Cid, Hgt 186, Byr 1938
      , Ecl 1
      ]
    , [ Iyr 2015, Hcl 1, Cid, Eyr 2021
      , Ecl 1, Hgt 186, Pid 1
      ]
    , [ Hcl 1, Iyr 2014, Pid 1, Cid, Hgt 150, Byr 1981, Ecl 1, Eyr 2024
      ]
    , [ Ecl 1, Hcl 1
      , Eyr 2028, Iyr 2010
      , Hgt 162, Byr 1944, Pid 1
      ]
    , [ Eyr 2028, Byr 1974
      , Ecl 1
      , Iyr 2010, Hcl 1, Hgt 160
      ]
    , [ Hcl 1
      , Byr 1959, Eyr 2027, Iyr 2016, Ecl 1, Hgt 169, Pid 1
      ]
    , [ Hcl 1, Pid 1, Hgt 153
      , Byr 1973, Iyr 2012
      , Ecl 1, Eyr 2026
      ]
    , [ Hgt 151
      , Byr 1966, Eyr 2029, Pid 1, Hcl 1, Ecl 1, Iyr 2010
      ]
    ]


puzzleInput6 : List Passport
puzzleInput6 =
    [ [ Hcl 1, Byr 1974, Pid 1, Iyr 2017, Eyr 2030
      , Hgt 165, Ecl 1
      ]
    , [ Iyr 2026, Pid 0
      , Ecl 0, Hcl 0, Hgt 71
      , Eyr 2029
      ]
    , [ Cid, Hcl 1, Byr 1998
      , Pid 1, Iyr 2015
      , Ecl 1, Eyr 2021, Hgt 165
      ]
    , [ Byr 1939, Hcl 1, Ecl 1, Hgt -69, Pid 1, Eyr 2027, Iyr 2013
      ]
    , [ Pid 1
      , Iyr 2015
      , Cid, Ecl 1
      , Byr 1942
      , Eyr 2027, Hgt 186, Hcl 1
      ]
    , [ Ecl 0, Eyr 2031, Iyr 1956
      , Hgt 0, Pid 0, Hcl 0, Byr 2027
      ]
    , [ Byr 1972, Iyr 2020, Eyr 2026, Hcl 1, Pid 1, Hgt -65, Ecl 1
      ]
    , [ Eyr 2027
      , Hgt 59
      , Byr 2022
      , Pid 1, Ecl 0, Iyr 2028, Hcl 0
      ]
    , [ Byr 2004, Hgt 0, Iyr 2017
      , Eyr 2040, Ecl 1, Pid 19, Cid, Hcl 0
      ]
    , [ Ecl 1, Byr 1962, Cid
      , Iyr 2019, Eyr 2026
      , Hgt 159
      , Hcl 1, Pid 1
      ]
    , [ Iyr 2026, Eyr 2039, Pid 1, Cid, Ecl 0, Hgt 166
      , Byr 2023, Hcl 0
      ]
    , [ Byr 1961, Iyr 2010, Ecl 1
      , Eyr 2023, Pid 1
      ]
    , [ Hgt 193, Pid 1, Eyr 2020, Hcl 1, Cid, Byr 1946
      ]
    , [ Eyr 2026, Ecl 1
      , Hcl 1, Byr 1983, Hgt 182, Pid 1, Cid, Iyr 2015
      ]
    , [ Hgt 152, Cid, Iyr 2012, Byr 1947, Hcl 1, Ecl 1
      , Pid 1, Eyr 2023
      ]
    , [ Hgt 150, Ecl 1, Pid 1
      , Iyr 2019, Cid, Byr 1976, Hcl 1, Eyr 2022
      ]
    , [ Hgt -164, Pid 1
      , Eyr 2021
      , Iyr 2014
      , Hcl 0, Cid, Ecl 1
      ]
    , [ Byr 1981, Pid 1, Iyr 2013, Eyr 2023
      , Hcl 1, Ecl 1
      , Hgt 151
      ]
    , [ Pid 1, Hgt 154, Cid
      , Hcl 1, Byr 1949, Ecl 1, Iyr 2010, Eyr 2028
      ]
    , [ Iyr 2019, Hcl 1
      , Pid 1, Ecl 1
      , Eyr 2030
      , Byr 1938, Hgt 177
      ]
    , [ Ecl 1, Hcl 1, Eyr 2029, Byr 1963
      , Hgt 185, Pid 1, Iyr 2011
      ]
    , [ Ecl 1, Eyr 2022, Byr 1941, Hgt 167
      , Iyr 2012, Hcl 1, Pid 1
      , Cid
      ]
    , [ Eyr 2027, Ecl 1, Byr 1968, Pid 1
      , Hcl 1, Hgt 151
      , Iyr 2011
      ]
    , [ Pid 1, Hgt 189, Eyr 2030, Ecl 1, Byr 2001, Hcl 1
      ]
    , [ Byr 1985
      , Ecl 1, Pid 1, Cid
      , Hgt 159, Hcl 1, Iyr 2018, Eyr 2020
      ]
    , [ Byr 1998
      , Hcl 1, Eyr 2023, Pid 1, Iyr 2011, Ecl 1, Hgt 177
      ]
    , [ Ecl 1
      , Byr 1980, Pid 1, Eyr 2029
      , Hcl 1, Hgt 186
      ]
    , [ Eyr 2030, Hcl 1
      , Hgt 165
      , Ecl 1
      , Iyr 2017, Pid 1, Cid, Byr 2001
      ]
    ]


puzzleInput7 : List Passport
puzzleInput7 =
    [ [ Byr 1994, Hcl 1, Iyr 2015
      , Pid 1
      , Hgt 175
      , Eyr 2027, Ecl 1
      ]
    , [ Hcl 1, Pid 1
      , Byr 1961, Ecl 1, Hgt 175
      , Iyr 2020, Eyr 2025
      ]
    , [ Byr 1987, Pid 1
      , Eyr 2028, Hcl 0
      , Hgt -64, Cid
      , Ecl 0, Iyr 2019
      ]
    , [ Iyr 2015, Hcl 0, Hgt -160, Ecl 0, Byr 2015, Eyr 2036
      ]
    , [ Byr 1935
      , Hcl 1, Hgt 152, Ecl 1
      , Pid 1, Iyr 2020, Eyr 2020
      ]
    , [ Pid 1, Byr 1998, Hgt 185, Ecl 1, Eyr 2026, Iyr 2013, Hcl 1
      ]
    , [ Ecl 1, Pid 1
      , Cid
      , Hgt -60
      , Eyr 2026
      , Hcl 1
      , Byr 1961, Iyr 2011
      ]
    , [ Eyr 2021, Hgt 162, Cid
      , Pid 1
      , Hcl 1
      , Ecl 1, Byr 1962, Iyr 2017
      ]
    , [ Hcl 1
      , Iyr 2016
      , Eyr 2029
      , Ecl 1, Byr 1927, Cid, Pid 1, Hgt -65
      ]
    , [ Byr 1931
      , Cid
      , Hgt -66
      , Ecl 1, Iyr 2020, Hcl 1, Eyr 2025, Pid 1
      ]
    , [ Hgt 0
      , Eyr 2040, Ecl 1, Byr 2029
      , Iyr 1967, Hcl 0
      , Pid 0, Cid
      ]
    , [ Pid 1
      , Hcl 1
      , Byr 1938
      , Hgt 178, Iyr 2018, Eyr 2030
      , Ecl 1
      ]
    , [ Hgt 185
      , Eyr 1984, Ecl 1, Pid 1
      , Hcl 0, Byr 2027, Iyr 2017
      ]
    , [ Pid 1, Byr 1957, Hcl 1, Ecl 0, Hgt 63, Eyr 2036, Iyr 1978
      ]
    , [ Hcl 0
      , Eyr 2023, Ecl 1, Hgt 162
      , Iyr 2016, Byr 1938, Pid 0
      ]
    , [ Hcl 1, Iyr 2013, Hgt 189
      , Pid 1, Ecl 1
      , Byr 1930, Eyr 2030
      ]
    , [ Eyr 2026
      , Iyr 2012, Hcl 1, Cid, Pid 1, Ecl 1
      , Hgt 159, Byr 1943
      ]
    , [ Ecl 1
      , Pid 1, Iyr 2016, Hgt 174, Cid, Eyr 2025
      , Hcl 1, Byr 1994
      ]
    , [ Iyr 2011, Pid 1, Ecl 1, Hgt 182, Hcl 1
      , Eyr 2023
      , Byr 1986
      ]
    , [ Hcl 1, Iyr 2010, Byr 1946, Eyr 2021
      , Cid, Pid 1, Hgt 180
      , Ecl 1
      ]
    , [ Iyr 2020
      , Hgt 173, Pid 1
      , Byr 1986
      , Hcl 1
      , Cid
      , Eyr 2025, Ecl 1
      ]
    , [ Hgt 180, Byr 1949
      , Hcl 1, Iyr 2010, Eyr 2030
      , Cid, Pid 1, Ecl 1
      ]
    , [ Iyr 2010, Eyr 2028
      , Pid 1
      , Hgt -63, Ecl 1
      , Byr 1948, Hcl 1
      ]
    ]


puzzleInput8 : List Passport
puzzleInput8 =
    [ [ Iyr 2020, Hcl 1
      , Ecl 1
      , Byr 1922, Pid 1, Eyr 2022, Hgt 177
      ]
    , [ Hcl 1, Eyr 2020, Iyr 2014, Byr 1983
      , Pid 1, Hgt -72
      , Ecl 1
      ]
    , [ Eyr 2023, Pid 1, Ecl 1
      , Hcl 1, Byr 1929, Hgt 167, Iyr 2010
      ]
    , [ Pid 1, Byr 1994, Hcl 1, Iyr 2018, Hgt 177, Ecl 1, Cid
      , Eyr 2024
      ]
    , [ Hcl 0, Iyr 1983
      , Byr 1954, Eyr 2037
      , Ecl 0, Pid 13, Hgt 166
      ]
    , [ Ecl 1
      , Hcl 1, Iyr 2014
      , Hgt 173
      , Byr 1939
      , Pid 1
      , Eyr 2025
      ]
    , [ Eyr 2028, Ecl 1, Hcl 1, Hgt 166, Byr 1938, Pid 1, Iyr 2011
      ]
    , [ Pid 1, Eyr 2024, Iyr 2018, Hgt 165
      , Ecl 1
      , Hcl 1, Cid
      ]
    , [ Cid
      , Pid 1, Hcl 1
      , Iyr 2018
      , Eyr 2027
      , Ecl 1
      , Hgt -66, Byr 1953
      ]
    , [ Hcl 1, Byr 1978
      , Iyr 2013
      , Hgt 180, Eyr 2027, Ecl 1, Pid 1
      ]
    , [ Pid 1, Hgt 155, Ecl 1, Cid, Byr 1936, Iyr 2010
      , Hcl 1, Eyr 2027
      ]
    , [ Ecl 1, Iyr 2024, Hcl 1, Pid 1, Cid, Hgt 186, Byr 1960
      , Eyr 2022
      ]
    , [ Cid, Iyr 2014
      , Byr 2000
      , Pid 1, Eyr 2021, Ecl 1, Hgt 162
      ]
    , [ Byr 1959, Cid
      , Pid 1
      , Ecl 1, Iyr 2018, Eyr 2027, Hgt 185
      ]
    , [ Pid 1, Ecl 1
      , Iyr 2013, Byr 1932
      , Hgt -68, Cid, Eyr 2025, Hcl 1
      ]
    , [ Byr 1927
      , Hgt 72, Ecl 1
      , Eyr 2021, Hcl 1
      , Pid 1, Iyr 2010
      ]
    , [ Byr 1943, Iyr 2011, Eyr 2024, Pid 1, Ecl 1, Hcl 1, Hgt 170
      ]
    , [ Pid 1, Ecl 1
      , Byr 1953, Hcl 1, Iyr 2013, Eyr 2025, Hgt 184
      ]
    , [ Iyr 2017, Eyr 2023, Pid 1
      , Hgt 179
      , Byr 1993, Hcl 1, Ecl 1
      ]
    , [ Ecl 1
      , Hgt 187, Eyr 2024, Byr 1971, Iyr 2020, Hcl 1, Pid 1
      , Cid
      ]
    , [ Pid 1, Ecl 1, Byr 1964
      , Iyr 2018
      , Eyr 2021, Cid
      , Hgt 153, Hcl 1
      ]
    , [ Byr 2002
      , Cid, Iyr 2014, Eyr 2024, Ecl 1, Hcl 1, Hgt 187
      , Pid 1
      ]
    , [ Hgt 178, Pid 1, Eyr 2027, Iyr 2013
      , Byr 1947
      , Hcl 1, Ecl 1
      ]
    , [ Eyr 2025, Ecl 1, Pid 1, Byr 1950
      , Iyr 2015, Hgt 165
      , Hcl 1, Cid
      ]
    , [ Ecl 1, Eyr 2029
      , Iyr 2015
      , Hgt 171, Hcl 1
      , Pid 1, Byr 1997
      ]
    ]


puzzleInput9 : List Passport
puzzleInput9 =
    [ [ Byr 1948, Iyr 2023, Pid 0, Hcl 0, Ecl 0, Eyr 2001
      , Cid
      ]
    , [ Eyr 2036, Hgt 0, Iyr 1957, Byr 1987, Hcl 0
      , Pid 0, Ecl 0
      ]
    , [ Eyr 2024, Hcl 1, Iyr 2017, Ecl 1, Byr 1988, Cid, Hgt 152, Pid 1
      ]
    , [ Iyr 2011, Pid 1
      , Cid, Hcl 1, Byr 1983, Ecl 1, Hgt 158, Eyr 2020
      ]
    , [ Ecl 1, Hgt 187, Eyr 2027, Iyr 2015
      , Hcl 1, Pid 1
      , Byr 1940
      , Cid
      ]
    , [ Hcl 1, Cid, Byr 1925
      , Hgt 155, Iyr 2015, Ecl 1, Eyr 2027, Pid 1
      ]
    , [ Iyr 2016, Ecl 1, Pid 1, Byr 1985, Eyr 2026
      , Hgt 154, Hcl 1
      ]
    , [ Ecl 1, Hcl 1
      , Iyr 1944
      , Pid 1, Eyr 2026, Byr 1922, Hgt 185
      ]
    , [ Iyr 2020, Hcl 1
      , Pid 1, Hgt 165
      , Ecl 1, Eyr 2022
      ]
    , [ Iyr 2014
      , Byr 1957, Hcl 1, Hgt 189
      , Eyr 2023, Pid 1, Ecl 1
      ]
    , [ Ecl 1, Cid, Hgt 170
      , Byr 1952, Hcl 1, Iyr 2020
      , Eyr 2026
      , Pid 1
      ]
    , [ Eyr 2027, Ecl 1
      , Byr 1975, Pid 1
      , Hcl 0, Hgt 157
      , Iyr 2013
      , Cid
      ]
    , [ Ecl 1
      , Hgt 193, Iyr 2015, Hcl 1, Byr 1989, Pid 1, Eyr 2021
      ]
    , [ Ecl 1, Eyr 2025, Hgt -69, Iyr 2014, Cid, Pid 1, Byr 1984, Hcl 1
      ]
    , [ Hcl 0, Byr 2013
      , Ecl 0
      , Pid 14, Hgt 0, Cid, Eyr 1985, Iyr 1935
      ]
    , [ Eyr 2025
      , Iyr 2026, Hgt -190, Pid 0
      , Ecl 0, Hcl 1, Byr 2030
      ]
    , [ Eyr 2029, Hgt 191
      , Byr 1986, Hcl 1, Cid, Pid 1, Iyr 2012, Ecl 1
      ]
    , [ Eyr 2025, Iyr 2017, Ecl 1
      , Hcl 0
      , Pid 0, Hgt 174, Byr 2016
      ]
    , [ Hcl 1
      , Eyr 2021, Ecl 1, Iyr 2013, Pid 1, Byr 1998
      , Hgt 161
      ]
    , [ Eyr 2029, Hgt 163, Byr 1933, Cid, Iyr 2011
      , Ecl 1
      , Hcl 1
      , Pid 1
      ]
    , [ Hgt 190, Eyr 2030, Hcl 1, Iyr 2011, Ecl 1, Pid 1, Byr 1969
      ]
    , [ Ecl 1, Iyr 2011, Eyr 2022
      , Cid
      , Byr 1978, Hgt -69, Hcl 1, Pid 1
      ]
    , [ Ecl 1, Hgt 164, Iyr 2019, Eyr 2027, Pid 1, Hcl 1
      , Byr 1976
      ]
    , [ Eyr 1938
      , Ecl 0, Pid 1
      , Iyr 2030
      , Hgt 184, Hcl 1, Byr 2013
      ]
    , [ Ecl 1, Byr 1997, Hcl 1, Cid, Pid 0
      , Eyr 2023, Hgt -161, Iyr 1936
      ]
    , [ Ecl 1
      , Byr 1938, Pid 1
      , Hgt 162, Iyr 2020
      , Eyr 2028
      , Hcl 1
      ]
    , [ Hgt 162, Cid
      , Hcl 1, Pid 1, Ecl 1, Byr 1980, Eyr 2028, Iyr 2014
      ]
    ]


puzzleInput10 : List Passport
puzzleInput10 =
    [ [ Byr 2007, Hgt -150, Hcl 0
      , Eyr 2032
      , Ecl 0
      , Iyr 2030, Pid 0
      ]
    , [ Ecl 1, Iyr 2017
      , Hcl 1
      , Pid 1, Byr 1976, Hgt 168
      , Eyr 2028
      ]
    , [ Eyr 1922, Ecl 0, Byr 2013, Hcl 1, Pid 0, Iyr 2030
      , Hgt 71
      ]
    , [ Hgt 164, Byr 1949, Ecl 1, Eyr 2026
      , Hcl 1
      ]
    , [ Ecl 1
      , Iyr 2013, Hgt 166, Hcl 1
      , Pid 1
      , Eyr 2030, Byr 1930
      ]
    , [ Cid
      , Iyr 2020, Byr 1978, Pid 1, Eyr 1955
      , Hcl 1
      , Ecl 1, Hgt 165
      ]
    , [ Ecl 1, Iyr 2018, Byr 1953
      , Hgt 177, Pid 1, Eyr 2025, Hcl 1
      ]
    , [ Byr 1984, Pid 1
      , Ecl 1, Hcl 1, Cid, Iyr 2020
      , Hgt 189, Eyr 2024
      ]
    , [ Iyr 2016
      , Ecl 1, Byr 1954
      , Hcl 1, Pid 1, Eyr 2026
      , Hgt 185
      ]
    , [ Byr 1967
      , Eyr 2021, Hcl 1
      , Pid 1, Hgt 158, Iyr 2018, Ecl 1
      ]
    , [ Iyr 2019, Eyr 2030, Pid 1, Hcl 1
      , Cid, Hgt 162, Ecl 1, Byr 1925
      ]
    , [ Eyr 2026, Pid 1, Hcl 1, Byr 1973, Iyr 2016, Ecl 1, Hgt 182
      ]
    , [ Pid 1, Eyr 2024, Hcl 1, Byr 1959
      , Iyr 2014, Hgt 191, Ecl 1
      ]
    , [ Hgt 193, Pid 1, Ecl 1
      , Cid
      , Iyr 2011, Hcl 1, Byr 1954, Eyr 2020
      ]
    , [ Byr 1966, Iyr 2019, Eyr 2025, Ecl 0
      , Hgt 184, Pid 0, Cid, Hcl 1
      ]
    , [ Byr 1955, Hcl 0, Ecl 1
      , Hgt 157, Iyr 2017, Eyr 2021
      , Pid 1
      ]
    , [ Hgt 172
      , Ecl 1
      , Pid 1
      , Hcl 1, Eyr 2030, Byr 1964, Iyr 2013
      ]
    , [ Iyr 2018, Hgt 152, Byr 1948, Hcl 1, Pid 1
      , Ecl 1
      , Eyr 2020
      ]
    , [ Cid
      , Ecl 1
      , Byr 1960, Iyr 2028, Pid 0, Eyr 2033, Hcl 1
      , Hgt 66
      ]
    , [ Iyr 1933, Ecl 0, Pid 0
      , Eyr 2030, Hcl 0, Hgt -154, Byr 2011
      ]
    , [ Cid, Ecl 1, Iyr 2014
      , Hgt 178
      , Byr 1992, Hcl 1, Eyr 2021
      , Pid 1
      ]
    , [ Iyr 2010, Pid 1
      , Ecl 1, Hgt 181, Byr 1980, Hcl 1, Eyr 2028
      ]
    ]
