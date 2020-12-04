
# Advent of Code 2020 - Day 4

## Task

The puzzle input consists of lines representing passport entries
with empty lines between the passports:
```text
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
````

You have to determine the number of valid passports.

&nbsp;

## Part 1

### Data Structure

I decided to use the following types:
```elm
Field : [ Field Int Str ]

Passport : List Field
```

A `Field` stores its "Kind" as an `Int` and the value as a `Str`.
The encoding for the field kind is
```text
byr -> 1 (Birth Year)
iyr -> 2 (Issue Year)
eyr -> 3 (Expiration Year)
hgt -> 4 (Height)
hcl -> 5 (Hair Color)
ecl -> 6 (Eye Color)
pid -> 7 (Passport ID)
cid -> 8 (Country ID)
```

&nbsp;

### Input Data

To transform the puzzle input into valid Roc literals I again used a `sed` script as I did in
[Day 2](Day02.md), a `vim` substitution command (being too lazy to hunt for a problem in my `sed`
file) and manually tweaked the begin and the end of the list literals.

Because the input has been well-formed, I could successfully transform it into valid Roc
literals. The sample input above was transformed into
```elm
[ [ Field 6 "gry", Field 7 "860033327", Field 3 "2020", Field 5 "#fffffd"
  , Field 1 "1937", Field 2 "2017", Field 8 "147", Field 4 "183cm"
  ]
, [ Field 2 "2013", Field 6 "amb", Field 8 "350", Field 3 "2023", Field 7 "028048884"
  , Field 5 "#cfa07d", Field 1 "1929"
  ]
, [ Field 5 "#ae17e1", Field 2 "2013"
  , Field 3 "2024"
  , Field 6 "brn", Field 7 "760753108", Field 1 "1931"
  , Field 4 "179cm"
  ]
, [ Field 5 "#cfa07d", Field 3 "2025", Field 7 "166559648"
  , Field 2 "2011", Field 6 "brn", Field 4 "59in"
  ]
]
```

&nbsp;

### Counting

The problem I had in [Day 2](Day02.md) with count functions using `List.keepIf` could be solved, so for counting I used functions like
```elm
countValidPassports : List Passport -> Int
countValidPassports = \passports ->
    List.keepIf passports validPassport |> List.len
```

Note to Richard: I didn't use the lastest version of `trunk` because I got `malloc`/`free` problems again, so I simply named my predicates without a leading "is". It seems that the current `trunk` works, if I build with the `--optimize` flag, which I didn't during writing the puzzle code.

&nbsp;

### Validation Logic

With all this in place it was pretty simple to count the valid passports:
```elm
validPassport : Passport -> Bool
validPassport = \passport ->
    List.len passport == 8 || (List.len passport == 7 && missingCid passport)


missingCid : Passport -> Bool
missingCid = \passport ->
    (List.keepIf passport cidField |> List.len) == 0


cidField : Field -> Bool
cidField = \field ->
    when field is
        Field 8 _ -> True
        _ -> False
```

Success with the test input!

&nbsp;

### Long Puzzle Input

Like at [Day 2](Day02.md), the puzzle input was too long for a single list literal. I again split the list into multiple sublists.

In the [Day 2](Day02.md) code I calculated the solution for each sublist and then combined the results into the final value. Depending on the problem, this isn't always straightforward or even impossible. This time I used `List.join` (which works for all element types, btw) to combine the sublists into a single list. It is the compiler who has problems with long lists, not the runtime, so this is working pretty well.

Success with the puzzle input, too!

&nbsp;

## Part 2

### Data Structure

In part 2, the validation logic has to be changed. Now each field type has its own rules for valid formats and valid content. For example, a valid Birth Year has to be a 4 digit number between 1920 and 2002.

I changed the `Field` data structure into
```elm
Field : [ Byr Int, Iyr Int, Eyr Int, Hgt Int, Hcl Int, Ecl Int, Pid Int, Cid ]
```

&nbsp;

### Validating the Syntax

I didn't know whether validating the format of the value strings is already possible with Roc, so I decided to do the format validation externally using a modified `sed` script. Validation of the valid numbers should happen in Roc though.

Syntactically valid values are encoded as the following numbers:
```text
byr -> number
iyr -> number
eyr -> number
hgt -> positive number for "cm", negative number for "in"
hcl -> 1
ecl -> 1
pid -> 1
cid -> (has no significant value)
```
Syntactically invalid values are encoded as the number 0.

Again, the simple `sed` script worked even for the complete puzzle input. The test input was transformed into the following Roc literal:
```elm
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
```

&nbsp;

### Validating the Values

Adding the new validation logic to the Roc code was straightforward:
```elm
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
```

Success for Day 4 !

&nbsp;

## Improvements

### Generalized Counting

Trying to extract the counting logic into a generic function didn't work when run with the `--optimize` flag:
```elm
countIf : List a, (a -> Bool) -> Int
countIf = \list, predicate ->
    List.keepIf list predicate |> List.len


countValidPassports1 : List Passport -> Int
countValidPassports1 = \passports ->
    countIf passports validPassport1
```
results in multiple errors:

```text
Mismatch in compiler/unify/src/unify.rs Line 227 Column 13
trying to unify Apply(`List.List`, [32973]) with rigid var 'a'

Mismatch in compiler/unify/src/unify.rs Line 1099 Column 13
Rigid 60 with Alias(`Day04.Passport`, [], 33115)
```

and later

```text
The 2nd argument to countIf is not what I expect:

26│      countIf passports validPassport1
                           ^^^^^^^^^^^^^^

This validPassport1 value is a:

    Attr Shared (Attr a List (Attr b Field) -> Attr x Bool)

But countIf needs the 2nd argument to be:

    Attr Shared (Attr a a -> Attr x Bool)
```

&nbsp;

### Using more String Functions

With the current improvements in implementing the builtin functions, maybe it would be possible to use more of the `Str` functions instead of doing so much preprocessing outside of the Roc code. If I have time, I'll investigate this more.