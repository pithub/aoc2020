
# Advent of Code 2020 - Day 2

## Task

The puzzle input is a list of 1000 lines in the following format:
```text
1-3 a: abcde
````
Two numbers, a lowercase character and a string of lowercase characters. (In the AoC story those lines contain passwords and data to check the validity of the passwords.)

You have to determine the number of valid lines.

&nbsp;

## Preparation

In my tests before the beginning of AoC 2020 I noticed many `malloc`/`free` errors when using Roc strings, so I decided beforehand to translate strings in the puzzle input into lists of integers.

I wrote a little `sed` script which translates the characters in the given lines into numbers ('a' -> 1, 'b' -> 2, and so on) and use the output as a list literal in the Roc code.

&nbsp;

## Data Structure

### Tag Unions

At first, I tried to use the following Tag Union:

```elm
[ Password Int Int Int (List Int) ]
```
with the two numbers, the encoded character and the list of encoded characters in the password. The line shown above would have been translated by the `sed` script into
```elm
, Password 1 3 1 [ 1, 2, 3, 4, 5 ]
```

Unfortunately, the compiler didn't like the resulting code. I don't remember the exact errors, but eventually I'll add branches to this repository with my trials.

&nbsp;

### Records

OK, this didn't work, but the various `Int` Parameters of the `Password` Tag Union are not very clear anyway. So I tried to use records with named fields next.

I changed the `sed` script to translate the lines into records like
```elm
, { min: 1, max: 3, char: 1, password: [ 1, 2, 3, 4, 5] }
```
This didn't work either. Compiler errors again.

&nbsp;

### Plain Lists of Ints

Finally I decided to use the data structure that has been the most stable one in my preliminary tests: plain lists of integers.

In the first part of the challenge, it's necessary to count how often the given character occurs in the given string (password). But if I have a plain list of ints, how do I restrict the counting to the part of the list which contains the password? I don't want to count the number "1" nor the given character "a". I don't know how to get a sublist in Roc yet, so I decided to encode the two numbers and the given character to negative integers, the characters in the password to positive integers. Hence, the final `sed` script translates the given input line into
```elm
, [ -1, -3, -1, 1, 2, 3, 4, 5 ]
```
This way, to count the number of "a"s in the password, I can simply count the number of "1"s in the whole list.

Finally, the compiler let me get one step further: counting the number of occurrences of a given character (integer) in the password (list of integers)...

&nbsp;

## Counting

### List.keepIf and List.len

The shortest code I could think of was
```elm
countMember : List Int, Int -> Int
countMember = \list, element ->
    isGivenElement = \e -> e == element
    list |> List.keepIf isGivenElement |> List.len
```
But the compiler complained, that `List.keepIf` needs one more parameter. I didn't know which one...

&nbsp;

### List.walk

Ok, I can count myself using `List.walk`:
```elm
countMember : List Int, Int -> Int
countMember = \list, element ->
    counter = \e, count ->
        if e == element then
            count + 1
        else
            count

    List.walk list counter 0
```
No success, this version yields

> Invalid function basic value enum or layout for List.keepIf ...

Ah, I remember this error. It relates to closures. Folkert wrote that we can use closures only in `List.map` yet.

&nbsp;

### List.map and List.walk

So, what should I map the list elements to in order to be able to count the occurrences of the given value with `List.walk`? If I subtract the relevant value from every element I can simply count the number of zeroes:
```elm
countMember : List Int, Int -> Int
countMember = \list, element ->
    mapper = \e -> e - element

    counter = \e, count ->
        if e == 0 then
            count + 1
        else
            count

    list |> List.map mapper |> List.walk counter 0
```

This seems to work. On to the next task...

&nbsp;

## Extracting the Validation Parameters

### Getter Function

In order to extract the three validation parameters from each list, I wrote the following function (remember that those parameters are encoded as negative numbers):
```elm
getField : List Int, Int -> Int
getField = \line, position ->
    when List.get line position is
        Ok n -> -n
        _ -> 0
```
Extracting the parameters was Ok, but I got errors if I wanted to use the list of integers afterwards. (It seems that I can't reproduce the errors when writing this text, though, so maybe I failed somewhere else.)

&nbsp;

### Inlined Getter

I remembered from my preliminary tests that sometimes it helps to manually "inline" getter functions, so instead of
```elm
    min = getField line 0
```
I wrote
```elm
    min = #getField line 0
        when List.get line 0 is
            Ok n -> -n
            _ -> 0
```
and with this implementation the test input yielded the expected result.

Now using the real puzzle input...

&nbsp;

## Real Puzzle Input

### 1000 Lines

When I tried to run the program with a list of 1000 integer lists, I saw nothing on the terminal for about a minute, and then a stack overflow error on my 2013 Mac Book Pro.

&nbsp;

### 8 x 125 Lines

Fortunately, in this case it is easy to split the input into smaller chunks, run the code on each chunk and combine the results. After a couple of binary splits I found, that 125 lines can be processed on my machine.

Instead of one list `realInput` with 1000 elements I now use 8 lists `realInput1` .. `realInput8` with 125 elements each. It still needs some time to run, but there's no stack overflow anymore.

Now I could solve the first part of the Day 2 puzzle :-)

For the second part of the puzzle, we need to change the validation logic a little bit, but the logic is even simpler, so it was easy to solve the second part, too.

&nbsp;

## Improvements

### Counting Revisited

Wen writing this text, I noticed that the counting method could easily be simplified by using `List.sum` instead of `List.walk`:
```elm
countMember : List Int, Int -> Int
countMember = \list, element ->
    mapper = \e -> if e == element then 1 else 0
    list |> List.map mapper |> List.sum
```

&nbsp;

### DRY

As written above, I actually could extract the logic to access the three validation parameters into a its own function. I added a new commit with some refactorings.

Another possible improvement: both parts of the puzzle can use the same code. Only the logic to validate a given input line is different. So I tried to pass the validation predicate as a parameter to the rest of the program, but then I got the error

> thread 'main' panicked at 'not yet implemented: TODO gracefully handle unused import {TestUtil} from module Day02', compiler/load/src/file.rs:2294:13

which is surprising, because I still use the `TestUtil` module.

&nbsp;

### --optimize

Trying to build the program with the `--optimize` parameter panics with

> thread '<unnamed>' panicked at 'assertion failed: `(left == right)`
  left: `FunctionPointer([Builtin(List(Unique, Builtin(Int64))), Builtin(Int64)], Union([[Builtin(Int64)], [Builtin(Int64), Builtin(Int64)]]))`,
 right: `FunctionPointer([Builtin(List(Refcounted, Builtin(Int64))), Builtin(Int64)], Union([[Builtin(Int64)], [Builtin(Int64), Builtin(Int64)]]))`', compiler/mono/src/ir.rs:5016:41

I've got this error before with the above mentioned getter functions. This works:
```elm
main : List (List Int)
main =
    list = [ 1, 2, 3, 4 ]

    [
        List.set list 0 (
            when List.get list 3 is
                Ok n -> n
                _ -> 0
        )
    ]
```
But when I extract the `List.get` call into a getter function, it panics when run with the `--optimize` parameter:
```elm
main : List (List Int)
main =
    getter = \l, i -> List.get l i

    list = [ 1, 2, 3, 4 ]

    [
        List.set list 0 (
            when getter list 3 is
                Ok n -> n
                _ -> 0
        )
    ]
```

Recap: even having to wait more than 60 seconds to build the program, and not being able to apply all improvements I'd like to, having a way to compute the solution is more than enough for me at this early stage of the Roc language.

Btw: the compiled program runs in 0.013 seconds :-)