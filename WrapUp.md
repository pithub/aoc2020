# Advent of Code 2020 - Wrap Up

Now, almost a year after the last commit, I finally took the time
to write a short wrap up of my "Advent of Code 2020 in Roc" adventure...


## Outcome

I have been able to complete every task with Roc! :thumbsup:


## Problems

As expected, I was mostly fighting the very early version of the Roc compiler, more than the AoC tasks themselves.

A big problem was that I couldn't reliably use expressive data structures like nested records and tag unions
without crashing the compiler or getting runtime errors.

* The input to the Roc programs was a ```List I64```: the bytes of the input file.
* The output was a ```List (List I64)```: the results of the test problems and the output of the real problems.
* The datatypes in the Roc programs themselves often ended as being plain ```List I64``` or ```List (List I64)```.

This made the code not very pleasant to look at.
On the other hand, even the most sophisticated data structures are finally represented as a sequence of bytes in memory,
so it was definitely doable.


## A Good Start

Despite the problems when trying to use expressive data structures and to build better abstractions,
the first days went pretty well.

I could solve the first 12 tasks right on the very day after my daily work,
and even got time to write a little bit about the solutions of [Day 2](Day02.md) and [Day 4](Day04.md).


## Stumbling Blocks from Day 13 on

Then came Day 13, where I didn't have the mathematical knowledge to solve the puzzle.
I always want to find a solution without using Google, so I left the problem open for the moment.

From Day 14 on, I needed something like a Map or a Set, which Roc didn't have.

I tried to use the red/black tree from the example folder,
but couldn't get it to compile and run because of the data structure problems.

Then I tried to represent a red/black tree with a - you guess it - ```List I64```.
This was perfectly doable.
Each node occupied a fixed amount of entries in the list,
and links to other nodes were just the index of the first entry of the target nodes (aka pointers).

This implementation kind of worked, and I could partly solve some of the following puzzles.
(Each day's task consists of two parts. For the first part, the problem size is often smaller than that of the second part.)
The ```ListTree``` and ```ListSet``` code worked for a small number of entries,
but got unbaringly slow very soon for larger problem sizes, and used more and more memory until it finally crashed.

For some days, I couldn't solve any of the following problems.

After giving up on the Day 13 problem, with the help of Google I found a way to solve the puzzle,
but that was pretty much all I "achieved".


## The Eye Opener

Then came the problem of Day 23.
In this problem, there are a certain number of labled cups arranged in a circle,
and the task is to repeat certain operations on the cups by moving and swapping them.

The first part was easy: 9 cups, 100 operations, easy.

The second part took the problem to a completely different dimension: 1 million cups and 10 millions operations!
Needless to say that my naive implementation wasn't able to find a solution in a reasonable amount of time.

For a couple of days I tried to find some clever formula to compute the solution, but couldn't find one.
What I found instead was a different representation of the circle of cups,
which reduced the amount of work to do per operation to a small constant.
Not having high hopes because of the sheer size of the problem, I gave the new algorithm a try, and was...

:scream: **Blown Away**

It only took a few seconds to compute the final result!

Besides being happy about finding the solution to part 2 of the Day 23 problem,
I started wondering: how was it possible that I could perform tens of millions of operations
on a list with one million entries in a few seconds,
but my ```ListTree``` was too slow to handle a couple of hundred entries?

I studied the implementation of the ```List``` builtins in the Roc compiler, and then finally found the answer:
the one million cup list was allocated **at once**,
while the nodes in the ```ListTree``` were added ony by one as needed.

Here's the culprit:

```elm
addNode : List I64, I64, I64, I74, I64, I64, I64 -> List I64
addNode = \tree, ptrOut, key, val, col, lft, rgt ->
    node = List.len tree
    newTree = tree
        |> List.append key
        |> List.append val
        |> List.append col
        |> List.append lft
        |> List.append rgt
    setPtrNode newTree ptrOut node
```

For each ```List.append```, Roc was allocating a new list with one more entry than the previous list
and copying each element of the previous list into the new list.
No surprise this was getting slow and using more and more memory!

And the remedy was easy: I added a "configuration" to the ```ListTree```
which specified how many nodes to allocate at once.
The new code for adding a node to the tree finally was:

```elm
addNode : List I64, I64, I64 -> List I64
addNode = \tree, ptrOut, key ->
    node = lastIndex tree
    nodeSize = internalNodeSize tree

    newTree =
        if node < List.len tree then
            tree
        else
            buffer = List.repeat (configNodesToAllocate tree * nodeSize) 0
            List.concat tree buffer

    newTree
        |> setNodeKey node key
        |> setNodeCol node 1
        |> setNodeLft node 0
        |> setNodeRgt node 0
        |> setPtrNode ptrOut node
```

This relatively small change to allocate nodes in chunks had a tremendous effect.


## Finishing Every Task

With the new implementations of ```ListTree``` and ```ListSet``` I was finally able to
finish every task that still had been open.

As written above, the code wasn't very nice to look at, but it worked.

Success! :star:

Here are some more things I used while implementing the solutions:


## Constant Strings as Hashes

There were almost no String builtins back then.
It wasn't possible to compare strings, so I represented strings by their hashes:

```elm
interface Hash exposes [ str ] imports []

str : List I64 -> I64
str = \chars ->
    List.walk chars (\c, h -> h * 33 + c) 5381
```

In the Roc programs, I defined the hashes of constant strings in a record:

```elm
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
```

...and could compare a given string variable (or better: its hash) using code like ```if entry.fld == s.byr then```.


## Parsers and Zippers

Working with ```List I64``` was quickly becoming tedious because ```List.get```
returns a ```Result```, so you always have to deal with the error case.

This led me to implement a list zipper, used together with the list itself,
a record defined as

```elm
Zip : { len: I64, idx: I64, val: I64, default: I64 }
```

storing the length of the list, the current position in the list, the value at this position,
and a default value used when doing a low-level read from the list:

```elm
read : List I64, I64, I64 -> I64
read = \list, idx, default ->
    when List.get list idx is
        Ok n -> n
        _ -> default
```

The ```Zip``` structure together with functions for moving the current position has been very useful.
To read a value I just moved the current position and used the ```val``` field from the ```Zip``` record.


I also tried to implement parsers to transform the incoming ```List I64``` into more useful data structures.
I defined the following types:

```elm
Parser a : List I64, State -> Res a

Res a : { val : a, state : State }

State : ListZip.Zip
```

Using the parsers together with more expressive types turned out to be less useful than expected,
because I got compiler errors not only from using the higher level data structures
but also from the parser code itself.


## AoC 2021

I've heard that a couple of people are planning to use Roc for the upcoming 2021 Advent of Code contest.
(This was the reason for me to finally write this wrap up...)

I wish you all good luck and at least as much fun as I had last year.
I'm certain that the Roc compiler is in a much better shape,
so that you can concentrate more on idiomatic and expressive code in this beautiful language! :heart:
