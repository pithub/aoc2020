interface Day22 exposes [ output ] imports [ ListZip, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData1  = parseDecks testInput1
    testData2  = parseDecks testInput2
    puzzleData = parseDecks puzzleInput

    [ TestUtil.verify 22 1 1 (firstResult testData1 ) 306
    , TestUtil.show   22 1   (firstResult puzzleData)
    , TestUtil.verify 22 2 1 (secondResult testData1 ) 291
    , TestUtil.verify 22 2 2 (secondResult testData2 ) 105
    , TestUtil.show   22 2   (secondResult puzzleData)
    ]


Decks : { cardsA : List I64, cardsB : List I64 }


#  first part


firstResult : Decks -> I64
firstResult = \decks ->
    initial = createGame 1 decks.cardsA 2 decks.cardsB False
    final = playUntilEnd initial
    score final.queueA final.deckA 1 0


#  second part


secondResult : Decks -> I64
secondResult = \decks ->
    final = playGame 1 decks.cardsA 2 decks.cardsB
    score final.queueA final.deckA 1 0


playGame : I64, List I64, I64, List I64 -> Game
playGame = \numA, cardsA, numB, cardsB ->
    game = createGame numA cardsA numB cardsB True
    playUntilEnd game


#  common


Game :
    { recursive : Bool, repeated : Bool, hashes : List I64
    , numA : I64, queueA : ListZip.Queue, deckA : List I64
    , numB : I64, queueB : ListZip.Queue, deckB : List I64
    }


createGame : I64, List I64, I64, List I64, Bool -> Game
createGame = \numA, cardsA, numB, cardsB, recursive ->
    lenA = List.len cardsA
    lenB = List.len cardsB
    deckA = List.concat cardsA (List.repeat lenB 0)
    deckB = List.concat cardsB (List.repeat lenA 0)
    queueA = ListZip.newFromTo deckA 0 (lenA - 1) 0
    queueB = ListZip.newFromTo deckB 0 (lenB - 1) 0

    { recursive, repeated: False, hashes: [], numA, queueA, deckA, numB, queueB, deckB }


playUntilEnd : Game -> Game
playUntilEnd = \game ->
    newGame = playRound game

    if newGame.repeated || ListZip.queueSize newGame.queueB == 0 then
        newGame
    else
        playUntilEnd newGame


playRound : Game -> Game
playRound = \game ->
    newGame = winnerAsA game

    if newGame.repeated then
        newGame
    else
        newQueueB = ListZip.forwardStart newGame.queueB newGame.deckB

        newQueueA1 = ListZip.forwardStart newGame.queueA newGame.deckA
        newQueueA2 = ListZip.forwardEnd newQueueA1 newGame.deckA
        newQueueA3 = ListZip.forwardEnd newQueueA2 newGame.deckA
        newDeckA = newGame.deckA
            |> List.set newQueueA2.endIdx newGame.queueA.startVal
            |> List.set newQueueA3.endIdx newGame.queueB.startVal
        newQueueA = ListZip.updateQueue newQueueA3 newDeckA

        { newGame & queueA: newQueueA, deckA: newDeckA, queueB: newQueueB }


winnerAsA : Game -> Game
winnerAsA = \game ->
    topCardA = game.queueA.startVal
    topCardB = game.queueB.startVal

    if game.recursive then
        newGame = addHash game

        if newGame.repeated then
            if newGame.numA == 1 then
                newGame
            else
                swapPlayer newGame

        else
            lenA = ListZip.queueSize newGame.queueA
            lenB = ListZip.queueSize newGame.queueB

            if topCardA < lenA && topCardB < lenB then
                cardsA = subDeck newGame.deckA newGame.queueA
                cardsB = subDeck newGame.deckB newGame.queueB
                final = playGame newGame.numA cardsA newGame.numB cardsB
                if final.numA == newGame.numA then
                    newGame
                else
                    swapPlayer newGame

            else if topCardA > topCardB then
                newGame
            else
                swapPlayer newGame

    else if topCardA > topCardB then
        game
    else
        swapPlayer game


subDeck : List I64, ListZip.Queue -> List I64
subDeck = \deck, queue ->
    len = queue.startVal
    newQueue = ListZip.forwardStart queue deck
    ListZip.collectQueue deck newQueue len


swapPlayer : Game -> Game
swapPlayer = \game ->
    { game & numA: game.numB, queueA: game.queueB, deckA: game.deckB
    , numB: game.numA, queueB: game.queueA, deckB: game.deckA
    }


addHash : Game -> Game
addHash = \game ->
    playerA = playerHash game.queueA game.deckA
    playerB = playerHash game.queueB game.deckB

    hash =
        if game.numA == 1 then
            listHash [ playerA, playerB ] 0 5301
        else
            listHash [ playerB, playerA ] 0 5301

    if isMember hash game.hashes 0 then
        { game & repeated: True }
    else
        newHashes = List.append game.hashes hash
        { game & repeated: False, hashes: newHashes }


playerHash : ListZip.Queue, List I64 -> I64
playerHash = \queue, deck ->
    len = ListZip.queueSize queue
    cards = ListZip.collectQueue deck queue len
    listHash cards 0 5301


listHash : List I64, I64, I64 -> I64
listHash = \list, idx, result ->
    when List.get list idx is
        Ok element ->
            newIdx = idx + 1
            when (result * 33 + element) % 144115188075855871 is
                Ok newResult ->
                    listHash list newIdx newResult
                _ ->
                    result
        _ ->
            result


isMember : I64, List I64, I64 -> Bool
isMember = \element, list, idx ->
    when List.get list idx is
        Ok n ->
            if n == element then
                True
            else
                newIdx = idx + 1
                isMember element list newIdx
        _ ->
            False


score : ListZip.Queue, List I64, I64, I64 -> I64
score = \queue, deck, factor, result ->
    newQueue = ListZip.backwardEnd queue deck
    newResult = result + queue.endVal * factor
    if ListZip.queueSize newQueue == 0 then
        newResult
    else
        newFactor = factor + 1
        score newQueue deck newFactor newResult


#  parser


parseDecks : List I64 -> Decks
parseDecks = \input ->
    initial = ListZip.newAtFirst input 0
    cardsA = parseCards initial input
    cardsB = parseCards cardsA.zip input
    { cardsA: cardsA.val, cardsB: cardsB.val }


Res a : { zip : ListZip.Zip, val : a }


parseCards : ListZip.Zip, List I64 -> Res (List I64)
parseCards = \zip, input ->
    start = skipLine zip input
    parseInts start input []


skipLine : ListZip.Zip, List I64 -> ListZip.Zip
skipLine = \zip, input ->
    newZip = ListZip.forward zip input
    if zip.val == 10 then
        newZip
    else
        skipLine newZip input


parseInts : ListZip.Zip, List I64, List I64 -> Res (List I64)
parseInts = \zip, input, result ->
    if ListZip.afterLast zip then
        { zip, val: result }
    else if zip.val == 10 then
        newZip = ListZip.forward zip input
        { zip: newZip, val: result }
    else
        int = parseInt zip input 0
        newResult = List.append result int.val
        parseInts int.zip input newResult


parseInt : ListZip.Zip, List I64, I64 -> Res I64
parseInt = \zip, input, num ->
    if ListZip.afterLast zip then
        { zip, val: num }
    else
        newZip = ListZip.forward zip input
        if zip.val == 10 then
            { zip: newZip, val: num }
        else
            newNum = num * 10 + zip.val - 48
            parseInt newZip input newNum


#  test data


testInput1 : List I64
testInput1 =
    [ 80, 108, 97, 121, 101, 114, 32, 49, 58, 10
    , 57, 10
    , 50, 10
    , 54, 10
    , 51, 10
    , 49, 10
    , 10
    , 80, 108, 97, 121, 101, 114, 32, 50, 58, 10
    , 53, 10
    , 56, 10
    , 52, 10
    , 55, 10
    , 49, 48
    ]


testInput2 : List I64
testInput2 =
    [ 80, 108, 97, 121, 101, 114, 32, 49, 58, 10
    , 52, 51, 10
    , 49, 57, 10
    , 10
    , 80, 108, 97, 121, 101, 114, 32, 50, 58, 10
    , 50, 10
    , 50, 57, 10
    , 49, 52
    ]
