interface Day17 exposes [ output ] imports [ TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData   = parseData testInput
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 17 1 1 (firstResult testData  ) 0
    , TestUtil.show   17 1   (firstResult puzzleData)
    , TestUtil.verify 17 2 1 (secondResult testData  ) 0
    , TestUtil.show   17 2   (secondResult puzzleData)
    , [ toN 0, toN -1, toN 1, toN -2, toN 2 ]
    , toIDbg 0
    , toIDbg 1
    , toIDbg 2
    ]


#  todo
#  Vielleicht wäre es effizienter, statt immer einen kompletten Kubus die
#  folgenden Typen zu verwenden:
#
#  IList a : { minIdx : I64, maxIdx : I64, list: List a }
#  Map : Ilist (IList (IList Bool))
#
#  Dann bräuchte man nur die Elemente speichern, die schon mal benutzt worden
#  sind.


toIDbg : I64 -> List I64
toIDbg = \m ->
    toIDbgH m -m -m -m []


toIDbgH : I64, I64, I64, I64, List I64 -> List I64
toIDbgH = \m, x, y, z, r ->
    if x > m then
        r
    else if y > m then
        toIDbgH m (x + 1) -m -m r
    else if z > m then
        toIDbgH m x (y + 1) -m r
    else
        i = toI x y z
        v = (((i * 10) + toN x) * 10 + toN y) * 10 + toN z
        nR = List.append r v
        toIDbgH m x y (z + 1) nR


#  first part


firstResult : List Bool -> I64
firstResult = \map ->
    List.len map


#  second part


secondResult : List Bool -> I64
secondResult = \data ->
    2 * List.len data


#  utils


toI : I64, I64, I64 -> I64
toI = \x, y, z ->
    aX = Num.abs x
    aY = Num.abs y
    aZ = Num.abs z

    d = max3 aX aY aZ
    ddm1 = d + d - 1
    ddp1 = d + d + 1

    nX = toN x
    nY = toN y
    nZ = toN z

    if aX == d then
        (nX * ddp1 + nY) * ddp1 + nZ
    else if aY == d then
        nX * 2 * ddp1 + (nY - ddm1) * ddp1 + nZ + ddm1 * ddm1 * ddm1 + 2 * ddm1 * ddm1
    else
        nX * 2 * ddm1 + nY * 2 + nZ + ddm1 * ddm1 * ddm1 - ddm1


max3 : I64, I64, I64 -> I64
max3 = \aX, aY, aZ ->
    if aX > aY then
        if aX > aZ then
            aX
        else
            aZ
    else if aY > aZ then
        aY
    else
        aZ


toN : I64 -> I64
toN = \n ->
    if n < 0 then
        -1 - n - n
    else if n > 0 then
        n + n
    else
        0


#  parser


parseData : List I64 -> List Bool
parseData = \input ->
    List.walk input parseWalker []


parseWalker : I64, List Bool -> List Bool
parseWalker = \val, acc ->
    when val is
        10 -> acc
        35 -> List.append acc True
        _ -> List.append acc False


#  test data


testInput : List I64
testInput =
    [ 46, 35, 46, 10
    , 46, 46, 35, 10
    , 35, 35, 35
    ]
