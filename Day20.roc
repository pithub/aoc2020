interface Day20 exposes [ output ] imports [ ListTree, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData   = parseData testInput
    puzzleData = parseData puzzleInput

    [ TestUtil.verify 20 1 1 (firstResult testData  ) 20899048083289
    , TestUtil.show   20 1   (firstResult puzzleData)
    , TestUtil.verify 20 2 1 (secondResult testData  ) 273
    , TestUtil.show   20 2   (secondResult puzzleData)
    ]


#  notes

#  edge numbers
#        1
#      +---+
#    2 |   | 3
#      +---+
#        4

#  coordinate transformation given top edge number and left edge number
#  tl   y   x
#  ----------
#  12  +y  +x
#  13  +y  -x
#  21  +x  +y
#  24  -x  +y
#  31  +x  -y
#  34  -x  -y
#  42  -y  +x
#  43  -y  -x

#  testdata tile infos
#   6: 2311 ->  10, 210, 318,  89, 231
#  15: 1951 -> 110, 397, 587, 318, 177
#  24: 1171 -> 210, 399, 391,  18,  24
#  33: 1427 -> 310, 183,   9, 234, 210
#  42: 1489 -> 410,  43, 565,  18, 183
#  51: 2473 -> 510, 481, 399, 116, 234
#  60: 2971 -> 610, 161,  78, 565,  85
#  69: 2729 -> 710,  85, 271,   9, 397
#  78: 3079 -> 810, 501,  89,  66, 116

#  testdata layout
#  15, 4, 2 |  6, 4, 2 | 78, 1, 2
#  69, 4, 2 | 33, 4, 2 | 51, 3, 4
#  60, 4, 2 | 42, 4, 2 | 24, 1, 3

#  testdata image
#  +--------------+--------------+--------------+
#  | 1951  42  52 | 2311  42  48 | 3079  12  48 |
#  +--------------+--------------+--------------+
#  | 1 00011010 0 | 0 01110011 1 | 1 01011111 0 |
#  |              |              |              |
#  | 0 01010010 1 | 1 11000101 0 | 0 10011111 1 |
#  | 0 11100001 0 | 0 01000010 0 | 0 01000000 0 |
#  | 1 11011011 0 | 0 10101001 1 | 1 11111000 0 |
#  | 0 11101111 1 | 1 10001011 1 | 1 11101001 0 |
#  | 0 11010000 1 | 1 10110111 0 | 0 10001011 0 |
#  | 1 00011111 1 | 1 11101000 1 | 1 01111101 1 |
#  | 0 00001001 1 | 1 00011001 0 | 0 01011100 0 |
#  | 1 01111000 1 | 1 10010000 0 | 0 01000000 0 |
#  |              |              |              |
#  | 1 01100011 0 | 0 01101001 0 | 0 01011100 0 |
#  +--------------+--------------+--------------+
#  | 2729  42  46 | 1427  42  49 | 2473  34  58 |
#  +--------------+--------------+--------------+
#  | 1 01100011 0 | 0 01101001 0 | 0 01011100 0 |
#  |              |              |              |
#  | 1 10010110 0 | 0 01001110 1 | 1 10110000 1 |
#  | 1 10111100 0 | 0 10111101 0 | 0 01011100 1 |
#  | 1 11101010 0 | 0 00101111 1 | 1 11010011 1 |
#  | 0 10111100 0 | 0 00110011 0 | 0 11111101 1 |
#  | 0 11001101 0 | 0 00010001 1 | 1 01010100 0 |
#  | 0 00010010 1 | 1 01010110 1 | 1 01110111 0 |
#  | 0 01010000 0 | 0 10110100 1 | 1 01110110 0 |
#  | 1 11101000 0 | 0 10010110 0 | 0 11111100 0 |
#  |              |              |              |
#  | 0 00101010 1 | 1 11011010 0 | 0 11000111 1 |
#  +--------------+--------------+--------------+
#  | 2971  42  50 | 1489  42  44 | 1171  13  53 |
#  +--------------+--------------+--------------+
#  | 0 00101010 1 | 1 11011010 0 | 0 11000111 1 |
#  |              |              |              |
#  | 0 01010111 0 | 0 01101101 1 | 1 00101100 1 |
#  | 0 01111011 1 | 1 10100011 0 | 0 10100101 1 |
#  | 1 00101001 0 | 0 00101010 0 | 0 11110111 0 |
#  | 0 10011110 1 | 1 00101010 1 | 1 11101110 0 |
#  | 0 11111001 1 | 1 11110001 0 | 0 11000011 0 |
#  | 1 10110010 0 | 0 01000100 0 | 0 11110001 0 |
#  | 1 01011100 0 | 0 11001100 0 | 0 11110110 1 |
#  | 1 00011100 0 | 0 01100010 0 | 0 00100111 1 |
#  |              |              |              |
#  | 0 01010000 1 | 1 10101000 0 | 0 00110000 0 |
#  +--------------+--------------+--------------+


#  first part


firstResult : List I64 -> I64
firstResult = \data ->
    dataPositions = getDataPositions data
    edgeInfos = getEdgeInfos data dataPositions
    tileInfos = getTileInfos dataPositions edgeInfos

    firstResultHelper tileInfos edgeInfos (ListTree.firstP tileInfos) 1


firstResultHelper : List I64, List I64, I64, I64 -> I64
firstResultHelper = \tileInfos, edgeInfos, tilePos, result ->
    if tilePos > 0 then
        newTilePos = ListTree.nextP tileInfos tilePos

        newResult =
            if isCorner tileInfos edgeInfos tilePos then
                tileId = ListTree.keyP tileInfos tilePos
                result * tileId
            else
                result

        firstResultHelper tileInfos edgeInfos newTilePos newResult

    else
        result


#  second part


secondResult : List I64 -> I64
secondResult = \data ->
    dataPositions = getDataPositions data
    edgeInfos = getEdgeInfos data dataPositions
    tileInfos = getTileInfos dataPositions edgeInfos
    layout = getLayout tileInfos edgeInfos

    waves = waveCnt data tileInfos layout

    monsters = List.sum
        [ monsterCnt data tileInfos layout 1 2
        , monsterCnt data tileInfos layout 1 3
        , monsterCnt data tileInfos layout 2 1
        , monsterCnt data tileInfos layout 2 4
        , monsterCnt data tileInfos layout 3 1
        , monsterCnt data tileInfos layout 3 4
        , monsterCnt data tileInfos layout 4 2
        , monsterCnt data tileInfos layout 4 3
        ]

    waves - 15 * monsters


getLayout : List I64, List I64 -> List I64
getLayout = \tileInfos, edgeInfos ->
    tilePos = ListTree.firstP tileInfos
    getLayoutFromTopLeft tileInfos edgeInfos tilePos


getLayoutFromTopLeft : List I64, List I64, I64 -> List I64
getLayoutFromTopLeft = \tileInfos, edgeInfos, tilePos ->
    if tilePos > 0 then
        if isCorner tileInfos edgeInfos tilePos then
            firstOuterEdgeNum = getFirstOuterEdgeNum tileInfos edgeInfos tilePos
            secondOuterEdgeNum = getSecondOuterEdgeNum tileInfos edgeInfos tilePos firstOuterEdgeNum
            layout = [ 0, tilePos, secondOuterEdgeNum, firstOuterEdgeNum ]
            addNextTopLayout tileInfos edgeInfos tilePos firstOuterEdgeNum 1 layout
        else
            newTilePos = ListTree.nextP tileInfos tilePos
            getLayoutFromTopLeft tileInfos edgeInfos newTilePos
    else
        []


addNextTopLayout : List I64, List I64, I64, I64, I64, List I64 -> List I64
addNextTopLayout = \tileInfos, edgeInfos, leftTilePos, leftEdgeNum, width, layout ->
    rightTreePos = getOtherEdge tileInfos edgeInfos leftTilePos leftEdgeNum
    rightTilePos = getOtherTile edgeInfos rightTreePos leftTilePos
    if rightTilePos > 0 then
        rightEdgeNum = getEdgeNum edgeInfos rightTreePos rightTilePos
        topEdgeNum = getAdjacentOuterEdge tileInfos edgeInfos rightTilePos rightEdgeNum
        newLayout = List.concat layout [ rightTilePos, topEdgeNum, rightEdgeNum ]
        addNextTopLayout tileInfos edgeInfos rightTilePos rightEdgeNum (width + 1) newLayout
    else
        newLayout = List.set layout 0 width
        addNextRowLayout tileInfos edgeInfos 1 newLayout


addNextRowLayout : List I64, List I64, I64, List I64 -> List I64
addNextRowLayout = \tileInfos, edgeInfos, layoutPos, layout ->
    topTilePos = getSafe layout layoutPos
    topEdgeNum = getSafe layout (layoutPos + 1)
    bottomTreePos = getOtherEdge tileInfos edgeInfos topTilePos topEdgeNum
    bottomTilePos = getOtherTile edgeInfos bottomTreePos topTilePos
    if bottomTilePos > 0 then
        bottomEdgeNum = getEdgeNum edgeInfos bottomTreePos bottomTilePos
        leftEdgeNum = getAdjacentOuterEdge tileInfos edgeInfos bottomTilePos bottomEdgeNum
        newLayout = List.concat layout [ bottomTilePos, bottomEdgeNum, leftEdgeNum ]
        addRemainingLayout tileInfos edgeInfos bottomTilePos leftEdgeNum (layoutPos + 3) newLayout
    else
        layout


addRemainingLayout : List I64, List I64, I64, I64, I64, List I64 -> List I64
addRemainingLayout = \tileInfos, edgeInfos, leftTilePos, leftEdgeNum, layoutPos, layout ->
    rightTreePos = getOtherEdge tileInfos edgeInfos leftTilePos leftEdgeNum
    rightTilePos = getOtherTile edgeInfos rightTreePos leftTilePos
    if rightTilePos > 0 then
        rightEdgeNum = getEdgeNum edgeInfos rightTreePos rightTilePos

        topTilePos = getSafe layout layoutPos
        topEdgeNum = getSafe layout (layoutPos + 1)
        bottomTreePos = getOtherEdge tileInfos edgeInfos topTilePos topEdgeNum
        bottomEdgeNum = getEdgeNum edgeInfos bottomTreePos rightTilePos

        newLayout = List.concat layout [ rightTilePos, bottomEdgeNum, rightEdgeNum ]
        addRemainingLayout tileInfos edgeInfos rightTilePos rightEdgeNum (layoutPos + 3) newLayout
    else
        addNextRowLayout tileInfos edgeInfos layoutPos layout


waveCnt : List I64, List I64, List I64 -> I64
waveCnt = \data, tileInfos, layout ->
    tileCnt = getSafe data 0
    width = getSafe layout 0
    height = divSafe tileCnt width

    maxY = 8 * height - 1
    maxX = 8 * width - 1

    waveCntHelper data tileInfos layout maxY maxX 0 0 0


waveCntHelper : List I64, List I64, List I64, I64, I64, I64, I64, I64 -> I64
waveCntHelper = \data, tileInfos, layout, maxY, maxX, y, x, cnt ->
    if x <= maxX then
        newCnt = cnt + getImageVal data tileInfos layout y x
        waveCntHelper data tileInfos layout maxY maxX y (x + 1) newCnt
    else if y < maxY then
        waveCntHelper data tileInfos layout maxY maxX (y + 1) 0 cnt
    else
        cnt


monsterCnt : List I64, List I64, List I64, I64, I64 -> I64
monsterCnt = \data, tileInfos, layout, topEdgeNum, leftEdgeNum ->
    horizontalMonsterHeight = 3
    horizontalMonsterWidth = 20
    monsterHeight =
        if topEdgeNum == 1 || topEdgeNum == 4 then
            horizontalMonsterHeight
        else
            horizontalMonsterWidth
    monsterWidth = horizontalMonsterHeight + horizontalMonsterWidth - monsterHeight

    monsterCoords =
        getMonsterCoords
            (List.repeat 30 0)
            [ 0, 18
            , 1, 0, 1, 5, 1, 6, 1, 11, 1, 12, 1, 17, 1, 18, 1, 19
            , 2, 1, 2, 4, 2, 7, 2, 10, 2, 13, 2, 16
            ]
            monsterHeight monsterWidth topEdgeNum leftEdgeNum 0

    tileCnt = getSafe data 0
    width = getSafe layout 0
    height = divSafe tileCnt width

    maxY = 8 * height - monsterHeight
    maxX = 8 * width - monsterWidth

    nextMonsterCnt data tileInfos layout maxY maxX topEdgeNum leftEdgeNum 0 0 monsterCoords 0


getMonsterCoords : List I64, List I64, I64, I64, I64, I64, I64 -> List I64
getMonsterCoords = \result, coords, monsterHeight, monsterWidth, topEdgeNum, leftEdgeNum, coordPos ->
    when List.get coords coordPos is
        Ok y ->
            x = getSafe coords (coordPos + 1)
            monsterY =
                if topEdgeNum == 1 then y
                else if topEdgeNum == 4 then monsterHeight - y - 1
                else if leftEdgeNum == 1 then x
                else monsterHeight - x - 1
            monsterX =
                if leftEdgeNum == 2 then x
                else if leftEdgeNum == 3 then monsterWidth - x - 1
                else if topEdgeNum == 2 then y
                else monsterWidth - y - 1
            result
                |> List.set coordPos monsterY
                |> List.set (coordPos + 1) monsterX
                |> getMonsterCoords coords monsterHeight monsterWidth topEdgeNum leftEdgeNum (coordPos + 2)
        _ ->
            result


nextMonsterCnt : List I64, List I64, List I64, I64, I64, I64, I64, I64, I64, List I64, I64 -> I64
nextMonsterCnt = \data, tileInfos, layout, maxY, maxX, topEdgeNum, leftEdgeNum, y, x, monsterCoords, cnt ->
    if x <= maxX then
        newCnt =
            if monsterFound data tileInfos layout topEdgeNum leftEdgeNum y x monsterCoords 0 then
                cnt + 1
            else
                cnt
        nextMonsterCnt data tileInfos layout maxY maxX topEdgeNum leftEdgeNum y (x + 1) monsterCoords newCnt
    else if y < maxY then
        nextMonsterCnt data tileInfos layout maxY maxX topEdgeNum leftEdgeNum (y + 1) 0 monsterCoords cnt
    else
        cnt


monsterFound : List I64, List I64, List I64, I64, I64, I64, I64, List I64, I64 -> Bool
monsterFound = \data, tileInfos, layout, topEdgeNum, leftEdgeNum, y, x, monsterCoords, monsterPos ->
    when List.get monsterCoords monsterPos is
        Ok monsterY ->
            monsterX = getSafe monsterCoords (monsterPos + 1)
            imageVal = getImageVal data tileInfos layout (y + monsterY) (x + monsterX)
            if imageVal > 0 then
                monsterFound data tileInfos layout topEdgeNum leftEdgeNum y x monsterCoords (monsterPos + 2)
            else
                False
        _ ->
            True


getImageVal : List I64, List I64, List I64, I64, I64 -> I64
getImageVal = \data, tileInfos, layout, y, x ->
    width = getSafe layout 0
    tileY = divSafe y 8
    tileX = divSafe x 8

    layoutPos = (width * tileY + tileX) * 3
    tilePos     = getSafe layout (layoutPos + 1)
    topEdgeNum  = getSafe layout (layoutPos + 2)
    leftEdgeNum = getSafe layout (layoutPos + 3)

    dataPos = ListTree.getP tileInfos tilePos 0
    dataY = y - 8 * tileY + 1
    dataX = x - 8 * tileX + 1
    getDataVal data dataPos topEdgeNum leftEdgeNum dataY dataX


getDataVal : List I64, I64, I64, I64, I64, I64 -> I64
getDataVal = \data, dataPos, topEdgeNum, leftEdgeNum, y, x ->
    dataY =
        if topEdgeNum == 1 then y
        else if topEdgeNum == 4 then 9 - y
        else if leftEdgeNum == 1 then x
        else 9 - x
    dataX =
        if leftEdgeNum == 2 then x
        else if leftEdgeNum == 3 then 9 - x
        else if topEdgeNum == 2 then y
        else 9 - y
    getSafe data (dataPos + 10 * dataY + dataX)


#  util


#  tileId -> dataPos, x, x, x, x

getDataPositions : List I64 -> List I64
getDataPositions = \data ->
    tileCnt = getSafe data 0
    dataPositions = ListTree.emptyWithConfig tileCnt 5
    getDataPositionsHelper dataPositions data tileCnt 0


getDataPositionsHelper : List I64, List I64, I64, I64 -> List I64
getDataPositionsHelper = \dataPositions, data, tileCnt, tileNum ->
    if tileNum < tileCnt then
        newTileNum = tileNum + 1
        tileId = getSafe data newTileNum
        dataPos = 1 + tileCnt + 100 * tileNum
        dataPositions
            |> ListTree.insert tileId dataPos
            |> getDataPositionsHelper data tileCnt newTileNum
    else
        dataPositions


#  edgeId -> tilePos 1, edgeNum 1, tilePos 2, edgeNum 2

getEdgeInfos : List I64, List I64 -> List I64
getEdgeInfos = \data, dataPositions ->
    tileCnt = ListTree.size dataPositions
    tilePos = ListTree.firstP dataPositions
    edgeInfos = ListTree.emptyWithConfig tileCnt 4
    getEdgeInfosHelper edgeInfos data dataPositions tilePos


getEdgeInfosHelper : List I64, List I64, List I64, I64 -> List I64
getEdgeInfosHelper = \edgeInfos, data, dataPositions, tilePos ->
    if tilePos > 0 then
        newTilePos = ListTree.nextP dataPositions tilePos
        edgeInfos
            |> addEdgeInfos data dataPositions tilePos
            |> getEdgeInfosHelper data dataPositions newTilePos
    else
        edgeInfos


addEdgeInfos : List I64, List I64, List I64, I64 -> List I64
addEdgeInfos = \edgeInfos, data, dataPositions, tilePos ->
    dataPos = ListTree.getP dataPositions tilePos 0
    edgeInfos
        |> addEdgeInfo data tilePos 1 (dataPos +  0) (dataPos +  9)  1 10 0 0
        |> addEdgeInfo data tilePos 2 (dataPos +  0) (dataPos + 90) 10 10 0 0
        |> addEdgeInfo data tilePos 3 (dataPos +  9) (dataPos + 99) 10 10 0 0
        |> addEdgeInfo data tilePos 4 (dataPos + 90) (dataPos + 99)  1 10 0 0


addEdgeInfo : List I64, List I64, I64, I64, I64, I64, I64, I64, I64, I64 -> List I64
addEdgeInfo = \edgeInfos, data, tilePos, edgeNum, pa, pe, pd, cnt, e1, e2 ->
    if cnt > 0 then
        newE1 = 2 * e1 + getSafe data pa
        newE2 = 2 * e2 + getSafe data pe
        addEdgeInfo edgeInfos data tilePos edgeNum (pa + pd) (pe - pd) pd (cnt - 1) newE1 newE2
    else
        edgeId = if e1 < e2 then e1 else e2
        if ListTree.find edgeInfos edgeId > 0 then
            edgeInfos
                |> ListTree.insertN edgeId 2 tilePos
                |> ListTree.insertN edgeId 3 edgeNum
        else
            edgeInfos
                |> ListTree.insertP edgeId
                |> ListTree.setI 0 tilePos
                |> ListTree.setI 1 edgeNum


isOuterEdge : List I64, I64 -> Bool
isOuterEdge = \edgeInfos, edgeId ->
    when ListTree.getN edgeInfos edgeId 2 is
        Ok tilePos -> tilePos == 0
        _ -> False


getOtherTile : List I64, I64, I64 -> I64
getOtherTile = \edgeInfos, edgePos, tilePos ->
    otherTilePos = ListTree.getP edgeInfos edgePos 2
    if otherTilePos == tilePos then
        ListTree.getP edgeInfos edgePos 0
    else
        otherTilePos


getEdgeNum : List I64, I64, I64 -> I64
getEdgeNum = \edgeInfos, edgePos, tilePos ->
    if ListTree.getP edgeInfos edgePos 2 == tilePos then
        ListTree.getP edgeInfos edgePos 3
    else
        ListTree.getP edgeInfos edgePos 1


#  tile id -> tile pos, edge id 1, edge id 2, edge id 3, edge id 4

getTileInfos : List I64, List I64 -> List I64
getTileInfos = \dataPositions, edgeInfos ->
    ListTree.walkP edgeInfos
        (\edgeId, tree, pos, tileInfos ->
            tilePos1 = ListTree.getP tree pos 0
            edgeNum1 = ListTree.getP tree pos 1
            newTileInfos = ListTree.setP tileInfos tilePos1 edgeNum1 edgeId
            tilePos2 = ListTree.getP tree pos 2
            if tilePos2 > 0 then
                edgeNum2 = ListTree.getP tree pos 3
                ListTree.setP newTileInfos tilePos2 edgeNum2 edgeId
            else
                newTileInfos
        )
        dataPositions


getOtherEdge : List I64, List I64, I64, I64 -> I64
getOtherEdge = \tileInfos, edgeInfos, tilePos, edgeNum ->
    otherEdgeNum = 5 - edgeNum
    otherEdgeId = ListTree.getP tileInfos tilePos otherEdgeNum
    ListTree.find edgeInfos otherEdgeId


getFirstOuterEdgeNum : List I64, List I64, I64 -> I64
getFirstOuterEdgeNum = \tileInfos, edgeInfos, tilePos ->
    getOuterEdgeNumHelper tileInfos edgeInfos tilePos 1


getSecondOuterEdgeNum : List I64, List I64, I64, I64 -> I64
getSecondOuterEdgeNum = \tileInfos, edgeInfos, tilePos, firstOuterEdgeNum ->
    getOuterEdgeNumHelper tileInfos edgeInfos tilePos (firstOuterEdgeNum + 1)


getOuterEdgeNumHelper : List I64, List I64, I64, I64 -> I64
getOuterEdgeNumHelper = \tileInfos, edgeInfos, tilePos, edgeNum ->
    if edgeNum > 4 then
        0
    else
        edgeId = ListTree.getP tileInfos tilePos edgeNum
        if isOuterEdge edgeInfos edgeId then
            edgeNum
        else
            getOuterEdgeNumHelper tileInfos edgeInfos tilePos (edgeNum + 1)


isCorner : List I64, List I64, I64 -> Bool
isCorner = \tileInfos, edgeInfos, tilePos ->
    firstOuterEdgeNum = getFirstOuterEdgeNum tileInfos edgeInfos tilePos
    firstOuterEdgeNum > 0 && getSecondOuterEdgeNum tileInfos edgeInfos tilePos firstOuterEdgeNum > 0


getAdjacentOuterEdge : List I64, List I64, I64, I64 -> I64
getAdjacentOuterEdge = \tileInfos, edgeInfos, tilePos, edgeNum ->
    if edgeNum == 1 || edgeNum == 4 then
        getAdjacentOuterEdgeHelper tileInfos edgeInfos tilePos 2 3
    else if edgeNum == 2 || edgeNum == 3 then
        getAdjacentOuterEdgeHelper tileInfos edgeInfos tilePos 1 4
    else
        0


getAdjacentOuterEdgeHelper : List I64, List I64, I64, I64, I64 -> I64
getAdjacentOuterEdgeHelper = \tileInfos, edgeInfos, tilePos, edgeNum1, edgeNum2 ->
    edgeId1 = ListTree.getP tileInfos tilePos edgeNum1
    if isOuterEdge edgeInfos edgeId1 then
        edgeNum1
    else
        edgeId2 = ListTree.getP tileInfos tilePos edgeNum2
        if isOuterEdge edgeInfos edgeId2 then
            edgeNum2
        else
            0


getSafe : List I64, I64 -> I64
getSafe = \list, idx ->
    when List.get list idx is
        Ok val -> val
        _ -> 0


divSafe : I64, I64 -> I64
divSafe = \n, m ->
    when n // m is
        Ok q -> q
        _ -> 0


#  parser


parseData : List I64 -> List I64
parseData = \input ->
    parseDataHelper input 0 0 [] []


parseDataHelper : List I64, I64, I64, List I64, List I64 -> List I64
parseDataHelper = \input, idx, id, ids, data ->
    when List.get input idx is
        Ok val ->
            newIdx = idx + 1

            if 48 <= val && val <= 57 then
                newId = id * 10 + val - 48
                parseDataHelper input newIdx newId ids data

            else
                newIds =
                    if val == 58 then
                        List.append ids id
                    else
                        ids

                newData =
                    if val == 35 then
                        List.append data 1
                    else if val == 46 then
                        List.append data 0
                    else
                        data

                parseDataHelper input newIdx 0 newIds newData

        _ ->
            List.join [ [ List.len ids ], ids, data ]


#  test data


testInput : List I64
testInput =
    [ 84, 105, 108, 101, 32, 50, 51, 49, 49, 58, 10
    , 46, 46, 35, 35, 46, 35, 46, 46, 35, 46, 10
    , 35, 35, 46, 46, 35, 46, 46, 46, 46, 46, 10
    , 35, 46, 46, 46, 35, 35, 46, 46, 35, 46, 10
    , 35, 35, 35, 35, 46, 35, 46, 46, 46, 35, 10
    , 35, 35, 46, 35, 35, 46, 35, 35, 35, 46, 10
    , 35, 35, 46, 46, 46, 35, 46, 35, 35, 35, 10
    , 46, 35, 46, 35, 46, 35, 46, 46, 35, 35, 10
    , 46, 46, 35, 46, 46, 46, 46, 35, 46, 46, 10
    , 35, 35, 35, 46, 46, 46, 35, 46, 35, 46, 10
    , 46, 46, 35, 35, 35, 46, 46, 35, 35, 35, 10
    , 10
    , 84, 105, 108, 101, 32, 49, 57, 53, 49, 58, 10
    , 35, 46, 35, 35, 46, 46, 46, 35, 35, 46, 10
    , 35, 46, 35, 35, 35, 35, 46, 46, 46, 35, 10
    , 46, 46, 46, 46, 46, 35, 46, 46, 35, 35, 10
    , 35, 46, 46, 46, 35, 35, 35, 35, 35, 35, 10
    , 46, 35, 35, 46, 35, 46, 46, 46, 46, 35, 10
    , 46, 35, 35, 35, 46, 35, 35, 35, 35, 35, 10
    , 35, 35, 35, 46, 35, 35, 46, 35, 35, 46, 10
    , 46, 35, 35, 35, 46, 46, 46, 46, 35, 46, 10
    , 46, 46, 35, 46, 35, 46, 46, 35, 46, 35, 10
    , 35, 46, 46, 46, 35, 35, 46, 35, 46, 46, 10
    , 10
    , 84, 105, 108, 101, 32, 49, 49, 55, 49, 58, 10
    , 35, 35, 35, 35, 46, 46, 46, 35, 35, 46, 10
    , 35, 46, 46, 35, 35, 46, 35, 46, 46, 35, 10
    , 35, 35, 46, 35, 46, 46, 35, 46, 35, 46, 10
    , 46, 35, 35, 35, 46, 35, 35, 35, 35, 46, 10
    , 46, 46, 35, 35, 35, 46, 35, 35, 35, 35, 10
    , 46, 35, 35, 46, 46, 46, 46, 35, 35, 46, 10
    , 46, 35, 46, 46, 46, 35, 35, 35, 35, 46, 10
    , 35, 46, 35, 35, 46, 35, 35, 35, 35, 46, 10
    , 35, 35, 35, 35, 46, 46, 35, 46, 46, 46, 10
    , 46, 46, 46, 46, 46, 35, 35, 46, 46, 46, 10
    , 10
    , 84, 105, 108, 101, 32, 49, 52, 50, 55, 58, 10
    , 35, 35, 35, 46, 35, 35, 46, 35, 46, 46, 10
    , 46, 35, 46, 46, 35, 46, 35, 35, 46, 46, 10
    , 46, 35, 46, 35, 35, 46, 35, 46, 46, 35, 10
    , 35, 46, 35, 46, 35, 46, 35, 35, 46, 35, 10
    , 46, 46, 46, 46, 35, 46, 46, 46, 35, 35, 10
    , 46, 46, 46, 35, 35, 46, 46, 35, 35, 46, 10
    , 46, 46, 46, 35, 46, 35, 35, 35, 35, 35, 10
    , 46, 35, 46, 35, 35, 35, 35, 46, 35, 46, 10
    , 46, 46, 35, 46, 46, 35, 35, 35, 46, 35, 10
    , 46, 46, 35, 35, 46, 35, 46, 46, 35, 46, 10
    , 10
    , 84, 105, 108, 101, 32, 49, 52, 56, 57, 58, 10
    , 35, 35, 46, 35, 46, 35, 46, 46, 46, 46, 10
    , 46, 46, 35, 35, 46, 46, 46, 35, 46, 46, 10
    , 46, 35, 35, 46, 46, 35, 35, 46, 46, 46, 10
    , 46, 46, 35, 46, 46, 46, 35, 46, 46, 46, 10
    , 35, 35, 35, 35, 35, 46, 46, 46, 35, 46, 10
    , 35, 46, 46, 35, 46, 35, 46, 35, 46, 35, 10
    , 46, 46, 46, 35, 46, 35, 46, 35, 46, 46, 10
    , 35, 35, 46, 35, 46, 46, 46, 35, 35, 46, 10
    , 46, 46, 35, 35, 46, 35, 35, 46, 35, 35, 10
    , 35, 35, 35, 46, 35, 35, 46, 35, 46, 46, 10
    , 10
    , 84, 105, 108, 101, 32, 50, 52, 55, 51, 58, 10
    , 35, 46, 46, 46, 46, 35, 35, 35, 35, 46, 10
    , 35, 46, 46, 35, 46, 35, 35, 46, 46, 46, 10
    , 35, 46, 35, 35, 46, 46, 35, 46, 46, 46, 10
    , 35, 35, 35, 35, 35, 35, 46, 35, 46, 35, 10
    , 46, 35, 46, 46, 46, 35, 46, 35, 46, 35, 10
    , 46, 35, 35, 35, 35, 35, 35, 35, 35, 35, 10
    , 46, 35, 35, 35, 46, 35, 46, 46, 35, 46, 10
    , 35, 35, 35, 35, 35, 35, 35, 35, 46, 35, 10
    , 35, 35, 46, 46, 46, 35, 35, 46, 35, 46, 10
    , 46, 46, 35, 35, 35, 46, 35, 46, 35, 46, 10
    , 10
    , 84, 105, 108, 101, 32, 50, 57, 55, 49, 58, 10
    , 46, 46, 35, 46, 35, 46, 46, 46, 46, 35, 10
    , 35, 46, 46, 46, 35, 35, 35, 46, 46, 46, 10
    , 35, 46, 35, 46, 35, 35, 35, 46, 46, 46, 10
    , 35, 35, 46, 35, 35, 46, 46, 35, 46, 46, 10
    , 46, 35, 35, 35, 35, 35, 46, 46, 35, 35, 10
    , 46, 35, 46, 46, 35, 35, 35, 35, 46, 35, 10
    , 35, 46, 46, 35, 46, 35, 46, 46, 35, 46, 10
    , 46, 46, 35, 35, 35, 35, 46, 35, 35, 35, 10
    , 46, 46, 35, 46, 35, 46, 35, 35, 35, 46, 10
    , 46, 46, 46, 35, 46, 35, 46, 35, 46, 35, 10
    , 10
    , 84, 105, 108, 101, 32, 50, 55, 50, 57, 58, 10
    , 46, 46, 46, 35, 46, 35, 46, 35, 46, 35, 10
    , 35, 35, 35, 35, 46, 35, 46, 46, 46, 46, 10
    , 46, 46, 35, 46, 35, 46, 46, 46, 46, 46, 10
    , 46, 46, 46, 46, 35, 46, 46, 35, 46, 35, 10
    , 46, 35, 35, 46, 46, 35, 35, 46, 35, 46, 10
    , 46, 35, 46, 35, 35, 35, 35, 46, 46, 46, 10
    , 35, 35, 35, 35, 46, 35, 46, 35, 46, 46, 10
    , 35, 35, 46, 35, 35, 35, 35, 46, 46, 46, 10
    , 35, 35, 46, 46, 35, 46, 35, 35, 46, 46, 10
    , 35, 46, 35, 35, 46, 46, 46, 35, 35, 46, 10
    , 10
    , 84, 105, 108, 101, 32, 51, 48, 55, 57, 58, 10
    , 35, 46, 35, 46, 35, 35, 35, 35, 35, 46, 10
    , 46, 35, 46, 46, 35, 35, 35, 35, 35, 35, 10
    , 46, 46, 35, 46, 46, 46, 46, 46, 46, 46, 10
    , 35, 35, 35, 35, 35, 35, 46, 46, 46, 46, 10
    , 35, 35, 35, 35, 46, 35, 46, 46, 35, 46, 10
    , 46, 35, 46, 46, 46, 35, 46, 35, 35, 46, 10
    , 35, 46, 35, 35, 35, 35, 35, 46, 35, 35, 10
    , 46, 46, 35, 46, 35, 35, 35, 46, 46, 46, 10
    , 46, 46, 35, 46, 46, 46, 46, 46, 46, 46, 10
    , 46, 46, 35, 46, 35, 35, 35, 46, 46, 46
    ]
