interface Day24 exposes [ output ] imports [ TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testData   = parseData testInput
    puzzleData = parseData puzzleInput

    blacks = firstResultHelper testData [] 0 0 0 0 []

    map0 = emptyMap
    map1 = blacksToMap blacks 0 map0

    x = -1
    y = -2

    #t1 = getTile map1 (x + 1) (y - 1)
    t2 = getTile map1 (x + 0) (y + 1)
    #t3 = getTile map1 (x + 1) (y + 0)
    #t4 = getTile map1 (x - 1) (y + 1)
    #t5 = getTile map1 (x + 0) (y - 1)
    #t6 = getTile map1 (x - 1) (y + 0)

    #  -3, 2 | -3, 3 | -2, 0 | 0, -1 | -2, 1 | 3, -3 | 0, 2 | 0, 0 | 2, 0 | -1, -1
    #
    #  -3-3    -2-3    -1-3    =0-3    +1-3    +2-3   [+3-3]
    #      -3-2    -2-2    -1-2    =0-2    +1-2    +2-2    +3-2
    #          -3-1    -2-1   [-1-1]  [=0-1]   +1-1    +2-1    +3-1
    #              -3=0   [-2=0]   -1=0   [=0=0]   +1=0   [+2=0]   +3=0
    #                  -3+1   [-2+1]   -1+1    =0+1    +1+1    +2+1    +3+1
    #                     [-3+2]   -2+2    -1+2   [=0+2]   +1+2    +2+2    +3+2
    #                         [-3+3]   -2+3    -1+3    =0+3    +1+3    +2+3    +3+3

    #n1 = countNeighbours map1 -1 -2 0 0
    #n2 = countNeighbours map1 -4  2 0 0
    #n3 = countNeighbours map1 -4  3 0 0
    #n4 = countNeighbours map1 -1  0 0 0

    #map2 = flipMap map1

    [ TestUtil.verify 24 1 1 (firstResult testData  ) 10
    , TestUtil.show   24 1   (firstResult puzzleData)
    , TestUtil.verify 24 2 1 (secondResult testData  ) 2208
    , TestUtil.show   24 2   (secondResult puzzleData)
    #, dbgPos blacks 0 []
    #, dbgPosI blacks 0 []
    #, dbgMap map1
    #, [ n1, n2, n3, n4 ]
    , [ t2 ]
    ]


#dbgPos : List Pos, I64, List I64 -> List I64
#dbgPos = \posList, idx, result ->
#    when List.get posList idx is
#        Ok pos ->
#            newIdx = idx + 1
#            newResult = result |> List.append pos.x |> List.append pos.y
#            dbgPos posList newIdx newResult
#        _ ->
#            result


#dbgPosI : List Pos, I64, List I64 -> List I64
#dbgPosI = \posList, idx, result ->
#    when List.get posList idx is
#        Ok pos ->
#            newIdx = idx + 1
#            newResult = result |> List.append (toI pos.x pos.y)
#            dbgPosI posList newIdx newResult
#        _ ->
#            result


#dbgToI : I64, I64, I64, List I64 -> List I64
#dbgToI = \d, x, y, r ->
#    if x <= d then
#        if y <= d then
#            dbgToI d x (y + 1) (List.append r (toI x y))
#        else
#            dbgToI d (x + 1) -d r
#    else
#        r


#dbgMap : TileMap -> List I64
#dbgMap = \map ->
#    List.concat [ map.minX, map.maxX, map.minY, map.maxY, -9 ] map.tiles


Dir : { dx : I64, dy : I64 }

Line : List Dir

Data : List Line


#  first part


firstResult : Data -> I64
firstResult = \data ->
    final = firstResultHelper data [] 0 0 0 0 []
    List.len final


Pos : { x : I64, y : I64 }


firstResultHelper : Data, Line, I64, I64, I64, I64, List Pos -> List Pos
firstResultHelper = \data, line, lineIdx, dirIdx, x, y, blacks ->
    when List.get line dirIdx is
        Ok dir ->
            newDirIdx = dirIdx + 1
            newX = x + dir.dx
            newY = y + dir.dy
            firstResultHelper data line lineIdx newDirIdx newX newY blacks
        _ ->
            newLineIdx = lineIdx + 1
            newBlacks =
                if lineIdx > 0 then
                    swapTile blacks x y 0 0 []
                else
                    blacks
            when List.get data newLineIdx is
                Ok newLine ->
                    firstResultHelper data newLine newLineIdx 0 0 0 newBlacks
                _ ->
                    newBlacks


swapTile : List Pos, I64, I64, I64, I64, List Pos -> List Pos
swapTile = \blacks, x, y, idx, found, result ->
    when List.get blacks idx is
        Ok pos ->
            same = if pos.x == x && pos.y == y then 1 else 0
            newIdx = idx + 1
            newFound = found + same
            newResult =
                if same > 0 then
                    result
                else
                    List.join [ result, [ pos ] ]
            swapTile blacks x y newIdx newFound newResult
        _ ->
            if found > 0 then
                result
            else
                pos = { x, y }
                List.join [ result, [ pos ] ]


#  second part


secondResult : Data -> I64
secondResult = \data ->
    #blacks = firstResultHelper data [] 0 0 0 0 []
    #map = blacksToMap blacks 0 [ [] ]
    List.len data


blacksToMap : List Pos, I64, TileMap -> TileMap
blacksToMap = \blacks, idx, map ->
    when List.get blacks idx is
        Ok pos ->
            newIdx = idx + 1
            newMap = setTile map pos.x pos.y 1
            blacksToMap blacks newIdx newMap
        _ ->
            map


#flipMap : TileMap -> TileMap
#flipMap = \map ->
#    initial = { minX: 0, maxX: 0, minY: 0, maxY: 0, tiles: [ 0 ] }
#    flipMapHelper map (map.minX - 1) (map.minY - 1) initial


#flipMapHelper : TileMap, I64, I64, TileMap -> TileMap
#flipMapHelper = \map, x, y, result ->
#    if y <= map.maxY + 1 then
#        val = getTile map x y
#        cnt = countNeighbours map x y 0 0
#
#        newResult =
#            if val == 1 && (cnt == 0 || cnt > 2) then
#                setTile result x y 0
#            else if val == 0 && cnt == 2 then
#                setTile result x y 1
#            else
#                setTile result x y val
#        
#        flipMapHelper map x (y + 1) newResult
#    
#    else if x <= map.maxX then
#        flipMapHelper map (x + 1) (map.minY - 1) result
#    
#    else
#        result


#countNeighbours : TileMap, I64, I64, I64, I64 -> I64
#countNeighbours = \map, x, y, idx, cnt ->
#    when List.get neighbours idx is
#        Ok dir ->
#            nX = x + dir.dx
#            nY = y + dir.dy
#            val = getTile map nX nY
#            countNeighbours map x y (idx + 1) (cnt + val)
#        _ ->
#            cnt


#neighbours : List Dir
#neighbours =
#    [ { dx:  1, dy: -1 }, { dx: 0, dy:  1 }, { dx:  1, dy: 0 }
#    , { dx: -1, dy:  1 }, { dx: 0, dy: -1 }, { dx: -1, dy: 0 }
#    ]


#  map


TileMap : { minX : I64, maxX : I64, minY : I64, maxY : I64, tiles : List I64 }


emptyMap : TileMap
emptyMap =
    { minX: 0, maxX: 0, minY: 0, maxY: 0, tiles: [ 0 ] }


setTile : TileMap, I64, I64, I64 -> TileMap
setTile = \map, x, y, val ->
    if map.minX <= x && x <= map.maxX && map.minY <= y && y <= map.maxY then
        { map & tiles: List.set map.tiles (toI x y) val }
    else if val > 0 then
        minX = min x map.minX
        maxX = max x map.maxX
        minY = min y map.minY
        maxY = max y map.maxY

        oldD = max (max (Num.abs map.minX) (Num.abs map.maxX)) (max (Num.abs map.minY) (Num.abs map.maxY))
        newD = max (max (Num.abs minX) (Num.abs maxX)) (max (Num.abs minY) (Num.abs maxY))

        if newD > oldD then
            oldDdp1 = oldD + oldD + 1
            newDdp1 = newD + newD + 1
            oldLen = oldDdp1 * oldDdp1
            newLen = newDdp1 * newDdp1
            added = List.repeat (newLen - oldLen) 0

            { minX, maxX, minY, maxY
            , tiles: map.tiles |> List.concat added |> List.set (toI x y) val
            }
        else
            { minX, maxX, minY, maxY
            , tiles: List.set map.tiles (toI x y) val
            }
    else
        map


getTile : TileMap, I64, I64 -> I64
getTile = \map, x, y ->
    if map.minX <= x && x <= map.maxX && map.minY <= y && y <= map.maxY then
        when List.get map.tiles (toI x y) is
            Ok val ->
                val
            _ ->
                0
    else
        0


toI : I64, I64 -> I64
toI = \x, y ->
    aX = Num.abs x
    aY = Num.abs y

    d = max aX aY

    nX = toN x
    nY = toN y

    if aX == d then
        ddp1 = d + d + 1
        nX * ddp1 + nY
    else
        ddm1 = d + d - 1
        nX * 2 + (nY - ddm1) + ddm1 * ddm1


max : I64, I64 -> I64
max = \aX, aY ->
    if aX > aY then
        aX
    else
        aY


min : I64, I64 -> I64
min = \aX, aY ->
    if aX < aY then
        aX
    else
        aY


toN : I64 -> I64
toN = \n ->
    if n < 0 then
        -1 - n - n
    else if n > 0 then
        n + n
    else
        0


#  parser


parseData : List I64 -> Data
parseData = \input ->
    initial = { first: 0, line: [], result: [ [] ] }  #  compiler error
    last = List.walk input parseWalker initial
    final = parseWalker 10 last
    final.result


ParseAcc : { first : I64, line : Line, result : Data }


parseWalker : I64, ParseAcc -> ParseAcc
parseWalker = \val, acc ->
    when val is
        101 ->
            dir =
                when acc.first is
                    110 ->
                        { dx: 1, dy: -1 }
                    115 ->
                        { dx: 0, dy: 1 }
                    _ ->
                        { dx: 1, dy: 0 }
            newLine = List.join [ acc.line, [ dir ] ]
            { acc & first: 0, line: newLine }
        110 ->
            { acc & first: val }
        115 ->
            { acc & first: val }
        119 ->
            dir =
                when acc.first is
                    110 ->
                        { dx: 0, dy: -1 }
                    115 ->
                        { dx: -1, dy: 1 }
                    _ ->
                        { dx: -1, dy: 0 }
            newLine = List.join [ acc.line, [ dir ] ]
            { acc & first: 0, line: newLine }
        _ ->
            newResult = List.join [ acc.result, [ acc.line ] ]
            { acc & line: [], result: newResult }


#  test data


testInput : List I64
testInput =
   [ 115, 101, 115, 101, 110, 119, 110, 101, 110, 101, 110, 101, 119, 115, 101, 101, 115, 119, 119, 115, 119, 115, 119, 119, 110, 101, 110, 101, 119, 115, 101, 119, 115, 119, 10
   , 110, 101, 101, 101, 110, 101, 115, 101, 110, 119, 110, 119, 119, 115, 119, 110, 101, 110, 101, 119, 110, 119, 119, 115, 101, 119, 110, 101, 110, 119, 115, 101, 115, 119, 101, 115, 119, 10
   , 115, 101, 115, 119, 110, 101, 115, 119, 115, 119, 115, 101, 110, 119, 119, 110, 119, 115, 101, 10
   , 110, 119, 110, 119, 110, 101, 115, 101, 101, 115, 119, 115, 119, 110, 101, 110, 101, 119, 110, 101, 115, 119, 119, 110, 101, 119, 115, 101, 115, 119, 110, 101, 115, 101, 101, 110, 101, 10
   , 115, 119, 119, 101, 115, 119, 110, 101, 115, 119, 110, 101, 110, 119, 115, 101, 119, 110, 119, 110, 101, 110, 101, 115, 101, 101, 110, 119, 10
   , 101, 101, 115, 101, 110, 119, 115, 101, 115, 119, 115, 119, 110, 101, 110, 119, 115, 119, 110, 119, 110, 119, 115, 101, 119, 119, 110, 119, 115, 101, 110, 101, 10
   , 115, 101, 119, 110, 101, 110, 101, 110, 101, 110, 101, 115, 101, 110, 119, 115, 101, 119, 110, 101, 110, 119, 119, 119, 115, 101, 10
   , 119, 101, 110, 119, 119, 119, 101, 115, 101, 101, 101, 119, 101, 115, 119, 119, 119, 110, 119, 119, 101, 10
   , 119, 115, 119, 101, 101, 115, 101, 110, 101, 110, 101, 119, 110, 119, 119, 110, 119, 115, 101, 110, 101, 119, 115, 101, 110, 119, 119, 115, 101, 115, 101, 115, 101, 110, 119, 110, 101, 10
   , 110, 101, 101, 115, 119, 115, 101, 101, 110, 119, 119, 115, 119, 110, 119, 115, 119, 115, 119, 110, 119, 10
   , 110, 101, 110, 119, 115, 119, 119, 115, 101, 119, 115, 119, 110, 101, 110, 101, 110, 101, 119, 115, 101, 110, 119, 115, 101, 110, 119, 110, 101, 115, 101, 115, 101, 110, 101, 119, 10
   , 101, 110, 101, 119, 110, 119, 101, 119, 110, 101, 115, 119, 115, 101, 119, 110, 119, 115, 119, 101, 110, 119, 101, 115, 119, 110, 101, 110, 119, 115, 101, 110, 119, 115, 119, 10
   , 115, 119, 101, 110, 101, 115, 119, 110, 101, 115, 119, 110, 101, 110, 101, 101, 110, 119, 110, 101, 119, 101, 110, 101, 119, 119, 110, 101, 115, 119, 115, 119, 110, 101, 115, 101, 10
   , 115, 119, 119, 101, 115, 101, 110, 101, 115, 101, 119, 101, 110, 119, 110, 101, 115, 119, 110, 119, 119, 110, 101, 115, 101, 115, 119, 119, 110, 101, 10
   , 101, 110, 101, 115, 101, 110, 119, 115, 119, 119, 115, 119, 110, 101, 110, 101, 115, 119, 115, 101, 110, 119, 110, 101, 119, 115, 119, 115, 101, 101, 110, 119, 115, 101, 115, 101, 10
   , 119, 110, 119, 110, 101, 115, 101, 110, 101, 115, 101, 110, 101, 110, 119, 119, 110, 101, 110, 119, 115, 101, 119, 101, 115, 101, 119, 115, 101, 115, 101, 115, 101, 119, 10
   , 110, 101, 110, 101, 119, 115, 119, 110, 119, 101, 119, 115, 119, 110, 101, 110, 101, 115, 101, 110, 119, 110, 101, 115, 101, 119, 101, 115, 119, 10
   , 101, 110, 101, 115, 119, 110, 119, 115, 119, 110, 119, 115, 101, 110, 101, 110, 119, 110, 119, 110, 119, 119, 115, 101, 101, 115, 119, 110, 101, 101, 119, 115, 101, 110, 101, 115, 101, 10
   , 110, 101, 115, 119, 110, 119, 101, 119, 110, 119, 110, 119, 115, 101, 101, 110, 119, 115, 101, 101, 115, 101, 119, 115, 101, 110, 119, 115, 119, 101, 101, 119, 101, 10
   , 119, 115, 101, 119, 101, 101, 101, 110, 119, 110, 101, 115, 101, 110, 119, 119, 119, 115, 119, 110, 101, 119
   ]
