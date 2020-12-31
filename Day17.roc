interface Day17 exposes [ output ] imports [ ListZip, TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testCube   = parseCube 3 3 testInput
    puzzleCube = parseCube 8 8 puzzleInput

    testC4   = parseC4 3 3 testInput
    puzzleC4 = parseC4 8 8 puzzleInput

    [ TestUtil.verify 17 1 1 (firstResult testCube  ) 112
    , TestUtil.show   17 1   (firstResult puzzleCube)
    , TestUtil.verify 17 2 1 (secondResult testC4  ) 848
    , TestUtil.show   17 2   (secondResult puzzleC4)
    ]


#  first part


firstResult : Cube -> I64
firstResult = \cube ->
    bufSum (repeatCycles cube 6)


repeatCycles : Cube, I64 -> Cube
repeatCycles = \cube, cnt ->
    if cnt > 0 then
        repeatCycles (cycle cube) (cnt - 1)
    else
        cube


cycle : Cube -> Cube
cycle = \cube ->
    xl = safeGet cube 2
    yl = safeGet cube 7
    zl = safeGet cube 12
    newCube = empty (xl + 2) (yl + 2) (zl + 2) 0

    xa = safeGet cube 4
    ya = safeGet cube 9
    za = safeGet cube 14

    xMin = xa - 1
    yMin = ya - 1
    zMin = za - 1

    xMax = xa + xl
    yMax = ya + yl
    zMax = za + zl

    cycleHelper cube newCube xMin xMax xMin yMin yMax yMin zMin zMax zMin


cycleHelper : Cube, Cube, I64, I64, I64, I64, I64, I64, I64, I64, I64 -> Cube
cycleHelper = \oldCube, cube, xMin, xMax, x, yMin, yMax, y, zMin, zMax, z ->
    newCube =
        if val oldCube x y z -1 -1 -1 0 0 > 0 then
            setIp cube (x - xMin + 1) (y - yMin + 1) (z - zMin + 1) 1
        else
            cube
    if z < zMax then
        cycleHelper oldCube newCube xMin xMax x yMin yMax y zMin zMax (z + 1)
    else if y < yMax then
        cycleHelper oldCube newCube xMin xMax x yMin yMax (y + 1) zMin zMax zMin
    else if x < xMax then
        cycleHelper oldCube newCube xMin xMax (x + 1) yMin yMax yMin zMin zMax zMin
    else
        newCube


val : Cube, I64, I64, I64, I64, I64, I64, I64, I64 -> I64
val = \cube, x, y, z, xd, yd, zd, v, n ->
    if xd == 0 && yd == 0 && zd == 0 then
        newV = get cube x y z
        val cube x y z 0 0 1 newV n
    else
        newN = n + get cube (x + xd) (y + yd) (z + zd)
        if zd < 1 then
            val cube x y z xd yd (zd + 1) v newN
        else if yd < 1 then
            val cube x y z xd (yd + 1) -1 v newN
        else if xd < 1 then
            val cube x y z (xd + 1) -1 -1 v newN
        else if (v == 1 && newN == 2) || newN == 3 then
            1
        else
            0


Cube : List I64

#{ dv : I64
#, xc : I64, xl : I64, xo : I64, xa : I64, xe : I64
#, yc : I64, yl : I64, yo : I64, ya : I64, ye : I64
#, zc : I64, zl : I64, zo : I64, za : I64, ze : I64
#}


empty : I64, I64, I64, I64 -> Cube
empty = \xc, yc, zc, dv ->
    List.concat
        (emptyCub xc yc zc dv)
        (emptyBuf xc yc zc dv)


emptyCub : I64, I64, I64, I64 -> List I64
emptyCub = \xc, yc, zc, dv ->
        [ dv, xc, 0, 0, 0, 0, yc, 0, 0, 0, 0, zc, 0, 0, 0, 0 ]


emptyBuf : I64, I64, I64, I64 -> List I64
emptyBuf = \xc, yc, zc, dv ->
    List.repeat (xc * yc * zc) dv


#cubeClr : Cube a, I64, I64, I64 -> Cube a
#cubeClr = \cube, _x, _y, _z ->
#    # todo
#    cube


copy : Cube -> Cube
copy = \cube ->
    List.join [ cube ]


set : Cube, I64, I64, I64, I64 -> Cube
set = \cube, x, y, z, v ->
    cube |> copy |> setIp x y z v


setIp : Cube, I64, I64, I64, I64 -> Cube
setIp = \cube, x, y, z, v ->
    cube
        |> setCub x y z
        |> setBuf x y z v


setCub : Cube, I64, I64, I64 -> Cube
setCub = \cube, x, y, z ->
    xl = safeGet cube 2
    yl = safeGet cube 7
    zl = safeGet cube 12

    if xl > 0 then
        xc = safeGet cube 1
        yc = safeGet cube 6
        zc = safeGet cube 11

        xa = safeGet cube 4
        ya = safeGet cube 9
        za = safeGet cube 14

        xd = x - xa
        yd = y - ya
        zd = z - za

        if xl - xd > xc || xd >= xc || yl - yd > yc || yd >= yc || zl - zd > zc || zd >= zc then
            cube
                |> List.set  5 x
                |> List.set 10 y
                |> List.set 15 z

        else if xd < 0 || xd >= xl || yd < 0 || yd >= yl || zd < 0 || zd >= zl then
            xo = safeGet cube 3
            yo = safeGet cube 8
            zo = safeGet cube 13

            xp = xo + xd
            yp = yo + yd
            zp = zo + zd

            cube
                |> List.set  2 (if xd >= xl then xd + 1 else if xd >= 0 then xl else xl - xd)
                |> List.set  7 (if yd >= yl then yd + 1 else if yd >= 0 then yl else yl - yd)
                |> List.set 12 (if zd >= zl then zd + 1 else if zd >= 0 then zl else zl - zd)
                |> List.set  3 (if xd >= 0 then xo else if xp >= 0 then xp else xp + xc )
                |> List.set  8 (if yd >= 0 then yo else if yp >= 0 then yp else yp + yc )
                |> List.set 13 (if zd >= 0 then zo else if zp >= 0 then zp else zp + zc )
                |> List.set  4 (if xd >= 0 then xa else x)
                |> List.set  9 (if yd >= 0 then ya else y)
                |> List.set 14 (if zd >= 0 then za else z)

        else
            cube

    else
        cube
            |> List.set  2 1
            |> List.set  7 1
            |> List.set 12 1
            |> List.set  4 x
            |> List.set  9 y
            |> List.set 14 z


setBuf : Cube, I64, I64, I64, I64 -> Cube
setBuf = \cube, x, y, z, v ->
    pos = bufPos cube x y z
    List.set cube pos v


get : Cube, I64, I64, I64 -> I64
get = \cube, x, y, z ->
    pos = bufPos cube x y z
    if pos < 0 then
        safeGet cube 0
    else
        when List.get cube pos is
            Ok v ->
                v
            _ ->
                safeGet cube 0


bufPos : Cube, I64, I64, I64 -> I64
bufPos = \cube, x, y, z ->
    xa = safeGet cube  4
    ya = safeGet cube  9
    za = safeGet cube 14

    xd = x - xa
    yd = y - ya
    zd = z - za

    if xd < 0 || yd < 0 || zd < 0 then
        -1

    else
        xl = safeGet cube  2
        yl = safeGet cube  7
        zl = safeGet cube 12

        if xd >= xl || yd >= yl || zd >= zl then
            -1

        else
            xc = safeGet cube  1
            yc = safeGet cube  6
            zc = safeGet cube 11

            xo = safeGet cube  3
            yo = safeGet cube  8
            zo = safeGet cube 13

            xp = xo + xd
            yp = yo + yd
            zp = zo + zd

            ((if xp >= xc then xp - xc else xp)  * yc +
             (if yp >= yc then yp - yc else yp)) * zc +
             (if zp >= zc then zp - zc else zp)  + 16


sum : Cube -> I64
sum = \cube ->
    List.sum cube


cubSum : Cube -> I64
cubSum = \cube ->
    safeGet cube  0 +
    safeGet cube  1 +
    safeGet cube  2 +
    safeGet cube  3 +
    safeGet cube  4 +
    safeGet cube  5 +
    safeGet cube  6 +
    safeGet cube  7 +
    safeGet cube  8 +
    safeGet cube  9 +
    safeGet cube 10 +
    safeGet cube 11 +
    safeGet cube 12 +
    safeGet cube 13 +
    safeGet cube 14 +
    safeGet cube 15


bufSum : Cube -> I64
bufSum = \cube ->
    sum cube - cubSum cube


#  second part


secondResult : C4 -> I64
secondResult = \c4 ->
    sumB4 (repeatCycles4 c4 6)


repeatCycles4 : C4, I64 -> C4
repeatCycles4 = \c4, cnt ->
    if cnt > 0 then
        repeatCycles4 (cycle4 c4) (cnt - 1)
    else
        c4


cycle4 : C4 -> C4
cycle4 = \c4 ->
    wl = safeGet c4  2
    xl = safeGet c4  7
    yl = safeGet c4 12
    zl = safeGet c4 17
    newC4 = empty4 (wl + 2) (xl + 2) (yl + 2) (zl + 2) 0

    wa = safeGet c4  4
    xa = safeGet c4  9
    ya = safeGet c4 14
    za = safeGet c4 19

    wMin = wa - 1
    xMin = xa - 1
    yMin = ya - 1
    zMin = za - 1

    wMax = wa + wl
    xMax = xa + xl
    yMax = ya + yl
    zMax = za + zl

    cycle4Helper c4 newC4 wMin wMax wMin xMin xMax xMin yMin yMax yMin zMin zMax zMin


cycle4Helper : C4, C4, I64, I64, I64, I64, I64, I64, I64, I64, I64, I64, I64, I64 -> C4
cycle4Helper = \oldC4, c4, wMin, wMax, w, xMin, xMax, x, yMin, yMax, y, zMin, zMax, z ->
    newC4 =
        if val4 oldC4 w x y z -1 -1 -1 -1 0 0 > 0 then
            setIp4 c4 (w - wMin + 1) (x - xMin + 1) (y - yMin + 1) (z - zMin + 1) 1
        else
            c4
    if z < zMax then
        cycle4Helper oldC4 newC4 wMin wMax w xMin xMax x yMin yMax y zMin zMax (z + 1)
    else if y < yMax then
        cycle4Helper oldC4 newC4 wMin wMax w xMin xMax x yMin yMax (y + 1) zMin zMax zMin
    else if x < xMax then
        cycle4Helper oldC4 newC4 wMin wMax w xMin xMax (x + 1) yMin yMax yMin zMin zMax zMin
    else if w < wMax then
        cycle4Helper oldC4 newC4 wMin wMax (w + 1) xMin xMax xMin yMin yMax yMin zMin zMax zMin
    else
        newC4


val4 : C4, I64, I64, I64, I64, I64, I64, I64, I64, I64, I64 -> I64
val4 = \c4, w, x, y, z, wd, xd, yd, zd, v, n ->
    if wd == 0 && xd == 0 && yd == 0 && zd == 0 then
        newV = get4 c4 w x y z
        val4 c4 w x y z 0 0 0 1 newV n
    else
        newN = n + get4 c4 (w + wd) (x + xd) (y + yd) (z + zd)
        if zd < 1 then
            val4 c4 w x y z wd xd yd (zd + 1) v newN
        else if yd < 1 then
            val4 c4 w x y z wd xd (yd + 1) -1 v newN
        else if xd < 1 then
            val4 c4 w x y z wd (xd + 1) -1 -1 v newN
        else if wd < 1 then
            val4 c4 w x y z (wd + 1) -1 -1 -1 v newN
        else if (v == 1 && newN == 2) || newN == 3 then
            1
        else
            0


C4 : List I64


empty4 : I64, I64, I64, I64, I64 -> Cube
empty4 = \wc, xc, yc, zc, dv ->
    List.concat
        (emptyC4 wc xc yc zc dv)
        (emptyB4 wc xc yc zc dv)


emptyC4 : I64, I64, I64, I64, I64 -> List I64
emptyC4 = \wc, xc, yc, zc, dv ->
        [ dv, wc, 0, 0, 0, 0, xc, 0, 0, 0, 0, yc, 0, 0, 0, 0, zc, 0, 0, 0, 0 ]


emptyB4 : I64, I64, I64, I64, I64 -> List I64
emptyB4 = \wc, xc, yc, zc, dv ->
    List.repeat (wc * xc * yc * zc) dv


copy4 : C4 -> C4
copy4 = \c4 ->
    List.join [ c4 ]


set4 : C4, I64, I64, I64, I64, I64 -> C4
set4 = \c4, w, x, y, z, v ->
    c4 |> copy4 |> setIp4 w x y z v


setIp4 : C4, I64, I64, I64, I64, I64 -> C4
setIp4 = \c4, w, x, y, z, v ->
    c4
        |> setC4 w x y z
        |> setB4 w x y z v


setC4 : C4, I64, I64, I64, I64 -> C4
setC4 = \c4, w, x, y, z ->
    wl = safeGet c4  2
    xl = safeGet c4  7
    yl = safeGet c4 12
    zl = safeGet c4 17

    if wl > 0 then
        wc = safeGet c4  1
        xc = safeGet c4  6
        yc = safeGet c4 11
        zc = safeGet c4 16

        wa = safeGet c4  4
        xa = safeGet c4  9
        ya = safeGet c4 14
        za = safeGet c4 19

        wd = w - wa
        xd = x - xa
        yd = y - ya
        zd = z - za

        if wl - wd > wc || wd >= wc || xl - xd > xc || xd >= xc || yl - yd > yc || yd >= yc || zl - zd > zc || zd >= zc then
            c4
                |> List.set  5 w
                |> List.set 10 x
                |> List.set 15 y
                |> List.set 20 z

        else if wd < 0 || wd >= wl || xd < 0 || xd >= xl || yd < 0 || yd >= yl || zd < 0 || zd >= zl then
            wo = safeGet c4  3
            xo = safeGet c4  8
            yo = safeGet c4 13
            zo = safeGet c4 18

            wp = wo + wd
            xp = xo + xd
            yp = yo + yd
            zp = zo + zd

            c4
                |> List.set  2 (if wd >= wl then wd + 1 else if wd >= 0 then wl else wl - wd)
                |> List.set  7 (if xd >= xl then xd + 1 else if xd >= 0 then xl else xl - xd)
                |> List.set 12 (if yd >= yl then yd + 1 else if yd >= 0 then yl else yl - yd)
                |> List.set 17 (if zd >= zl then zd + 1 else if zd >= 0 then zl else zl - zd)
                |> List.set  3 (if wd >= 0 then wo else if wp >= 0 then wp else wp + wc )
                |> List.set  8 (if xd >= 0 then xo else if xp >= 0 then xp else xp + xc )
                |> List.set 13 (if yd >= 0 then yo else if yp >= 0 then yp else yp + yc )
                |> List.set 18 (if zd >= 0 then zo else if zp >= 0 then zp else zp + zc )
                |> List.set  4 (if wd >= 0 then wa else w)
                |> List.set  9 (if xd >= 0 then xa else x)
                |> List.set 14 (if yd >= 0 then ya else y)
                |> List.set 19 (if zd >= 0 then za else z)

        else
            c4

    else
        c4
            |> List.set  2 1
            |> List.set  7 1
            |> List.set 12 1
            |> List.set 17 1
            |> List.set  4 w
            |> List.set  9 x
            |> List.set 14 y
            |> List.set 19 z


setB4 : C4, I64, I64, I64, I64, I64 -> C4
setB4 = \c4, w, x, y, z, v ->
    pos = posB4 c4 w x y z
    List.set c4 pos v


get4 : C4, I64, I64, I64, I64 -> I64
get4 = \c4, w, x, y, z ->
    pos = posB4 c4 w x y z
    if pos < 0 then
        safeGet c4 0
    else
        when List.get c4 pos is
            Ok v ->
                v
            _ ->
                safeGet c4 0


posB4 : C4, I64, I64, I64, I64 -> I64
posB4 = \c4, w, x, y, z ->
    wa = safeGet c4  4
    xa = safeGet c4  9
    ya = safeGet c4 14
    za = safeGet c4 19

    wd = w - wa
    xd = x - xa
    yd = y - ya
    zd = z - za

    if wd < 0 || xd < 0 || yd < 0 || zd < 0 then
        -1

    else
        wl = safeGet c4  2
        xl = safeGet c4  7
        yl = safeGet c4 12
        zl = safeGet c4 17

        if wd >= wl || xd >= xl || yd >= yl || zd >= zl then
            -1

        else
            wc = safeGet c4  1
            xc = safeGet c4  6
            yc = safeGet c4 11
            zc = safeGet c4 16

            wo = safeGet c4  3
            xo = safeGet c4  8
            yo = safeGet c4 13
            zo = safeGet c4 18

            wp = wo + wd
            xp = xo + xd
            yp = yo + yd
            zp = zo + zd

            (((if wp >= wc then wp - wc else wp)  * xc +
              (if xp >= xc then xp - xc else xp)) * yc +
              (if yp >= yc then yp - yc else yp)) * zc +
              (if zp >= zc then zp - zc else zp)  + 21


sum4 : C4 -> I64
sum4 = \c4 ->
    List.sum c4


sumC4 : C4 -> I64
sumC4 = \c4 ->
    safeGet c4  0 +
    safeGet c4  1 +
    safeGet c4  2 +
    safeGet c4  3 +
    safeGet c4  4 +
    safeGet c4  5 +
    safeGet c4  6 +
    safeGet c4  7 +
    safeGet c4  8 +
    safeGet c4  9 +
    safeGet c4 10 +
    safeGet c4 11 +
    safeGet c4 12 +
    safeGet c4 13 +
    safeGet c4 14 +
    safeGet c4 15 +
    safeGet c4 16 +
    safeGet c4 17 +
    safeGet c4 18 +
    safeGet c4 19 +
    safeGet c4 20


sumB4 : C4 -> I64
sumB4 = \c4 ->
    sum4 c4 - sumC4 c4


#  utils


safeGet : Cube, I64 -> I64
safeGet = \cube, idx ->
    when List.get cube idx is
        Ok v -> v
        _ -> -1


#  parser


parseCube : I64, I64, List I64 -> Cube
parseCube = \yc, zc, input ->
    cube = empty 1 yc zc 0
    zip = ListZip.newAtFirst input 0
    parseCubeHelper zip input cube 1 1


parseCubeHelper : ListZip.Zip, List I64, Cube, I64, I64 -> Cube
parseCubeHelper = \zip, input, cube, y, z ->
    if ListZip.afterLast zip then
        cube
    else
        newZip = ListZip.forward zip input
        when zip.val is
            35 ->
                newCube = set cube 1 y z 1
                parseCubeHelper newZip input newCube y (z + 1)
            46 ->
                parseCubeHelper newZip input cube y (z + 1)
            _ ->
                parseCubeHelper newZip input cube (y + 1) 1


parseC4 : I64, I64, List I64 -> C4
parseC4 = \yc, zc, input ->
    c4 = empty4 1 1 yc zc 0
    zip = ListZip.newAtFirst input 0
    parseC4Helper zip input c4 1 1


parseC4Helper : ListZip.Zip, List I64, C4, I64, I64 -> C4
parseC4Helper = \zip, input, c4, y, z ->
    if ListZip.afterLast zip then
        c4
    else
        newZip = ListZip.forward zip input
        when zip.val is
            35 ->
                newC4 = set4 c4 1 1 y z 1
                parseC4Helper newZip input newC4 y (z + 1)
            46 ->
                parseC4Helper newZip input c4 y (z + 1)
            _ ->
                parseC4Helper newZip input c4 (y + 1) 1


#  test data


testInput : List I64
testInput =
    [ 46, 35, 46, 10
    , 46, 46, 35, 10
    , 35, 35, 35
    ]
