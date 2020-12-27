interface ListTree
    exposes [ empty, emptyWithConfig, isEmpty, size, singleton, insert, get, toList ]
    imports []


#  Der ganze Baum wird als List I64 abgebildet
#  Ein Knoten besteht aus 5 I64 Einträgen: key, value, color, left, right
#  Red ist 1, Black ist 2
#  Left und Right sind die Adressen vom jeweiligen SubTree Root Node
#  Die Adresse vom obersten Root Node steht im ersten Element der Liste
#  Im zweiten Element steht, ob beim Insert ein neuer Eintrag erstellt wurde
#  Empty wird durch Adresse 0 dargestellt
#  Wenn eine Funktion einen Node zurückgibt, muss die Adresse vom Pointer als Parameter übergeben werden


empty : List I64
empty =
    emptyWithConfig 1


emptyWithConfig : I64 -> List I64
emptyWithConfig = \nodes ->
    [ 0, 0, nodes, 4 ]


allocNodes : List I64 -> I64
allocNodes = \tree ->
    read tree 2


lastIndex : List I64 -> I64
lastIndex = \tree ->
    read tree 3


setLastIndex : List I64, I64 -> List I64
setLastIndex = \tree, val ->
    List.set tree 3 val


size : List I64 -> I64
size = \tree ->
    when (lastIndex tree - 4) // 5 is
        Ok len -> len
        _ -> 0


isEmpty : List I64 -> Bool
isEmpty = \tree ->
    size tree == 0


singleton : I64, I64 -> List I64
singleton = \key, val ->
    [ 4, 0, 1, 9, key, val, 2, 0, 0 ]


get : List I64, I64 -> Result I64 I64
get = \tree, key ->
    ptr = 0
    node = getPtrNode tree ptr

    getHelper tree node key


getHelper : List I64, I64, I64 -> Result I64 I64
getHelper = \tree, node, key ->
    if node == 0 then
        Err key
    else
        nodeKey = getNodeKey tree node
        if key < nodeKey then
            lftNode = getNodeLft tree node
            getHelper tree lftNode key
        else if key > nodeKey then
            rgtNode = getNodeRgt tree node
            getHelper tree rgtNode key
        else
            val = getNodeVal tree node
            Ok val


insert : List I64, I64, I64 -> List I64
insert = \tree, key, val ->
    ptrOut = 0
    ptrIn = 0

    tree
        |> setInserted 0
        |> insertHelp ptrOut ptrIn key val
        |> setPtrCol ptrOut 2


insertHelp : List I64, I64, I64, I64, I64 -> List I64
insertHelp = \tree, ptrOut, ptrIn, key, val ->
    when getPtrNode tree ptrIn is
        0 ->
            addNode tree ptrOut key val 1 0 0

        node ->
            nodeKey = getNodeKey tree node
            if key < nodeKey then
                lftIdx = node + 3
                tree
                    |> insertHelp lftIdx lftIdx key val
                    |> balance ptrOut node
            else if key > nodeKey then
                rgtIdx = node + 4
                tree
                    |> insertHelp rgtIdx rgtIdx key val
                    |> balance ptrOut node
            else
                setNodeVal tree node val


balance : List I64, I64, I64 -> List I64
balance = \tree, ptrOut, node ->
    if inserted tree then
        rgtNode = getNodeRgt tree node
        rgtCol = getNodeCol tree rgtNode
        lftNode = getNodeLft tree node
        lftCol = getNodeCol tree lftNode

        if rgtCol == 1 then
            if lftCol == 1 then
                tree
                    |> setNodeCol lftNode 2
                    |> setNodeCol rgtNode 2
                    |> setNodeCol node 1
                    |> setPtrNode ptrOut node
            else
                nodeCol = getNodeCol tree node
                rgtLftNode = getNodeLft tree rgtNode
                tree
                    |> setNodeCol node 1
                    |> setNodeRgt node rgtLftNode
                    |> setNodeCol rgtNode nodeCol
                    |> setNodeLft rgtNode node
                    |> setPtrNode ptrOut rgtNode
        else
            lftLftNode = getNodeLft tree lftNode
            lftLftCol = getNodeCol tree lftLftNode

            if lftCol == 1 && lftLftCol == 1 then
                lftRgtNode = getNodeRgt tree lftNode
                tree
                    |> setNodeCol lftLftNode 2
                    |> setNodeCol node 2
                    |> setNodeLft node lftRgtNode
                    |> setNodeRgt lftNode node
                    |> setPtrNode ptrOut lftNode
            else
                setPtrNode tree ptrOut node
    else
        tree


addNode = \tree, ptrOut, key, val, col, lft, rgt ->
    node = lastIndex tree

    newTree =
        if node < List.len tree then
            tree
        else
            buffer = List.repeat (5 * allocNodes tree) 0
            List.concat tree buffer

    newTree
        |> List.set (node + 0) key
        |> List.set (node + 1) val
        |> List.set (node + 2) col
        |> List.set (node + 3) lft
        |> List.set (node + 4) rgt
        |> setLastIndex (node + 5)
        |> setInserted 1
        |> setPtrNode ptrOut node


setInserted : List I64, I64 -> List I64
setInserted = \tree, flg ->
    List.set tree 1 flg


inserted : List I64 -> Bool
inserted = \tree ->
    read tree 1 > 0


toList : List I64 -> List I64
toList = \tree ->
    rootNode = getPtrNode tree 0
    toListHelper [] tree rootNode


toListHelper : List I64, List I64, I64 -> List I64
toListHelper = \result, tree, node ->
    if node == 0 then
        result
    else
        result
            |> toListHelper tree (getNodeLft tree node)
            |> List.append (getNodeVal tree node)
            |> toListHelper tree (getNodeRgt tree node)


#getPtrKey : List I64, I64 -> I64
#getPtrKey = \tree, ptrIn ->
#    getPtr tree ptrIn 0


#setPtrKey : List I64, I64, I64 -> List I64
#setPtrKey = \tree, ptrIn, key ->
#    setPtr tree ptrIn 0 key


getNodeKey : List I64, I64 -> I64
getNodeKey = \tree, node ->
    getNode tree node 0


#setNodeKey : List I64, I64, I64 -> List I64
#setNodeKey = \tree, node, key ->
#    setNode tree node 0 key


#getPtrVal : List I64, I64 -> I64
#getPtrVal = \tree, ptrIn ->
#    getPtr tree ptrIn 1


#setPtrVal : List I64, I64, I64 -> List I64
#setPtrVal = \tree, ptrIn, val ->
#    setPtr tree ptrIn 1 val


getNodeVal : List I64, I64 -> I64
getNodeVal = \tree, node ->
    getNode tree node 1


setNodeVal : List I64, I64, I64 -> List I64
setNodeVal = \tree, node, val ->
    setNode tree node 1 val


#getPtrCol : List I64, I64 -> I64
#getPtrCol = \tree, ptrIn ->
#    getPtr tree ptrIn 2


setPtrCol : List I64, I64, I64 -> List I64
setPtrCol = \tree, ptrIn, col ->
    setPtr tree ptrIn 2 col


getNodeCol : List I64, I64 -> I64
getNodeCol = \tree, node ->
    getNode tree node 2


setNodeCol : List I64, I64, I64 -> List I64
setNodeCol = \tree, node, col ->
    setNode tree node 2 col


#getPtrLft : List I64, I64 -> I64
#getPtrLft = \tree, ptrIn ->
#    getPtr tree ptrIn 3


#setPtrLft : List I64, I64, I64 -> List I64
#setPtrLft = \tree, ptrIn, lft ->
#    setPtr tree ptrIn 3 lft


getNodeLft : List I64, I64 -> I64
getNodeLft = \tree, node ->
    getNode tree node 3


setNodeLft : List I64, I64, I64 -> List I64
setNodeLft = \tree, node, lft ->
    setNode tree node 3 lft


#getPtrRgt : List I64, I64 -> I64
#getPtrRgt = \tree, ptrIn ->
#    getPtr tree ptrIn 4


#setPtrRgt : List I64, I64, I64 -> List I64
#setPtrRgt = \tree, ptrIn, rgt ->
#    setPtr tree ptrIn 4 rgt


getNodeRgt : List I64, I64 -> I64
getNodeRgt = \tree, node ->
    getNode tree node 4


setNodeRgt : List I64, I64, I64 -> List I64
setNodeRgt = \tree, node, rgt ->
    setNode tree node 4 rgt


#getPtr : List I64, I64, I64 -> I64
#getPtr = \tree, ptrIn, off ->
#    node = getPtrNode tree ptrIn
#    getNode tree node off


setPtr : List I64, I64, I64, I64 -> List I64
setPtr = \tree, ptrIn, off, num ->
    node = getPtrNode tree ptrIn
    setNode tree node off num


getPtrNode : List I64, I64 -> I64
getPtrNode = \tree, ptrIn ->
    read tree ptrIn


setPtrNode : List I64, I64, I64 -> List I64
setPtrNode = \tree, ptrOut, node ->
    List.set tree ptrOut node


getNode : List I64, I64, I64 -> I64
getNode = \tree, node, off ->
    if node > 0 then
        numIdx = node + off
        read tree numIdx
    else
        0


setNode : List I64, I64, I64, I64 -> List I64
setNode = \tree, node, off, num ->
    if node > 0 then
        numIdx = node + off
        List.set tree numIdx num
    else
        tree


read : List I64, I64 -> I64
read = \tree, idx ->
    when List.get tree idx is
        Ok n -> n
        _ -> 0
