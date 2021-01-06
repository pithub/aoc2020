interface ListSet
    exposes [ empty, emptyWithConfig, size, insert, inserted, member, toList ]
    imports []


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
    when (lastIndex tree - 4) // 4 is
        Ok len -> len
        _ -> 0


member : List I64, I64 -> Bool
member = \tree, val ->
    ptr = 0
    node = getPtrNode tree ptr

    memberHelper tree node val


memberHelper : List I64, I64, I64 -> Bool
memberHelper = \tree, node, val ->
    if node == 0 then
        False
    else
        nodeVal = getNodeVal tree node
        if val < nodeVal then
            lftNode = getNodeLft tree node
            memberHelper tree lftNode val
        else if val > nodeVal then
            rgtNode = getNodeRgt tree node
            memberHelper tree rgtNode val
        else
            True


insert : List I64, I64 -> List I64
insert = \tree, val ->
    ptrOut = 0
    ptrIn = 0

    tree
        |> setInserted 0
        |> insertHelp ptrOut ptrIn val
        |> setPtrCol ptrOut 2


insertHelp : List I64, I64, I64, I64 -> List I64
insertHelp = \tree, ptrOut, ptrIn, val ->
    when getPtrNode tree ptrIn is
        0 ->
            addNode tree ptrOut val 1 0 0

        node ->
            nodeVal = getNodeVal tree node
            if val < nodeVal then
                lftIdx = node + 2
                tree
                    |> insertHelp lftIdx lftIdx val
                    |> balance ptrOut node
            else if val > nodeVal then
                rgtIdx = node + 3
                tree
                    |> insertHelp rgtIdx rgtIdx val
                    |> balance ptrOut node
            else
                tree


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


addNode = \tree, ptrOut, val, col, lft, rgt ->
    node = lastIndex tree

    newTree =
        if node < List.len tree then
            tree
        else
            buffer = List.repeat (4 * allocNodes tree) 0
            List.concat tree buffer

    newTree
        |> List.set (node + 0) val
        |> List.set (node + 1) col
        |> List.set (node + 2) lft
        |> List.set (node + 3) rgt
        |> setLastIndex (node + 4)
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


#getPtrVal : List I64, I64 -> I64
#getPtrVal = \tree, ptrIn ->
#    getPtr tree ptrIn 0


#setPtrVal : List I64, I64, I64 -> List I64
#setPtrVal = \tree, ptrIn, val ->
#    setPtr tree ptrIn 0 val


getNodeVal : List I64, I64 -> I64
getNodeVal = \tree, node ->
    getNode tree node 0


#setNodeVal : List I64, I64, I64 -> List I64
#setNodeVal = \tree, node, val ->
#    setNode tree node 0 val


#getPtrCol : List I64, I64 -> I64
#getPtrCol = \tree, ptrIn ->
#    getPtr tree ptrIn 1


setPtrCol : List I64, I64, I64 -> List I64
setPtrCol = \tree, ptrIn, col ->
    setPtr tree ptrIn 1 col


getNodeCol : List I64, I64 -> I64
getNodeCol = \tree, node ->
    getNode tree node 1


setNodeCol : List I64, I64, I64 -> List I64
setNodeCol = \tree, node, col ->
    setNode tree node 1 col


#getPtrLft : List I64, I64 -> I64
#getPtrLft = \tree, ptrIn ->
#    getPtr tree ptrIn 2


#setPtrLft : List I64, I64, I64 -> List I64
#setPtrLft = \tree, ptrIn, lft ->
#    setPtr tree ptrIn 2 lft


getNodeLft : List I64, I64 -> I64
getNodeLft = \tree, node ->
    getNode tree node 2


setNodeLft : List I64, I64, I64 -> List I64
setNodeLft = \tree, node, lft ->
    setNode tree node 2 lft


#getPtrRgt : List I64, I64 -> I64
#getPtrRgt = \tree, ptrIn ->
#    getPtr tree ptrIn 3


#setPtrRgt : List I64, I64, I64 -> List I64
#setPtrRgt = \tree, ptrIn, rgt ->
#    setPtr tree ptrIn 3 rgt


getNodeRgt : List I64, I64 -> I64
getNodeRgt = \tree, node ->
    getNode tree node 3


setNodeRgt : List I64, I64, I64 -> List I64
setNodeRgt = \tree, node, rgt ->
    setNode tree node 3 rgt


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
