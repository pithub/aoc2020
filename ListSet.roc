interface ListSet
    exposes [ empty, size, insert, inserted, member, firstP, nextP, valP, vals, walk ]
    imports []


#  admin values
#    0: root node
#    1: inserted node 
#    2: nodes to allocate (default 1)
#    3: used list size

#  per node
#    0: key
#    1: color
#    2: left subtree
#    3: right subtree
#    4: value


empty : I64 -> List I64
empty = \nodesToAllocate ->
    [ 0, 0, nodesToAllocate, 4 ]


insertedNode : List I64 -> I64
insertedNode = \tree ->
    read tree 1


setInsertedNode : List I64, I64 -> List I64
setInsertedNode = \tree, val ->
    List.set tree 1 val


configNodesToAllocate : List I64 -> I64
configNodesToAllocate = \tree ->
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


firstP : List I64 -> I64
firstP = \tree ->
    firstPos = 4
    if firstPos < lastIndex tree then
        firstPos
    else
        0


nextP : List I64, I64 -> I64
nextP = \tree, pos ->
    nextPos = pos + 4
    if nextPos < lastIndex tree then
        nextPos
    else
        0


valP : List I64, I64 -> I64
valP = \tree, pos ->
    getNodeKey tree pos


member : List I64, I64 -> Bool
member = \tree, val ->
    root = getPtrNode tree 0
    memberHelper tree root val


memberHelper : List I64, I64, I64 -> Bool
memberHelper = \tree, node, key ->
    if node > 0 then
        nodeKey = getNodeKey tree node
        if key < nodeKey then
            memberHelper tree (getNodeLft tree node) key
        else if key > nodeKey then
            memberHelper tree (getNodeRgt tree node) key
        else
            True
    else
        False


insert : List I64, I64 -> List I64
insert = \tree, val ->
    ptrOut = 0
    ptrIn = 0

    tree
        |> setInsertedNode 0
        |> insertHelper ptrOut ptrIn val
        |> setPtrCol ptrOut 2


insertHelper : List I64, I64, I64, I64 -> List I64
insertHelper = \tree, ptrOut, ptrIn, key ->
    when getPtrNode tree ptrIn is
        0 ->
            addNode tree ptrOut key

        node ->
            nodeKey = getNodeKey tree node
            if key < nodeKey then
                lftIdx = node + 2
                tree
                    |> insertHelper lftIdx lftIdx key
                    |> balance ptrOut node
            else if key > nodeKey then
                rgtIdx = node + 3
                tree
                    |> insertHelper rgtIdx rgtIdx key
                    |> balance ptrOut node
            else
                tree


addNode : List I64, I64, I64 -> List I64
addNode = \tree, ptrOut, key ->
    node = lastIndex tree

    newTree =
        if node < List.len tree then
            tree
        else
            buffer = List.repeat (configNodesToAllocate tree * 4) 0
            List.concat tree buffer

    newTree
        |> setNodeKey node key
        |> setNodeCol node 1
        |> setNodeLft node 0
        |> setNodeRgt node 0
        |> setInsertedNode node
        |> setLastIndex (node + 4)
        |> setPtrNode ptrOut node


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


inserted : List I64 -> Bool
inserted = \tree ->
    insertedNode tree > 0


vals : List I64 -> List I64
vals = \tree ->
    walk tree (\key, result -> List.append result key) []


walk : List I64, (I64, a -> a), a -> a
walk = \tree, f, init ->
    rootNode = getPtrNode tree 0
    walkHelper init tree f rootNode


walkHelper : a, List I64, (I64, a -> a), I64 -> a
walkHelper = \result, tree, f, node ->
    if node == 0 then
        result
    else
        left = walkHelper result tree f (getNodeLft tree node)
        this = f (getNodeKey tree node) left
        walkHelper this tree f (getNodeRgt tree node)


getNodeKey : List I64, I64 -> I64
getNodeKey = \tree, node ->
    getNode tree node 0


setNodeKey : List I64, I64, I64 -> List I64
setNodeKey = \tree, node, key ->
    setNode tree node 0 key


setPtrCol : List I64, I64, I64 -> List I64
setPtrCol = \tree, ptrIn, col ->
    setPtr tree ptrIn 1 col


getNodeCol : List I64, I64 -> I64
getNodeCol = \tree, node ->
    getNode tree node 1


setNodeCol : List I64, I64, I64 -> List I64
setNodeCol = \tree, node, col ->
    setNode tree node 1 col


getNodeLft : List I64, I64 -> I64
getNodeLft = \tree, node ->
    getNode tree node 2


setNodeLft : List I64, I64, I64 -> List I64
setNodeLft = \tree, node, lft ->
    setNode tree node 2 lft


getNodeRgt : List I64, I64 -> I64
getNodeRgt = \tree, node ->
    getNode tree node 3


setNodeRgt : List I64, I64, I64 -> List I64
setNodeRgt = \tree, node, rgt ->
    setNode tree node 3 rgt


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
