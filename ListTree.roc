interface ListTree
    exposes
        [ empty, size
        , insert, insertN, insertP, inserted
        , get, getN, find, getI, setI, getP, setP
        , firstP, nextP, keyP
        , keys, vals, walk, walkP
        ]
    imports []


#  admin values
#    0: root node
#    1: found node
#    2: inserted node
#    3: nodes to allocate (default 1)
#    4: values per node (default 1)
#    5: used list size

#  per node
#    0: key
#    1: color
#    2: left subtree
#    3: right subtree
#    4ff: value(s)


empty : I64, I64 -> List I64
empty = \nodesToAllocate, valsPerNode ->
    [ 0, 0, 0, nodesToAllocate, valsPerNode, 6 ]


foundNode : List I64 -> I64
foundNode = \tree ->
    read tree 1


setFoundNode : List I64, I64 -> List I64
setFoundNode = \tree, val ->
    List.set tree 1 val


insertedNode : List I64 -> I64
insertedNode = \tree ->
    read tree 2


setInsertedNode : List I64, I64 -> List I64
setInsertedNode = \tree, val ->
    List.set tree 2 val


configNodesToAllocate : List I64 -> I64
configNodesToAllocate = \tree ->
    read tree 3


configValsPerNode : List I64 -> I64
configValsPerNode = \tree ->
    read tree 4


lastIndex : List I64 -> I64
lastIndex = \tree ->
    read tree 5


setLastIndex : List I64, I64 -> List I64
setLastIndex = \tree, val ->
    List.set tree 5 val


size : List I64 -> I64
size = \tree ->
    when (lastIndex tree - 6) // (internalNodeSize tree) is
        Ok len -> len
        _ -> 0


internalNodeSize : List I64 -> I64
internalNodeSize = \tree ->
    4 + configValsPerNode tree


firstP : List I64 -> I64
firstP = \tree ->
    firstPos = 6
    if firstPos < lastIndex tree then
        firstPos
    else
        0


nextP : List I64, I64 -> I64
nextP = \tree, pos ->
    nextPos = pos + internalNodeSize tree
    if nextPos < lastIndex tree then
        nextPos
    else
        0


keyP : List I64, I64 -> I64
keyP = \tree, pos ->
    getNodeKey tree pos


get : List I64, I64 -> Result I64 I64
get = \tree, key ->
    getN tree key 0


getN : List I64, I64, I64 -> Result I64 I64
getN = \tree, key, num ->
    node = find tree key
    if node > 0 then
        Ok (getNodeVal tree node num)
    else
        Err key


find : List I64, I64 -> I64
find = \tree, key ->
    root = getPtrNode tree 0
    findHelper tree root key


findHelper : List I64, I64, I64 -> I64
findHelper = \tree, node, key ->
    if node > 0 then
        nodeKey = getNodeKey tree node
        if key < nodeKey then
            findHelper tree (getNodeLft tree node) key
        else if key > nodeKey then
            findHelper tree (getNodeRgt tree node) key
        else
            node
    else
        0


insert : List I64, I64, I64 -> List I64
insert = \tree, key, val ->
    insertN tree key 0 val


insertN : List I64, I64, I64, I64 -> List I64
insertN = \tree, key, num, val ->
    newTree = insertP tree key
    node = foundNode newTree
    setNodeVal newTree node num val


insertP : List I64, I64 -> List I64
insertP = \tree, key ->
    ptrOut = 0
    ptrIn = 0

    tree
        |> setInsertedNode 0
        |> insertHelper ptrOut ptrIn key
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
                setFoundNode tree node


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
        |> setInsertedNode node
        |> setFoundNode node
        |> setLastIndex (node + nodeSize)
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


getI : List I64, I64 -> I64
getI = \tree, num ->
    node = foundNode tree
    getNodeVal tree node num


setI : List I64, I64, I64 -> List I64
setI = \tree, num, val ->
    node = foundNode tree
    setNodeVal tree node num val


getP : List I64, I64, I64 -> I64
getP = \tree, node, num ->
    getNodeVal tree node num


setP : List I64, I64, I64, I64 -> List I64
setP = \tree, node, num, val ->
    setNodeVal tree node num val


keys : List I64 -> List I64
keys = \tree ->
    walk tree (\key, _val, result -> List.append result key) []


vals : List I64 -> List I64
vals = \tree ->
    walk tree (\_key, val, result -> List.append result val) []


walk : List I64, (I64, I64, a -> a), a -> a
walk = \tree, f, init ->
    rootNode = getPtrNode tree 0
    walkHelper init tree f rootNode


walkHelper : a, List I64, (I64, I64, a -> a), I64 -> a
walkHelper = \result, tree, f, node ->
    if node == 0 then
        result
    else
        left = walkHelper result tree f (getNodeLft tree node)
        this = f (getNodeKey tree node) (getNodeVal tree node 0) left
        walkHelper this tree f (getNodeRgt tree node)


walkP : List I64, (I64, List I64, I64, a -> a), a -> a
walkP = \tree, f, init ->
    rootNode = getPtrNode tree 0
    walkPHelper init tree f rootNode


walkPHelper : a, List I64, (I64, List I64, I64, a -> a), I64 -> a
walkPHelper = \result, tree, f, node ->
    if node == 0 then
        result
    else
        left = walkPHelper result tree f (getNodeLft tree node)
        this = f (getNodeKey tree node) tree node left
        walkPHelper this tree f (getNodeRgt tree node)


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


getNodeVal : List I64, I64, I64 -> I64
getNodeVal = \tree, node, num ->
    getNode tree node (4 + num)


setNodeVal : List I64, I64, I64, I64 -> List I64
setNodeVal = \tree, node, num, val ->
    setNode tree node (4 + num) val


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
