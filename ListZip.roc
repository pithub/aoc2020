interface ListZip exposes
    [ Zip
    , newAtFirst, newAtLast, newAt
    , first, beforeFirst, last, afterLast
    , forward, backward, move, moveTo
    , collect
    , Queue
    , newFromTo, newStartEnd, startZip, endZip
    , updateQueue, queueSize
    , forwardStart, backwardStart
    , forwardEnd, backwardEnd
    , collectQueue
    ]
    imports []


#  ---


Zip : { len: I64, idx: I64, val: I64, default: I64 }


newAtFirst : List I64, I64 -> Zip
newAtFirst = \list, default ->
    newAt list 0 default


newAtLast : List I64, I64 -> Zip
newAtLast = \list, default ->
    lastIdx = List.len list - 1
    newAt list lastIdx default


newAt : List I64, I64, I64 -> Zip
newAt = \list, idx, default ->
    len = List.len list
    val = read list idx default

    { len, idx, val, default }


first : Zip, List I64 -> Zip
first = \zip, list ->
    moveTo zip list 0


beforeFirst : Zip -> Bool
beforeFirst = \zip ->
    zip.idx < 0


last : Zip, List I64 -> Zip
last = \zip, list ->
    moveTo zip list (zip.len - 1)


afterLast : Zip -> Bool
afterLast = \zip ->
    zip.idx >= zip.len


forward : Zip, List I64 -> Zip
forward = \zip, list ->
    move zip list 1


backward : Zip, List I64 -> Zip
backward = \zip, list ->
    move zip list -1


move : Zip, List I64, I64 -> Zip
move = \zip, list, len ->
    idx = zip.idx + len
    moveTo zip list idx


moveTo : Zip, List I64, I64 -> Zip
moveTo = \zip, list, idx ->
    if idx == zip.idx then
        zip
    else
        val = read list idx zip.default

        { zip & idx, val }


read : List I64, I64, I64 -> I64
read = \list, idx, default ->
    when List.get list idx is
        Ok n -> n
        _ -> default


collect : List I64, Zip, I64 -> List I64
collect = \list, zip, len ->
    collectHelper list zip len []


collectHelper : List I64, Zip, I64, List I64 -> List I64
collectHelper = \list, zip, len, acc ->
    if len > 1 then
        newAcc = List.append acc zip.val
        newZip = forward zip list
        newLen = len - 1
        collectHelper list newZip newLen newAcc
    else if len == 1 then
        List.append acc zip.val
    else
        acc


#  ---


Queue : { len: I64, startIdx: I64, startVal: I64, endIdx: I64, endVal: I64, default: I64 }


newFromTo : List I64, I64, I64, I64 -> Queue
newFromTo = \list, startIdx, endIdx, default ->
    len = List.len list
    startVal = read list startIdx default
    endVal = read list endIdx default

    { len, startIdx, startVal, endIdx, endVal, default }


newStartEnd : Zip, Zip, I64 -> Queue
newStartEnd = \start, end, default ->
    { len: end.len, startIdx: start.idx, startVal: start.val, endIdx: end.idx, endVal: end.val, default }


startZip : Queue -> Zip
startZip = \queue ->
    { len: queue.len, idx: queue.startIdx, val: queue.startVal, default: queue.default }


endZip : Queue -> Zip
endZip = \queue ->
    { len: queue.len, idx: queue.endIdx, val: queue.endVal, default: queue.default }


forwardStart : Queue, List I64 -> Queue
forwardStart = \queue, list ->
    moveStart queue list 1


backwardStart : Queue, List I64 -> Queue
backwardStart = \queue, list ->
    moveStart queue list -1


moveStart : Queue, List I64, I64 -> Queue
moveStart = \queue, list, len ->
    startIdx = checkedIdx queue (queue.startIdx + len)
    moveStartTo queue list startIdx


moveStartTo : Queue, List I64, I64 -> Queue
moveStartTo = \queue, list, startIdx ->
    if startIdx == queue.startIdx then
        queue
    else
        startVal = read list startIdx queue.default

        { queue & startIdx, startVal }


forwardEnd : Queue, List I64 -> Queue
forwardEnd = \queue, list ->
    moveEnd queue list 1


backwardEnd : Queue, List I64 -> Queue
backwardEnd = \queue, list ->
    moveEnd queue list -1


moveEnd : Queue, List I64, I64 -> Queue
moveEnd = \queue, list, len ->
    endIdx = checkedIdx queue (queue.endIdx + len)
    moveEndTo queue list endIdx


moveEndTo : Queue, List I64, I64 -> Queue
moveEndTo = \queue, list, endIdx ->
    if endIdx == queue.endIdx then
        queue
    else
        endVal = read list endIdx queue.default

        { queue & endIdx, endVal }


queueSize : Queue -> I64
queueSize = \queue ->
    checkedIdx queue (queue.endIdx - queue.startIdx + 1)


checkedIdx : Queue, I64 -> I64
checkedIdx = \queue, idx ->
    if idx < 0 then
        checkedIdx queue (idx + queue.len)
    else if idx >= queue.len then
        checkedIdx queue (idx - queue.len)
    else
        idx


collectQueue : List I64, Queue, I64 -> List I64
collectQueue = \list, queue, len ->
    collectQueueHelper list queue len []


collectQueueHelper : List I64, Queue, I64, List I64 -> List I64
collectQueueHelper = \list, queue, len, acc ->
    if len > 1 then
        newAcc = List.append acc queue.startVal
        newQueue = forwardStart queue list
        newLen = len - 1
        collectQueueHelper list newQueue newLen newAcc
    else if len == 1 then
        List.append acc queue.startVal
    else
        acc


updateQueue : Queue, List I64 -> Queue
updateQueue = \queue, list ->
    startVal = read list queue.startIdx queue.default
    endVal = read list queue.endIdx queue.default

    { queue & startVal, endVal }
