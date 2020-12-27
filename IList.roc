interface IList exposes
    [ IList, emptyNum, emptyIList
    , safeGet, set, clear ]
    imports []


#  ---


Idx : I64

IList a :
    { len : Idx, minIdx : Idx, first : Idx, last : Idx
    , list : List a, default : a, isDefault : (a, a -> Bool)
    }


new : List a, a, (a, a -> Bool) -> IList a
new = \list, default, isDefault ->
    { len: 0, minIdx: 0, first: 0, last: 0, list, default, isDefault }


emptyNum : I64 -> IList I64
emptyNum = \default ->
    new [ default ] default (\val, def -> val == def)


emptyIList : IList a -> IList (IList a)
emptyIList = \default ->
    list = [ default ]
    copyOfDefault = new default.list default.default default.isDefault
    new list copyOfDefault (\val, _def -> val.len == 0)


safeGet : IList a, Idx -> a
safeGet = \ilist, idx ->
    if ilist.len == 0 then
        ilist.default
    else
        relToMin = idx - ilist.minIdx
        if relToMin < ilist.first || relToMin > ilist.last then
            ilist.default
        else
            when List.get ilist.list relToMin is
                Ok val ->
                    val
                _ ->
                    ilist.default


set : IList a, Idx, a -> IList a
set = \ilist, idx, val ->
    if ilist.len == 0 then
        { ilist & len: 1, minIdx: idx, first: 0, last: 0, list: [ val ] }
    else
        relToMin = idx - ilist.minIdx
        if relToMin < 0 then
            additional = -relToMin
            empty = List.repeat (additional - 1) ilist.default
            { ilist & len: ilist.len + additional, minIdx: idx
            , first: 0, last: ilist.last + additional
            , list: List.join [ [ val ], empty, ilist.list ]
            }
        else
            relToLen = relToMin - ilist.len
            if relToLen < 0 then
                newFirst = if ilist.first < relToMin then ilist.first else relToMin
                newLast = if ilist.last > relToMin then ilist.last else relToMin
                { ilist & first: newFirst, last: newLast
                , list: List.set ilist.list relToMin val
                }
            else
                empty = List.repeat relToLen ilist.default
                { ilist & len: relToMin + 1, last: relToMin
                , list: List.join [ ilist.list, empty, [ val ] ]
                }


clear : IList a, Idx -> IList a
clear = \ilist, idx ->
    if ilist.len == 0 then
        ilist
    else
        relToMin = idx - ilist.minIdx
        if relToMin < ilist.first || relToMin > ilist.last then
            ilist
        else if ilist.len == 1 then
            { ilist & len: 0 }
        else if relToMin == ilist.first then
            trimFirst ilist (ilist.first + 1)
        else if relToMin == ilist.last then
            trimLast ilist (ilist.last - 1)
        else
            { ilist & list: List.set ilist.list relToMin ilist.default }


trimFirst : IList a, Idx -> IList a
trimFirst = \ilist, first ->
    when List.get ilist.list first is
        Ok val ->
            if first < ilist.last then
                if ilist.isDefault val ilist.default then
                    trimFirst ilist (first + 1)
                else
                    { ilist & first }
            else
                { ilist & len: 1, minIdx: ilist.minIdx + first
                , first: 0, last: 0
                , list: [ val ]
                }
        _ ->
            { ilist & len: 0 }


trimLast : IList a, Idx -> IList a
trimLast = \ilist, last ->
    when List.get ilist.list last is
        Ok val ->
            if last > ilist.first then
                if ilist.isDefault val ilist.default then
                    trimLast ilist (last - 1)
                else
                    { ilist & last }
            else
                { ilist & len: 1, first: 0, last: 0, list: [ val ] }
        _ ->
            { ilist & len: 0 }
