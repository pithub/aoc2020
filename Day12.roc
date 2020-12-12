interface Day12 exposes [ output ] imports [ TestUtil ]


output : List I64 -> List (List I64)
output = \puzzleInput ->
    testMoves   = parseMoves testInput
    puzzleMoves = parseMoves puzzleInput

    [ TestUtil.verify 12 1 1 (endPos testMoves  ) 25
    , TestUtil.show   12 1   (endPos puzzleMoves)
    , TestUtil.verify 12 2 1 (endPosWithWaypoint testMoves  ) 286
    , TestUtil.show   12 2   (endPosWithWaypoint puzzleMoves)
    ]


Move : { cmd : I64, len : I64 }


#  first part


endPos : List Move -> I64
endPos = \moves ->
    initial = { x: 0, y: 0, dx: 1, dy: 0 }
    final = List.walk moves walkWalker initial
    Num.abs final.x + Num.abs final.y


WalkState : { x : I64, y : I64, dx : I64, dy : I64 }


walkWalker : Move, WalkState -> WalkState
walkWalker = \move, state ->
    when move.cmd is
        78 -> { state & y: state.y - move.len }  # N
        83 -> { state & y: state.y + move.len }  # S
        69 -> { state & x: state.x + move.len }  # E
        87 -> { state & x: state.x - move.len }  # W
        76 -> left move.len state
        82 -> right move.len state
        70 -> { state & x: state.x + move.len * state.dx, y: state.y + move.len * state.dy }  # F
        _ -> state


left : I64, WalkState -> WalkState
left = \len, state ->
    when degrees len is
        1 -> { state & dx:  state.dy, dy: -state.dx }
        2 -> { state & dx: -state.dx, dy: -state.dy }
        3 -> { state & dx: -state.dy, dy:  state.dx }
        _ -> state


right : I64, WalkState -> WalkState
right = \len, state ->
    when degrees len is
        1 -> { state & dx: -state.dy, dy:  state.dx }
        2 -> { state & dx: -state.dx, dy: -state.dy }
        3 -> { state & dx:  state.dy, dy: -state.dx }
        _ -> state


degrees : I64 -> I64
degrees = \len ->
    when len // 90 is
        Ok deg ->
            when deg % 4 is
                Ok norm -> norm
                _ -> 0
        _ -> 0


#  second part


endPosWithWaypoint : List Move -> I64
endPosWithWaypoint = \moves ->
    initial = { x: 0, y: 0, dx: 10, dy: -1 }
    final = List.walk moves wayWalker initial
    Num.abs final.x + Num.abs final.y


wayWalker : Move, WalkState -> WalkState
wayWalker = \move, state ->
    when move.cmd is
        78 -> { state & dy: state.dy - move.len }  # N
        83 -> { state & dy: state.dy + move.len }  # S
        69 -> { state & dx: state.dx + move.len }  # E
        87 -> { state & dx: state.dx - move.len }  # W
        76 -> left move.len state
        82 -> right move.len state
        70 -> { state & x: state.x + move.len * state.dx, y: state.y + move.len * state.dy }  # F
        _ -> state


#  test data


parseMoves : List I64 -> List Move
parseMoves = \input ->
    initial = { cmd: 0, len: 0, moves: [] }
    final = List.walk input parseWalker initial
    final.moves


ParseAcc : { cmd : I64, len : I64, moves : List Move }


parseWalker : I64, ParseAcc -> ParseAcc
parseWalker = \val, acc ->
    if 48 <= val && val <= 57 then
        { acc & len: 10 * acc.len + val - 48 }
    else if val == 10 then
        move = { cmd: acc.cmd, len: acc.len }
        { cmd: 0, len: 0, moves: List.append acc.moves move }
    else
        { acc & cmd: val }


testInput : List I64
testInput =
    [ 70, 49, 48, 10
    , 78, 51, 10
    , 70, 55, 10
    , 82, 57, 48, 10
    , 70, 49, 49, 10
    ]
