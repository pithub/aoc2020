interface Day06 exposes [ output ] imports [ TestUtil ]


output : List Int -> List (List Int)
output = \input ->
    [ TestUtil.verify 6 1 1 611 0
    , TestUtil.show   6 1   (List.len input)
    , TestUtil.verify 6 2 1 621 0
    , TestUtil.show   6 2   (List.len input)
    ]
