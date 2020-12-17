interface Hash exposes [ str ] imports []


str : List I64 -> I64
str = \chars ->
    List.walk chars (\c, h -> h * 33 + c) 5381
