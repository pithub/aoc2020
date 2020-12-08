platform pithub/aoc2020e
    requires { aocMain : Effect {} }
    exposes [ Task ]
    packages {}
    imports []
    provides [ mainForHost ]
    effects Effect
        {
            readFile : Str -> Effect (List I64),
            writeData : List (List I64) -> Effect {}
        }

mainForHost : Effect {} as Fx
mainForHost = aocMain
