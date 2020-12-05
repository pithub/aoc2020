platform pithub/aoc2020
    requires { aocMain : Effect {} }
    exposes []
    packages {}
    imports []
    provides [ mainForHost ]
    effects Effect
        {
            readFile : Str -> Effect (List Int),
            writeData : List (List Int) -> Effect {}
        }

mainForHost : Effect {} as Fx
mainForHost = aocMain
