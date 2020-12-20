platform pithub/aoc2020e
    requires { aocMain : Effect {} }
    exposes []
    packages {}
    imports [ Task ]
    provides [ mainForHost ]
    effects Effect
        {
            readFile : Str -> Effect (List I64),
            writeData : List (List I64) -> Effect {}
        }

mainForHost : Task.Task {} as Fx
mainForHost = aocMain
