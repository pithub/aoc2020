interface Task
    exposes [ Task, after, readFile, writeData ]
    imports [ Effect ]

Task a : Effect.Effect a

after : Task a, (a -> Task b) -> Task b
after = \first, second -> Effect.after first second

readFile : Str -> Task (List I64)
readFile = \name -> Effect.readFile name

writeData : List (List I64) -> Task {}
writeData = \data -> Effect.writeData data
