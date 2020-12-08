interface Task
    exposes [ Task, after, readFile, writeData ]
    imports [ Effect ]

Task a : Effect.Effect a

after = \first, second -> Effect.after first second

readFile = \name -> Effect.readFile name

writeData = \data -> Effect.writeData data
