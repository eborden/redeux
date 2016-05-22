# Redeux
Haskell Redux-y Virtual-Dom-y Thanger

`note: You shouldn't use this, I'm just having fun.`

## Key Features
- The command pattern has been replaced with a free monad for a more flexible mutation grammer.
- State is held within an MVar for blocking state mutation. If an action runs async it may only read/write atomically.
- The free monad aproach allows for many middleware like redux features to be expressed in the command grammer. Async can be implemnted within the grammer itself.
