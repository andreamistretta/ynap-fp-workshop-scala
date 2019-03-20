# Text-based RPG

## Refactoring TODO
- Remove global mutable state
    - var executing in Game
    - remove while loop
    - var(s) x, y in Player
    - var world in Game
- Enforce correctness through types
    - remove primitive
    - don't reuse similar types for different needs
    - making illegal states unrepresentable
- Separate construction from evaluation
    - split understand command from apply them (app. input)
    - split render sate from actual println (app. output)
    - where possible eliminate/centralize side-effects
- Remove throws
    - from exception to effect
    - transform a "crazy goto" into a value
- Handle side effect with IO monad
    - I/O operations are side-effect
    - lazy capture the I/O operation
    - execute "later" on specific request
    - "later" means in the main
- Use lens to manipulate immutable objects
    - copy is great but it's not scalable with nested structure
    - the code leak the structure of your model (fragile to change)

## Features TODO
- every cell can be land or sea
- if the player ends up in the sea, he dies and the game ends
- a land can contains an enemy
- add the command 'fight', in response the player suffers damage for a constant value eg: -10
- produce a random damage value
- a land can contains potion that gives life to the player for a constant value of eg: 20