# Welcome to Equation Labyrinth

A game that challenges both your puzzle-solving and math skill.

## How to play:

There are 10 levels to this game.
The higher level are more difficult.

When you first selects a level, a maze will be loaded.

### Here are some basics about the Maze:

- "■" - This is your current position, this will be colored red for better visibility
- "·" - This represents an empty space where you can navigate to
- "█" - This represents a wall cannot be navigated through

### Gameplay loop:

1. At the start, you will be given 4 randomly chosen equation, with an "n" as part of the equation.
2. You will be prompted to select one of the equation.
3. Then, you will be asked to enter a value for "n" to complete the equation.
4. Your input will be applied to the n in the equation and an answer will be obtained.
   *Note that the answer will always be rounded to the closest whole number
5. The number will represent how far you can move during this turn.
6. You will then be prompted to choose a direction to move in.

### Movement rules:

- If your number is lower than the distance you are to a wall, you will move towards the direction normally.
- If your number is a negative, you will move in the opposite direction.
- If you choose a direction where a wall is directly next to you, you will move the opposite direction.
- If your number is larger than the distance to a closest wall, you will move towards the wall and then back.
Example: If you got 10 from your equation and choose to move left, but the closest left wall is 7 spaces away, you will move towards the wall, then move back 3 spaces, which means you would have moved 4 spaces to the left.
*This will apply to the exit as well, so if you are 3 spaces away from the exit, but you got 5 and move towards the exit, you will be put back 2 spaces.

## Game

This gameplay loop will keep repeating until you reach the end of the maze.
Once you reach the end of the maze, the program will tell you how much turns you took to complete it.

Try to solve each maze using the least amount of turns possible!

Enjoy and have fun! 🙂
