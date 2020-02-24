# marsrover

Haskell implementation of [Mars rover kata](https://kata-log.rocks/mars-rover-kata).

This project has been created using the following:

```sh
 > stack new marsrover hspec 
```

## Requirements

These are the requirements that has been implemented:

- You are given the initial starting point (x,y) of a rover and the direction (N,S,E,W) it is facing.
- The rover receives a character array of commands.
- Implement commands that move the rover forward/backward (f,b).
- Implement commands that turn the rover left/right (l,r).

## Test

Change code and run test:

```sh
  > stack ghci --ghci-options -isrc --ghci-options -itest marsrover:marsrover-test
```

Run all the tests using:

```sh
  > stack test
```

## Run

Run the project

```sh
  > stack run
  S 8 4 LFFFRRRFLLLL LFBR<EOL+Ctrl+D>
```

