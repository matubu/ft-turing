# Turing Machine Simulator

The goal of this project is to write a program able to simulate a single headed, single tape Turing machine from a machine description provided in json. The program is written in Haskell and uses the Aeson library for JSON decoding.

## Getting Started

To run the program, you will need to have the Haskell platform installed on your machine. Once you have that set up, you can clone this repository and navigate to the project directory.

## Running the simulation

To run the simulation, you will need to provide the program with a JSON file that describes the Turing machine you want to simulate. The JSON file should have the following structure:

``` JSON
{
    "name": "",
    "alphabet": [],
    "blank": "",
    "states": [],
    "initial": "",
    "finals": [],
    "transitions": {}
}

```

- `name`: A string representing the name of the Turing machine
- `alphabet: An array of strings representing the alphabet of the machine. Each string should be a single character.
- `blank`: A string representing the blank symbol. It should be a single character and a member of the alphabet.
- `states`: An array of strings representing the states of the machine. Each string should be at least one character.
- `initial`: A string representing the initial state. It should be a member of the states array.
- `finals`: An array of strings representing the final states. Each string should be a member of the states array.
- `transitions`: An object representing the transitions of the machine. The keys of the object should be members of the states array, and the values should be arrays of objects representing the transitions from that state. Each transition object should have the following structure:

``` MD
- read: A string representing the symbol being read. It should be a member of the alphabet array.
- to_state: A string representing the state the machine should transition to. It should be a member of the states array.
- write: A string representing the symbol to be written. It should be a member of the alphabet array.
- action: A string representing the action to be taken. It should be either "LEFT" or "RIGHT".
```

Once you have a valid JSON file, you can run the simulation by providing the path to the file as an argument to the program:

``` bash
./Main path/to/file.json
```

The program will then simulate the Turing machine and print the final tape configuration to the console.

## Validation

The program includes a validation function that checks the JSON file for any errors. If the file is invalid, the program will print an error message to the console indicating what is wrong with the file.

## Limitations

This implementation of a Turing machine simulator currently only supports single-tape machines with a single head. It also currently only supports the "LEFT" and "RIGHT" actions for head movement.

## Example

This is an example of a configuration for a Turing machine, specifically designed for the task of unary addition.

``` JSON
{
 "name" : "unary_add",
 "alphabet": [ ".", "1", "+" ],
 "blank" : ".",
 "states" : [ "scan", "move", "HALT" ],
 "initial" : "scan",
 "finals" : [ "HALT" ],
 "transitions" : {
  "scan": [
   { "read" : "1", "to_state": "move", "write": ".", "action": "RIGHT"},
   { "read" : ".", "to_state": "HALT", "write": ".", "action": "RIGHT"},
   { "read" : "+", "to_state": "HALT", "write": ".", "action": "RIGHT"}
  ],
  "move": [
   { "read" : "1", "to_state": "move", "write": "1", "action": "RIGHT"},
   { "read" : ".", "to_state": "HALT", "write": "1", "action": "RIGHT"},
   { "read" : "+", "to_state": "HALT" , "write": "1", "action": "RIGHT"}
  ]
 }
}
```

The machine has a name "unary_add" and an alphabet that consists of three symbols: ".", "1", and "+". The blank symbol is ".".

There are three states in this machine: "scan", "move", and "HALT". The initial state is "scan" and the final state(s) is/are "HALT".

The transitions of the machine are defined in the "transitions" field of the configuration. There are two types of transition, one for the "scan" state and one for the "move" state.

The transitions for the "scan" state are defined as follows:

- If the machine reads a "1" symbol, it moves to the "move" state, writes a "." symbol, and moves the tape head to the right.
- If the machine reads a "." symbol, it moves to the "HALT" state, writes a "." symbol, and moves the tape head to the right.
- If the machine reads a "+" symbol, it moves to the "HALT" state, writes a "." symbol, and moves the tape head to the right.

The transitions for the "move" state are defined as follows:

- If the machine reads a "1" symbol, it stays in the "move" state, writes a "1" symbol, and moves the tape head to the right.
- If the machine reads a "." symbol, it moves to the "HALT" state, writes a "1" symbol, and moves the tape head to the right.
- If the machine reads a "+" symbol, it moves to the "HALT" state, writes a "1" symbol, and moves the tape head to the right.

In this example, the Turing machine will take as input a string of "1"s separated by "+" symbol, and it will add the number of "1"s on the left and right side of the "+" symbol and write the result on the right side of the "+" symbol. For example, if the input is "111+11", the machine will write "11111" on the tape.
