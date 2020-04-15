# General description
Minimalization of deterministic FA to minimal complete FA. Implementation using basic minimalization algorithm based on indistinguishable classes.

# Description of input and output formats
Input and output format is divided by lines where:
1. first line contains list of numbers divided by comma representing **states** of automata
2. second line contains lowercase alphabet characters representing **alphabet** of automata
3. third line contains number representing **start state** of automata
4. fourth line contains list of numbers divided by comma representing **end states** of automata
5. rest of lines contains sequences of **number,character,number** representing **transitions** of automata as **source state,symbol,destination state**

# Phases of implemented minimization process
1. Read DFA from the file or stdin
2. Parse input DFA into inner representation
3. Validate DFA syntax
4. Remove nonreachable states
5. Create complete DFA with SINK state addition
6. Create list of indistinguishable classes and sort it
7. Construct new DFA from list of indistinguishable classes

## Classes representation and new DFA creation
Indistinguishable classes are represented as tuples in list with two components.
Format of class: `([Index],[State])`
`[State]` is a list of states of given class
`[Index]` is a list of reachable classes of given class
After classes creation, sorting in traversal order is required. After that, index of given class in list of classes represents its name (or actual new state of minimal DFA). New DFA is created from data in ordered classes list.

## Example
INPUT:
`1,2,3,4,5,6`
`ab` 
`1`
`1,6`
`1,a,6`
`1,b,2`
`2,a,5`
`2,b,4`
`3,a,3`
`3,b,6`
`4,a,4`
`4,b,1`
`5,a,2`
`5,b,3`
`6,a,1`
`6,b,5`
CLASSES:
`[([0,1],["1","6"]),`
`([1,2],["2","5"]),`
`([2,0],["4","3"])]`
OUTPUT:
`0,1,2`
`ab`
`0`
`0`
`0,a,0`
`0,b,1`
`1,a,1`
`1,b,2`
`2,a,2`
`2,b,0`

# How to run
## Compilation
Using Makefile with command `make` or manually using `ghc` with command `ghc dka-2-mka.hs`.

## Launching
The program is expected to launch as  `./dka-2-mka -i/-t [input]`.
* `-i` read input DFA, parse and validate DFA and then print it on stdout
* `-t` read input DFA, run all phases of minimalization process and then print it on stdout
* `input` defines an input file, in case of absence read from stdin

# Known Issues
Unknown

# Author
Marek Salon (xsalon00)