# Gaia
A weak attempt at replicating John Koza's genetic programming work to evolve electronic circuits to perform specific functions.

## TODO
* |x| DONE: Fix FitnessManager and FitnessRequester. Adding more islands ends up blocking the
result gathering. Perhaps some issue in the use of the Akka ask pattern?
* Use ngspice as a library
* |x| DONE: Output more population statistics: fitness avg, fitness median
* 