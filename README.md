# SATServer

This is my first serious attempt at programming something Haskell. This project implements a Sat Solver within a client server architecture. I implemented the DPLL algorithm which is a backtracking-based-search algorithm for deciding the
satisfiability of propositional logic formulae in conjunctive normal form, as a backend for a Server that
can be queried for solving Boolean Satisfiability Problems. 

The user can provide the Server a propositional logic formula, which is parsed and converted into CNF form, and passed to the DPLL algorithm. The server returns a ticket which the user can later use to query the result of the problem.

The grammer of the formula should be as follows: 
b_prop ::= var | (b_prop | b_prop) | (b_prop & b_prop) | (~b_prop {NO SPACE AFTER THE TILDA ( ~ ) } )

# Usage

In order to run the program, run the generated executable. 
From another terminal, use telnet to issue the following command

$telnet <ip> 4444

In order to provide a propositon to solve, enter 'prop' in the terminal.

Make sure the provided boolen formula obeys the grammar provided.

The parser in this code uses the hatt library to convert the boolean formula into CNF form, as is required by the dpll algorithm.
The hatt library only allows single character to be variables, therefore please donot provide variable names longer than a single character.

Once the formula has been given, the server will inform whether it obeys the grammar. If so a ticket will be provided.

This tickets can then be used to query the results of the SAT problem.

In order to query the results, connect to the server as described above.

This time select the 'res' option. Enter the ticket. The server will respond appropriately.
