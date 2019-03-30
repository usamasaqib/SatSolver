# SATServer

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
