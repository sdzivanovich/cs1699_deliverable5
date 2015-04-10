## CS1699 Deliverable 5: Property-Based Testing With Haskell ##

*Sheridan Zivanovich (sdz5 at pitt.edu)*

### Building ###
To build, do:

	$ make normal

To build with instrumentation (allows for code coverage analysis), do:

	$ make instrument

To clean, do:

	$ make clean

### Running ###
To run the tests, do (after building):

	$ ./Main

To run the tests and generate code coverage analysis, you can use the shell script:

	$ ./coverage.sh

This will run the tests, display their outcomes and the code coverage analysis on the terminal, and generate .html files in a `coverage/` directory (all via hpc). 

### Notes ###
This project has a dependency on QuickCheck.

`MyList.hs` contains an abbreviated List module taken from the GHC source of `Data.List`. See the source file for information on the licensing, etc.

`Main.hs` contains the properties for testing `MyList`.



	
