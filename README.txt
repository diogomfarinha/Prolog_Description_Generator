This software was created as part of a Master's Project with the objective of producing descriptions for procedural Prolog.
To know more about the approach and the Project itself read the Project paper in this repository.

To test the software it is recommended that you load 'Testing.pl' as it loads all modules. The programs used for 
testing can be found in 'Test_Programs.pl'. You can the software as follows:

%Generate a description for or any program and corresponding arity
?-prolog_to_imperative(displayStatus/0).
?-prolog_to_nl(displayStatus/0).

%Generate descriptions for all test programs
?-test1.
?-test2.

%Send all generated descriptions to a txt file for further analysis
?-results.


