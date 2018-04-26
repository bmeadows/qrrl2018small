OVERVIEW:

Requires SWI-Prolog (version 6 or higher) to run.

The core components of the architecture are contained in the main file 'qRRL.pl'. This requires that several more files be included in the same directory:
- 'parameters.pl'
- 'meta.pl'
- 'rrl_domain.pl'
- 'data_collector.pl'


BUILD INSTRUCTIONS:

It is recommended to first become familiar with the content of the file 'parameters.pl' which specifies parameters for each part of the system.
Before running, set each of these to an appropriate value. The file provided has certain default parameters set.

Create or modify the file 'rrl_domain.pl' describing the domain the architecture should operate over, in the form required.
The provided version of the file is configured to learn failure cases for one target action in a simple simulated robot domain.


RUN INSTRUCTIONS:

To run the program, consult the main file with SWI-Prolog (swipl.exe). For example, open the terminal in the directory containing 'qRRL.pl' and enter
  ?- consult(qRRL).
or open the file 'qRRL.pl' itself when .pl files are associated with SWI-Prolog.

To begin learning, enter run(filename), specifying the file to send final output axioms to, for example:
  ?- run('learned.txt').

A batch processing interface is available using runbatch('counterFilename', 'errorFilename', 'timeFilename') in an analogous way.
This command can be run multiple times updating the run data that is stored in the output files each time. An example of this for Windows operating systems is given in the file 'scriptrrl.bat'.
Batch processing uses the batch_collect_data function in the supporting file 'data_collector.pl' to analyse information on false positives over extended trials, printing results to the file 'post_analysis.txt'.
