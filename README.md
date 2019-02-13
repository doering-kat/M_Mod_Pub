M\_Mod\_Pub: A Bayesian natural mortality model
================
Kathryn Doering
January 9, 2019

Project description
-------------------

This project contains code and associated data for model runs of the Bayesian natural mortality model for a manuscript.

These were completed after KD's thesis defense (theses runs are in the Oyster\_Stock\_Assessment project, model All\_Run\_4). Runs include a base model, with small modifications, as well as a senstivity runs.

Code and data subdirectories description
----------------------------------------

All code for this project is in the Code subdirectory, which is then further separted into other subdirectories based on specific project tasks. The Data subdirectory includes data used as input, that are not derived from other scripts in this project

These folders together contain all the necessary components to reproduce the analyses.

Output subdirectories: Derived\_Data and Figs
---------------------------------------------

Derived\_Data is the subdirectory where data from running the scripts (intermediate and final results) are stored.

Figures saved in the scripts are output to the Figs subdirectory.

Both of these subdirectories have further subdirectories that mirror naming conventions within the Code subdirectory. For example, figure output from ./Code/1\_Start/foo.R would be put in ./Figs/1\_Start/foo, while data output (.csv, .rda, .txt, etc.) would be put in ./Derived\_Data/1\_Start/foo
