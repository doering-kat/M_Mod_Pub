M\_Mod\_Pub: A Bayesian natural mortality model
================
Kathryn Doering<sup>1</sup>, Michael Wilberg<sup>1</sup>, Dong Liang<sup>1</sup>, and Mitchell Tarnowski<sup>2</sup>

#### Author affiliations

<sup>1</sup>University of Maryland Center for Environmental Science

<sup>2</sup>Maryland Department of Natural Resources

Project description
-------------------

This project contains code and associated data for model runs of the Bayesian natural mortality model. This model is intended for species like bivalves that are relatively sessile and leave behind evidence when they die. In particular, it was developed for the eastern oyster *Crassostrea virginica* in the Maryland potion of Chesapeake Bay. This model uses data from Maryland Department of Natural Resources (MDNR)'s [oyster fall dredge survey](https://dnr.maryland.gov/fisheries/Pages/shellfish-monitoring/fall-survey.aspx).

These runs were completed after KD's thesis defense. Runs include a base model, with small modifications from the thesis version based on feedback, as well as sensitivity runs.

The available data and files in this repository together contain all the necessary components to reproduce the analyses.

Code and Data subdirectories description
----------------------------------------

All code for this project is in the Code subdirectory, which is then further separated into other subdirectories based on specific project tasks. The Data subdirectory includes data used as input, that are not derived from other scripts in this project.

Output subdirectories: Derived\_Data and Figs
---------------------------------------------

Derived\_Data is the subdirectory where data from running the scripts (intermediate and final results) are stored. Derived\_Data in the repository only includes the cleaned version of the fall survey data used for running the model. Other intermediate forms of the data are not included in the repository.

Figures saved in the scripts are output to the Figs subdirectory. Note that the Figs subdirectory is not shown in the repository.

Both of these subdirectories have further subdirectories that mirror naming conventions within the Code subdirectory. For example, figure output from `./Code/1_Start/foo.R` would be put in `./Figs/1_Start/foo`, while data output (.csv, .rda, .txt, etc.) would be put in `./Derived_Data/1_Start/foo`
