Here is a quick description of the files:

1) simulation_toplevel.R has the main source code for running the simulation on a variety of graphs and logging the results.
2) utils.R is where most of the simulation and computations are carried out.
3) process_results.R carries out some simple plotting on the results obtained from a simulation.

The "data" directory contains .csv files of all of the graphs. All graphs are organized by the overall graph size N and number of connections K.

The "results" directory is where all simulation results get saved.

PACKAGE DEPENDENCIES:

This code uses a few packages that need to be installed prior to use.  You can do this in R with the following lines of code:

install.packages('tidyverse')

install.packages('msm')

install.packages('itertools')

install.packages('miscTools')
