# Introduction

This file contains instructions for running the code associated with the OMSCS CS7641 - Machine Learning Assignment 2.


There are four files of code containing functions for graphs, functions for tables, code to download and process the data, and installation of the necessary packages. The remaining files contain code to run each of the different algorithms on both data sets to produce either the training set size learning curves (lc) or the complexity based learning curves(cx).

# Location

All of the files can be found in the github repository: https://github.com/wittyalias/gt-cs7641/tree/master/Randomized%20Optimization

# Environment

This analysis uses the R package for statistical computing (https://www.r-project.org/), version 3.5.2 (but will work with any 3.5 version),j and Python (https://www.python.org/). 

# Dataset

The dataset used for this assignment is the Abalone a from the UCI Machine Learning Repository (http://archive.ics.uci.edu/ml/index.php). The code is set up to either
- download the data directly from the UCI repository, save it to the 'raw-input' folder, clean it and save it to the 'input' folder OR
- if the data has already been downloaded, load the data from an 'input' subfolder, with 'abalone' and 'adult' subfolders. These two datasets are located at the websites below:

Abalone:
URL: http://archive.ics.uci.edu/ml/datasets/Abalone

# Instructions

Install R (https://www.r-project.org/) and Rstudio( https://www.rstudio.com/):
Download or fork the git repository to a local folder. This should contain all the cleaned data.
If not, then run the setup file: preprocess_data.R. 

Following that, the neuralnet-base.R produces the base neural network that is used by genetic.R, simulated-annealing.R and randomized-hill.R. 

For the python code, install python, numpy, and mlrose. 

Prior to running the k-colours.py, run the make_graph.R. This creates the graph that is used in k-colours.py. 
 
# Packages

This analysis uses a number of packages for data cleaning and manipulation, charting and (of course) machine learning algorithms. They are:

- dplyr: used for data manipulation
- tidyr: used for data manipulation
- ggplot: used for creating charts
- caret: used for creating data partitions as well as training some models and managing cross-validation
- stringr: used to easily clean strings in the data sets
- knitr: used for building the rmarkdown into pdf
- pander: used for displaying tables in the final report
- GA: implementation of genetic algorithms
- GenSA: implementation of simulated annealing
- igraph: used to create connected graph


# Folder and file structure

There are four relevant folders:
- raw-input: the download destination for the csv files from the UCI repository.
- input: the location of the processed data, ready for use by the machine learning models.
- code: the location of all of the code necessary to run the analysis.
- output: the location of the tables containing the results of the analysis necessary for making the relevant learning curves.

# Code Files

Setup
- preprocess_data.r: download and prepare the data for analysis
- make_graph.R

Neural network analysis
- neuralnet-base.R
- randomized-hill.R
- simulated-annealing.R
- genetic.R

Optimisation problem analyis
- travellig_salesman.py
- four_peaks.py
- k-colours.py


