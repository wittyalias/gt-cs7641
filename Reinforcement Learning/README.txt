# Introduction

This file contains instructions for running the code associated with the OMSCS CS7641 - Machine Learning Assignment 4 - Reinforcement learning.

# Files:
     code:
     -Analysis.r - this is the main file that sets up and runs the analysis
     
     ReserveDesign_R - The files in this folder were originally taken from the ecological modelling example in the MDPtoolbox documentation. They are largely unchanged, through explore_solution_reserve.r has been edited to change the plot margins. The documenation is located at http://www7.inra.fr/mia/T/MDPtoolbox/Documentation.html and the files are located at https://sourcesup.renater.fr/frs/?group_id=3692.

# Location

All of the files can be found in the github repository: 
        https://github.com/wittyalias/gt-cs7641/tree/master/Reinforcement%20Learning

# Instructions

  R can be optained from https://cran.r-project.org/.
  Rstudio (a useful IDE) can be found at https://www.rstudio.com/.
  
  Once these are installed, the MDPtoolbox (Chades et al., 2004) can be loaded into R using install.packages() from the CRAN repository.
  
  If you are going to run the analysis, it may be helpful to edit the Analyses.r first. In particular, lines 100 and 107 call the mdp_Q_learning function for 5,000,000 iterations. This can take a long time to run. Around 10,000 iterations runs in about 20 seconds. 

Chades I., Chapron G., Cros MJ., Garcia F., Sabbadin R. (2014). MDPtoolbox: a multi-platform toolbox to solve stochastic dynamic programming problems. Ecography 37:916-920. 
  
