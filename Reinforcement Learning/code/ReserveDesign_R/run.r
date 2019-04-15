#########################################################################
# RESERVE EXAMPLE
#########################################################################

rm(list=ls())

library(MDPtoolbox)

source('mdp_example_reserve.r')
source('explore_solution_reserve.r')

source('dec2binvec.r')
source('getSite.r')
source('binvec2dec.r')
source('getState.r')

# Generate the species richness matrix (e.g. 7 species, 20 sites)
set.seed(1)
M <- round(matrix(nrow=7, ncol=20, data=runif(7*20,0,1)))

# Generate the transition and reward matrix
PR <- mdp_example_reserve(M, 0.2)
P <- PR$P
R <- PR$R

# Solve the reserve design problem
results <- mdp_value_iteration(P, R, 0.96, 0.001);
V <- results$V
policy <- results$policy

# Explore solution
explore_solution_reserve(numeric(7), policy, M, P, R)

