library(MDPtoolbox)

source(file.path("ReserveDesign_R", 'mdp_example_reserve.r'))
source(file.path("ReserveDesign_R",'explore_solution_reserve.r'))

source(file.path("ReserveDesign_R",'dec2binvec.r'))
source(file.path("ReserveDesign_R",'getSite.r'))
source(file.path("ReserveDesign_R",'binvec2dec.r'))
source(file.path("ReserveDesign_R",'getState.r'))

###########################################
# Small analyses

# Generate the species richness matrix: 5 species, 10 sites)
set.seed(1)

species = 5
sites = 10

M <- round(matrix(nrow=species, 
                  ncol=sites, 
                  data=floor(runif(species*sites,0,1.5))))

# Generate the transition and reward matrix
small_PR <- mdp_example_reserve(M, 0.1)

# Solve the reserve design problem - value iteration 
small_value <- mdp_value_iteration(small_PR$P, small_PR$R, 0.95, 0.001);

# Solve the reserve design problem - policy iteration 
# The base mdp_policy_iteration function actually evaluates using a policy matrix
small_policy_mat <- mdp_policy_iteration(small_PR$P, small_PR$R, .95)

# In order to use a policy iteration, you need to specify an eval_type. But this necessitates
# supplying a a starting policy as well. The documentation for this says that its optional, 
# but the code is written in a Matlab style and if eval_type is included, then policy0 must be as well.
# using policy0 in the same way that the function uses it if not supplied

#However, they give the exact same answer. I suppose I should make sure the same is true for the large model
# small_policy <- mdp_policy_iteration(P = small_PR$P, 
#                                      R = small_PR$R, 
#                                      discount = .95, 
#                                      policy0 = mdp_bellman_operator(P = small_PR$P, 
#                                                                     PR = mdp_computePR(small_PR$P, small_PR$R),
#                                                                     discount = .95,
#                                                                     Vprev = numeric(dim(small_PR$P)[1]))[[2]],
#                                      max_iter = 1000,
#                                      eval_type = 1)

saveRDS(small_PR, file.path("..", "output", "small_PR.rds"))
saveRDS(small_value, file.path("..", "output", "small_value.rds"))
saveRDS(small_policy, file.path("..", "output", "small_policy.rds"))
saveRDS(M, file.path("..", "output", "small_M.rds"))


############################################
# Large analyses


# Generate the species richness matrix (e.g. 7 species, 20 sites)
set.seed(1)
species = 8
sites = 30

M <- round(matrix(nrow=species, 
                  ncol=sites, 
                  data=floor(runif(species*sites,0,1.5))))

# Generate the transition and reward matrix
large_PR <- mdp_example_reserve(M, 0.1)

# Solve the reserve design problem - value iteration 
large_value <- mdp_value_iteration(large_PR$P, large_PR$R, 0.95, 0.001);
V <- large_value$V
# policy <- small_value$policy

# Solve the reserve design problem - value iteration 
large_policy_mat <- mdp_policy_iteration(large_PR$P, large_PR$R, .95)
# 
# Nope, no difference for the larger model either. 
# large_policy <- mdp_policy_iteration(P = large_PR$P, 
#                                      R = large_PR$R, 
#                                      discount = .95, 
#                                      policy0 = mdp_bellman_operator(P = large_PR$P, 
#                                                                     PR = mdp_computePR(large_PR$P, large_PR$R),
#                                                                     discount = .95,
#                                                                     Vprev = numeric(dim(large_PR$P)[1]))[[2]],
#                                      max_iter = 1000,
#                                      eval_type = 1)

saveRDS(large_PR, file.path("..", "output", "large_PR.rds"))
saveRDS(large_value, file.path("..", "output", "large_value.rds"))
saveRDS(large_policy, file.path("..", "output", "large_policy.rds"))
saveRDS(M, file.path("..", "output", "large_M.rds"))

############################################
# Reinforcement learning - Q-learning

start_time <- Sys.time()
small_Q <- mdp_Q_learning(small_PR$P, small_PR$R, discount = .95, N = 5000000)
end_time <- Sys.time()
small_Q["time"] <- difftime(end_time, start_time, units = "secs")

saveRDS(small_Q, file.path("..", "output", "small_Q.rds"))

start_time <- Sys.time()
large_Q <- mdp_Q_learning(large_PR$P, large_PR$R, .95)
end_time <- Sys.time()
large_Q["time"] <- difftime(end_time, start_time, units = "secs")

saveRDS(large_Q, file.path("..", "output", "large_Q.rds"))
