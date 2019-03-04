# Make a graph for the Max - k color problem
library(dplyr)
library(igraph)

# set.seed(12345) # seed for 25 nodes
# set.seed(3625) # seed for 50 nodes
set.seed(96498) # seed for 100 nodes

num_nodes <- 100

graph <- erdos.renyi.game(num_nodes, 0.3, type=c("gnp", "gnm"),
                          directed = FALSE, loops = FALSE)

graph_list <- list()

for (i in 1:num_nodes){
    graph_list[[i]] <- data.frame(node1 = rep(i, sum(graph[i])), 
                                node2 = (1:num_nodes)[as.logical(graph[i])])
}

graph_list <- bind_rows(graph_list)

write.csv(graph_list, 
          file.path("..", "output", paste0("graph_", num_nodes,  ".csv")),
          row.names = FALSE,
          col.names = FALSE)
