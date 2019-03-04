# CS7641  Randomized Optimization Assignment
# Genetic Algorithm Weighting Neural Network

# Uncomment if you need to install
#install.packages("GA")
#install.packages("neuralnet")
#install.packages("gnew_models")
library("dplyr")
library("GA")
library("neuralnet")
library("caret")


set.seed(12345)

#load the data
abalone_test_set <- readRDS(file.path("..", "input",  "abalone_test_set.rds"))
abalone_train_set <- readRDS(file.path("..", "input",  "abalone_train_set.rds"))

model <- readRDS(file = file.path("..", "output", "nn-base.rds"))

new_model <- model

train_actual <- abalone_train_set %>%
    select(young, middling, old) %>%
    max.col %>%
    factor

test_actual <- abalone_test_set %>%
    select(young, middling, old) %>%
    max.col %>%
    factor

# Settings
iterations <- 1000

# maxfactor <- function(x) {
#   return(which(x == max(x)))
# }

# Neural Network as Fitness Function

# As defined in ?GA::ga :
# the fitness function, any allowable R function which takes as input an individual string representing a potential solution, and returns a numerical value describing its “fitness”.

# An attempt to edit the previous fitness function to accomodate a new_model with 10 hidden nodes and 10 inputs (plust a bias)
fit_func_edit <- function(string=c()) {

    # Weights for the hidden layer
    for (j in 1:10){
        new_model$weights[[1]][[1]][,j] <- string[(1 + 11*(j-1)):(11*j)]
    }
    # Weights for the output layer
    for(j in 1:3){
        new_model$weights[[1]][[2]][,j] <- string[((111 + 11*(j-1))):(110 + (11*j))]
    }

    prediction <- compute(new_model,
                          abalone_train_set[!names(abalone_train_set) %in% c("young" ,"middling", "old")])$net.result %>%
        max.col %>%
        factor

    return(confusionMatrix(prediction, train_actual)$overall["Accuracy"])
}


# data frame to collect information about the algorithm to compare to others

ga_table <- data.frame(iteration = 1:iterations,
                       fitness_mean = NA,
                       fitness_max = NA,
                       start_time =  .POSIXct(rep(NA, iterations)),
                       end_time =  .POSIXct(rep(NA, iterations)),
                       iter_time = NA)

# Using the abalone dataset with a neural network with 10 hidden nodes
# The abalone dataset has 10 features, meaning that there are 11 weights (inc. bias)
# for each of the nodes -> 110 weights. Plus 11 weights for each of the three output
# layer nodes -> 33 weights. So the genetic algorithm is going to be adjusting
# 143 values, that are the weights of the new_model to optimize it.

monitor_func <- function(obj, digits = getOption("digits"))
{
    # contour(x1, x2, f, drawlabels = FALSE, col = grey(0.5))
        end_time <- Sys.time()
        fitness <- na.exclude(obj@fitness)
        sumryStat <- c(mean(fitness), max(fitness))
        sumryStat <- format(sumryStat, digits = digits)


        ga_table[obj@iter, "fitness_mean"] <<- sumryStat[1]
        ga_table[obj@iter, "fitness_max"] <<- sumryStat[2]
        ga_table[obj@iter, "start_time"] <<- start_time
        ga_table[obj@iter, "end_time"] <<- end_time
        ga_table[obj@iter, "iter_time"] <<- difftime(end_time, start_time, units = "secs")


        cat(paste("GA | iter =", obj@iter, "| Mean =", sumryStat[1],
                  "| Best =", sumryStat[2]))
        cat("\n")
        flush.console()

        start_time <<- Sys.time()
}

start_time = Sys.time()
ga_result <- ga(type = "real-valued",
                fitness = fit_func_edit,
                lower = rep(-3,143),
                upper = rep(3,143),
                popSize = 1000,
                maxiter = iterations,
                monitor = monitor_func,
                run = 150)

# Get rid of any of the empty spaces in the table
ga_table <- ga_table[complete.cases(ga_table),]

# cat("            Time:",(proc.time() - ptm),"\n")
# cat(" Optimal Weights:",ga_result@solution[1,],"\n")

for (j in 1:10){
    new_model$weights[[1]][[1]][,j] <- ga_result@solution[1,][(1 + 11*(j-1)):(11*j)]
}
# Weights for the output layer
for(j in 1:3){
    new_model$weights[[1]][[2]][,j] <- ga_result@solution[1,][((111 + 11*(j-1))):(110 + (11*j))]
}

train_pred <- compute(new_model, abalone_train_set[!names(abalone_train_set) %in% c("young" ,"middling", "old")])$net.result %>%
    max.col %>%
    factor

train_acc <- confusionMatrix(train_pred, train_actual)$overall["Accuracy"]

test_pred <- compute(new_model, abalone_test_set[!names(abalone_test_set) %in% c("young" ,"middling", "old")])$net.result %>%
    max.col %>%
    factor

test_acc <- confusionMatrix(test_pred, test_actual)$overall["Accuracy"]

acc_df <- readRDS(file.path("..", "output", "acc_df.rds"))

max_iter <- max(ga_table$iteration)
acc_df <- acc_df %>%
        mutate(ga = c(train_acc,
                      test_acc,
                      ga_table[max_iter, "iteration"],
                      sum(ga_table$iter_time)))

saveRDS(new_model, file.path("..", "output", "ga_nn_model.rds"))
saveRDS(ga_result, file.path("..", "output", "ga_nn.rds"))
saveRDS(acc_df, file.path("..", "output", "acc_df.rds"))
saveRDS(ga_table, file.path("..", "output", "ga_table.rds"))
