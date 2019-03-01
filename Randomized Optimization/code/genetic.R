# CS7641, Spring 2015, Randomized Optimization Assignment
# Genetic Algorithm Weighting of Pima Indians Neural Network

# Uncomment if you need to install
#install.packages("GA")
#install.packages("neuralnet")
#install.packages("gnew_models")
library("dplyr")
library("GA")
library("neuralnet")
library("gmodels")
library("caret")


# tryCatch(
#     setwd(dirname(parent.frame(2)$ofile)),
#     error = function(e) {setwd("code")}
# )

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
iterations <-500

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

# for (j in 1:3){
#     cat(paste("[(1 + 11*(j-1)): ", (111 + 11*(j-1)),"(11*j): ", 110+(11*j), "\n"))
# }


# fit_func <- function(string=c()) {
#       new_model$weights[[1]][[1]][,1] <- string[1:9]
#       new_model$weights[[1]][[1]][,2] <- string[10:18]
#       prediction <- compute(new_model, training_set[1:8])$net.result
#       classification <- apply(prediction, c(1), maxfactor)
#       prediction <- c('Diabetic', 'Normal')[classification]
#       result <- table(prediction,training_set$Diagnosis)
#       return(sum(prediction == training_set$Diagnosis)/sum(result))
# }

ptm <- proc.time()

# Using the abalone dataset with a neural network with 10 hidden nodes
# The abalone dataset has 10 features, meaning that there are 11 weights (inc. bias)
# for each of the nodes -> 110 weights. Plus 11 weights for each of the three output
# layer nodes -> 33 weights. So the genetic algorithm is going to be adjusting 
# 143 values, that are the weights of the new_model to optimize it. 

# monitor_func <- function(obj) 
# { 
#     contour(x1, x2, f, drawlabels = FALSE, col = grey(0.5))
#     title(paste("iteration =", obj@iter), font.main = 1)
#     points(obj@population, pch = 20, col = 2)
#     Sys.sleep(0.2)
# }

ga_result <- ga(type = "real-valued",
                fitness = fit_func_edit,
                lower = rep(-3,143),
                upper = rep(3,143),
                popSize = 1000,
                maxiter = iterations)

cat("            Time:",(proc.time() - ptm),"\n")
cat(" Optimal Weights:",ga_result@solution[1,],"\n")

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

acc_df <- acc_df %>% 
        mutate(ga = c(train_acc, test_acc))

saveRDS(new_model, file.path("..", "output", "ga_nn_model.rds"))
saveRDS(ga_result, file.path("..", "output", "ga_nn.rds"))
saveRDS(acc_df, file.path("..", "output", "acc_df.rds"))

