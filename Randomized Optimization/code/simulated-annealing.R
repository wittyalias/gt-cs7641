# CS7641, Randomized Optimization Assignment
# Simulated Annealing Weighting  Neural Network

library("GenSA")
library("dplyr")
library("caret")
library("neuralnet")

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
iterations <- 100

# Neural Network as Fitness Function
step_num <- 0
fit_func <- function(string=c()) {

    # Weights for the hidden layer
    for (j in 1:10){
        new_model$weights[[1]][[1]][,j] <- string[(1 + 11*(j-1)):(11*j)]
    }
    # Weights for the output layer
    for(j in 1:3){
        new_model$weights[[1]][[2]][,j] <- string[((111 + 11*(j-1))):(110 + (11*j))]
    }

    prediction <- neuralnet::compute(new_model,
                          abalone_train_set[!names(abalone_train_set) %in% c("young" ,"middling", "old")])$net.result %>%
        max.col %>%
        factor
    
    accuracy <- confusionMatrix(prediction, train_actual)$overall["Accuracy"]
    
    # cat("Step number: ", step_num, "fitness: ", accuracy, "\n")
    # step_num <<- step_num + 1

    return(-accuracy)
}

sa_table <- data.frame(iteration = 1:iterations,
                       fitness_mean = NA,
                       fitness_max = NA,
                       start_time =  .POSIXct(rep(NA, iterations)),
                       end_time =  .POSIXct(rep(NA, iterations)),
                       iter_time = NA)


set.seed(1234)
global.min <- -1
tol <- 1e-13

start_time <- Sys.time()
sa_result <- GenSA(lower = rep(-3,143),
                   upper = rep(3,143),
                   fn = fit_func,
                   control = list(threshold.stop = global.min+tol,
                                  nb.stop.improvement = 100,
                                  smooth = TRUE,
                                  maxit = iterations,
                                  verbose = TRUE,
                                  temperature = 20)
                   )

end_time <- Sys.time()

 for (j in 1:10){
     new_model$weights[[1]][[1]][,j] <- sa_result$par[(1 + 11*(j-1)):(11*j)]
 }
 # Weights for the output layer
 for(j in 1:3){
     new_model$weights[[1]][[2]][,j] <- sa_result$par[((111 + 11*(j-1))):(110 + (11*j))]
 }

 train_pred <- neuralnet::compute(new_model, abalone_train_set[!names(abalone_train_set) %in% c("young" ,"middling", "old")])$net.result %>%
     max.col %>%
     factor

 train_acc <- confusionMatrix(train_pred, train_actual)$overall["Accuracy"]

 test_pred <- neuralnet::compute(new_model, abalone_test_set[!names(abalone_test_set) %in% c("young" ,"middling", "old")])$net.result %>%
     max.col %>%
     factor

 test_acc <- confusionMatrix(test_pred, test_actual)$overall["Accuracy"]

 acc_df <- readRDS(file.path("..", "output", "acc_df.rds"))

 # max_iter <- max(ga_table$iteration)
 acc_df <- acc_df %>% 
         mutate(sa = c(train_acc,
                       test_acc,
                       iterations,
                       difftime(end_time, start_time, units = "secs")))
 
 saveRDS(new_model, file.path("..", "output", "sa_nn_model.rds"))
 saveRDS(sa_result, file.path("..", "output", "sa_nn.rds"))
 saveRDS(acc_df, file.path("..", "output", "acc_df.rds"))
 # saveRDS(ga_table, file.path("..", "output", "ga_table.rds"))
