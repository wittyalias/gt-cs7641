# CS7641, Randomized Optimization Assignment
# Simulated Annealing Weighting 

# Uncomment if you need to install
#install.packages("neuralnet")
#install.packages("gmodels")

library("neuralnet")
library("caret")
library("gtools")



set.seed(12345)

#load the data
abalone_test_set <- readRDS(file.path("..", "input",  "abalone_test_set.rds"))
abalone_train_set <- readRDS(file.path("..", "input",  "abalone_train_set.rds"))

#load the model
model <- readRDS(file = file.path("..", "output", "nn-base.rds"))

#create a new model for manipulation - keep the old one for working comparison
new_model <- model

train_actual <- abalone_train_set %>%
    select(young, middling, old) %>%
    max.col %>%
    factor(levels = c(1,2,3))

test_actual <- abalone_test_set %>%
    select(young, middling, old) %>%
    max.col %>%
    factor


# Neural Network as Fitness Function

# fit_func <- function(string=c()) {
#   model$weights[[1]][[1]][,1] <- string[1:9]
#   model$weights[[1]][[1]][,2] <- string[10:18]
#   prediction <- compute(model, training_set[1:8])$net.result
#   classification <- apply(prediction, c(1), maxfactor)
#   prediction <- c('Diabetic', 'Normal')[classification]
#   result <- table(prediction,training_set$Diagnosis)
#   return(sum(prediction == training_set$Diagnosis)/sum(result))
# }

fit_func <- function(string=c()) {

    # Weights for the hidden layer
    for (j in 1:10){
        new_model$weights[[1]][[1]][,j] <<- string[(1 + 11*(j-1)):(11*j)]
    }
    # Weights for the output layer
    for(j in 1:3){
        new_model$weights[[1]][[2]][,j] <<- string[((111 + 11*(j-1))):(110 + (11*j))]
    }

    prediction <- compute(new_model,
                          abalone_train_set[!names(abalone_train_set) %in% c("young" ,"middling", "old")])$net.result %>%
        max.col %>%
        factor(levels = c(1,2,3))

    return(confusionMatrix(prediction, train_actual)$overall["Accuracy"])
}

# Settings
random_restarts <- 100
start_time <- Sys.time()

# data frame to collect information about the algorithm to compare to others

rhc_table <- data.frame(iteration = 1:random_restarts,
                       fitness = NA,
                       start_time =  .POSIXct(rep(NA, random_restarts)),
                       end_time =  .POSIXct(rep(NA, random_restarts)),
                       iter_time = NA)


global_accuracy <- 0
# global_weights <- list()
weight_step <- 0.1

# ptm <- proc.time()




for (i in 1:random_restarts) {
  # weights <- sample(-1000:1000,18,replace=T)/1000
        nsteps <- 0
        weights <- runif(143, min = -2, max = 2)
        accuracy <- fit_func(weights)
        current_accuracy <- accuracy
        #cat("---------------------------------------------------\n")
        #cat("New Random Start:",weights)
        #cat(" (",accuracy,")\n", sep="")
        
        
        # Then look at each weight, add some small amount to it and check the accuracy
        # if the accuracy is better, adjust the weight -> move in that direction on the
        # fitness function.
        accuracy_improved = TRUE
        while(accuracy_improved){
            # Randomise the order you deal with the weights in
            # weight_order <- sample(1:18,18,replace=F)
            # weight_order <- gtools::permute(1:143)
            current_accuracy <- accuracy
        
            for (j in 1:143){#gtools::permute(1:143)){
                      new_weights <- weights
                      new_weights[j] <- weights[j] + weight_step
                      new_accuracy <- fit_func(new_weights)
                
                      if ( new_accuracy > accuracy) {
                        accuracy <- new_accuracy
                        weights <- new_weights
                        #at("                :",weights)
                        #cat(" (",accuracy,")\n", sep="")
                        next
                      }
                
                      new_weights <- weights
                      new_weights[j] <- weights[j] - weight_step
                      new_accuracy <- fit_func(new_weights)
                
                      if ( new_accuracy > accuracy) {
                        accuracy <- new_accuracy
                        weights <- new_weights
                        #cat("                :",weights)
                        #cat(" (",accuracy,")\n", sep="")
                        next
                        }
            }
            # If the current accuracy is the same or less than the ideal accuracy
            # then it hasn't improved and the algo is at a maximum
            if (current_accuracy >= accuracy) accuracy_improved = FALSE
            nsteps <- nsteps + 1
        }
                
        #Accuracy no longer improved - compare the most recent maxima to the current global
        # update the global if necessary
        if ( accuracy > global_accuracy) {
                global_accuracy <<- accuracy
                global_weights <<- weights
                #cat("      New Optima:",weights)
                #cat(" (",accuracy,")\n", sep="")
        }
        
        end_time <- Sys.time()
        
        rhc_table[i, "fitness"] <- accuracy
        rhc_table[i, "start_time"] <- start_time
        rhc_table[i, "end_time"] <- end_time
        rhc_table[i, "iter_time"] <- difftime(end_time, start_time, units = "secs")
        
        start_time <<- Sys.time()
        
        cat("Random Restart No: ", i,  "Accuracy: ", accuracy, "; Number of steps: ", nsteps, "\n")
# BACK TO THE BEGINNING WITH A RANDOM RESTART!
}


for (j in 1:10){
    new_model$weights[[1]][[1]][,j] <- global_weights[(1 + 11*(j-1)):(11*j)]
}
# Weights for the output layer
for(j in 1:3){
    new_model$weights[[1]][[2]][,j] <- global_weights[((111 + 11*(j-1))):(110 + (11*j))]
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
        mutate(rhc = c(train_acc,
                      test_acc,
                      random_restarts,
                      sum(rhc_table$iter_time)))

saveRDS(new_model, file.path("..", "output", "rhc_nn_model.rds"))
# saveRDS(ga_result, file.path("..", "output", "rhc_nn.rds"))
saveRDS(acc_df, file.path("..", "output", "acc_df.rds"))
saveRDS(rhc_table, file.path("..", "output", " rhc_table.rds"))

# prediction <- compute(model, testing_set[1:8])$net.result
# classification <- apply(prediction, c(1), maxfactor)
# prediction <- c('Diabetic', 'Normal')[classification]
# result <- table(prediction,testing_set$Diagnosis)
#
# cat("  Testing Accuracy:",sum(prediction == testing_set$Diagnosis)/sum(result),"\n")
#
# CrossTable(x = testing_set$Diagnosis,
#            y = prediction,
#            prop.r = FALSE,
#            prop.c = FALSE,
#            prop.t = FALSE,
#            prop.chisq = FALSE,
#            dnn = c("Actual", "Prediction"))
