# CS7641, Randomized Optimization Assignment
# Simulated Annealing Weighting  Neural Network

# Uncomment if you need to install
#install.packages("GenSA")
#install.packages("neuralnet")
#install.packages("gmodels")
require("GenSA")
require("neuralnet")
require("gmodels")

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

sa_table <- data.frame(iteration = 1:iterations,
                       fitness_mean = NA,
                       fitness_max = NA,
                       start_time =  .POSIXct(rep(NA, iterations)),
                       end_time =  .POSIXct(rep(NA, iterations)),
                       iter_time = NA)


set.seed(1234)
global.min <- 0
tol <- 1e-13
sa_result <- GenSA(lower=rep(-50,18),
                   upper=rep(50,18),
                   fn = fit_func,
                   control = list(threshold.stop=global.min+tol,
                                  maxit=iterations,
                                  verbose=TRUE,
                                  temperature=10)
                   )

cat("            Time:",(proc.time() - ptm),"\n")
cat(" Optimal Weights:",as.vector(sa_result$par),"\n")

model$weights[[1]][[1]][,1] <- as.vector(sa_result$par[1:9])
model$weights[[1]][[1]][,2] <- as.vector(sa_result$par[10:18])

prediction <- compute(model, testing_set[1:8])$net.result
classification <- apply(prediction, c(1), maxfactor)
prediction <- c('Diabetic', 'Normal')[classification]
result <- table(prediction,testing_set$Diagnosis)

cat("        Accuracy:",sum(prediction == testing_set$Diagnosis)/sum(result),"\n")

CrossTable(x = testing_set$Diagnosis,
           y = prediction,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE,
           dnn = c("Actual", "Prediction"))
