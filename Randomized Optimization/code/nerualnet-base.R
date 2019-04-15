
library("neuralnet")
library("dplyr")
library("caret")

set.seed(12345)

#load the data
abalone_test_set <- readRDS(file.path("..", "input",  "abalone_test_set.rds"))
abalone_train_set <- readRDS(file.path("..", "input",  "abalone_train_set.rds"))

# This place revealed the mystery of the nearualnet formula https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
f <- as.formula(paste("young + middling + old ~", paste(names(abalone_train_set)[!names(abalone_train_set) %in% c("young" ,"middling", "old")], collapse = "+"))) 

train_actual <- abalone_train_set %>% 
    select(young, middling, old) %>% 
    max.col %>% 
        factor

test_actual <- abalone_test_set %>% 
        select(young, middling, old) %>% 
        max.col %>% 
        factor

start_time <- Sys.time()
# Create our neural network model
model <- neuralnet(formula = f,
                 data = abalone_train_set,
                 hidden = 10,
                 algorithm = "backprop",
                 threshold = 0.5,
                 learningrate = 0.0001)

end_time <-  Sys.time()

abalone_map <- data.frame(age = c("young" ,"middling", "old"), model_result = 1:3)

nn_test_results <- neuralnet::compute(model, abalone_test_set[!names(abalone_train_set) %in% c("young" ,"middling", "old")])$net.result %>% 
        max.col %>% 
        factor()

model_results <- model$net.result[[1]] %>% 
    max.col %>% 
        factor()

train_acc <- confusionMatrix(model_results, train_actual)$overall["Accuracy"]
test_acc <- confusionMatrix(nn_test_results, test_actual)$overall["Accuracy"]
acc_df <- data.frame(Metric = c("Training Accuracy", 
                                "Test Accuracy", 
                                "Iterations", 
                                "Total time"), 
                     nn_base = c(train_acc, 
                                 test_acc, 
                                 model$result.matrix["steps",], 
                                 difftime(end_time, start_time, units = "secs")))


orig_model <- readRDS(file.path("..", "output", "nn-base.rds"))

saveRDS(model, file.path("..", "output", "nn-base.rds"))
saveRDS(acc_df, file.path("..", "output", "acc_df.rds"))

