
set.seed(57683)


packages <- c("dplyr","caret", "stringr", "tidyr")
lapply(packages, library, character.only = TRUE, quietly = TRUE)

################
# Make sure that the working directory is the same as the source file
# paste("File Directory: ", dirname(parent.frame(2)$ofile))

tryCatch(
        setwd(dirname(parent.frame(2)$ofile)),
        error = function(e) {setwd("code")}
)

####################################
#  load_abalone_data

if (!file.exists(file.path("..", "raw-input",  "abalone.data"))){
  # Alternatively, the file can be read from the ML repository at UCI
        abalone <- read.csv(file = 'https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = FALSE)
        names(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")
        write.csv(abalone, file = file.path("..", "raw-input", "abalone.data"), row.names = FALSE)
} else{
  abalone <- read.csv(file.path("..", "raw-input",  "abalone.data"), header = TRUE)
}

####################################
#  process_abalone

# Bins 
abalone <- abalone %>% 
        mutate(age = cut(abalone$rings, 
                         breaks = c(0,8, 10, 29), 
                         labels = c("young", "middling", "old")))

standardise_abalone <- preProcess(abalone, 
                                  method = c("center", "scale"))
abalone <- data.frame(predict(standardise_abalone, 
                              newdata = abalone))

# Separate into training and validation sets
# abalone_test_ind <- createDataPartition(abalone$sex, p = 0.2, list = FALSE)
abalone_test_ind <- createDataPartition(abalone$age, p = 0.2, list = FALSE)    
   
# Create dummy variables for use with the neuralnet package - age 
# NOTE: This can't be done before creating the partition, because the partition depends on age, which is what needs to be separated into different columnns
abalone <- abalone %>% 
        mutate(indicator = 1) %>% 
        spread(key = age,
               value = indicator, 
               fill = 0) %>% 
        mutate(indicator = 1) %>% 
        select(-rings) %>% 
        spread(key = sex, 
               value = indicator,
               fill = 0)

abalone_train_set <- slice(abalone, -abalone_test_ind)
abalone_test_set <- slice(abalone, abalone_test_ind)

# abalone_valid_ind <- createDataPartition(abalone_train_set$sex, p = 0.2/.8, list = FALSE)
# abalone_valid_ind <- createDataPartition(abalone_train_set$age, p = 0.2/.8, list = FALSE)
# abalone_valid_set <- slice(abalone_train_set, abalone_valid_ind)
# abalone_train_set <- slice(abalone_train_set, -abalone_valid_ind)


#########################
# Save data to rds for ready access to machine learning files

rds_to_save <- c("abalone",
                 "abalone_train_set",
                 "abalone_test_set")


for (i in 1:length(rds_to_save)){
        saveRDS(get(rds_to_save[i]), file.path("..", "input", paste0(rds_to_save[i],".rds")))
        # save(list = rds_to_save[i], file = file.path("..", "input", paste0(rds_to_save[i],".rda")))
}

rm(packages, rds_to_save, i, abalone_test_ind, abalone)
