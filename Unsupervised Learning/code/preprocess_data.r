
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



#####################################
#  load_adult_data

# If the adult data files don't exists, then download them from the UCI repository and save them, otherwise, load from the local copy
if (!file.exists(file.path("..", "raw-input",  "adult.data"))){
        adult_train_set <- read.csv(file = 'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data',
                                    header = FALSE)
        names(adult_train_set) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "earnings")
        
        adult_test_set <- read.csv(file = 'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test',
                                   header = FALSE, skip = 1)
        names(adult_test_set) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "earnings")
        
        write.csv(adult_train_set, file = file.path("..", "raw-input",  "adult.data"), row.names = FALSE)
        write.csv(adult_test_set, file = file.path("..", "raw-input",  "adult.test"), row.names = FALSE)
} else{
        # The adult dataset comes with both a training and a test set already separated out, so they are loaded
        # separately.
        adult_train_set <- read.csv(file = file.path("..", "raw-input",  "adult.data"), 
                                    header = TRUE, 
                                    stringsAsFactors = FALSE)
        adult_test_set <- read.csv(file = file.path("..", "raw-input", "adult.test"), 
                                   header = TRUE, 
                                   stringsAsFactors = FALSE)
}


################################
#  process_adult

# The adult dataset needs a bit of processing - mostly in terms of one-hot encoding,
# but it could also use some collapsing of some of the factor levels.

adult_test_set$earnings <-  as.character(adult_test_set$earnings) %>%
        str_replace("\\.", "")
adult_test_set$earnings[adult_test_set$earnings == ""] <- NA

# Get rid of the excess white space
adult_train_set <- adult_train_set %>%
        mutate_if(is.character, str_trim)
adult_test_set <- adult_test_set %>%
        mutate_if(is.character, str_trim)

# Convert the missing values to NA
adult_train_set[adult_train_set == "?"] <- NA
adult_test_set[adult_test_set == "?"] <- NA

# Holland, Netherlands only exists in the train set. Its actually possible that the country should be reduced to USA and other, but since this assignment is about comparing models, I'm not going to get into feature selection right now.
adult_train_set$native_country[adult_train_set$native_country == "Holand-Netherlands"] <-NA

#Education and education_num code the same variable in different ways as a factor and a numeric respectively.
# fnlwgt is the weight for each observation when looking to estimate weighted tallies of socio-economic characteristics of the population.
# Dropping both fnlwgt and education.
adult_train_set <- adult_train_set %>%
        select(-education, -fnlwgt) %>%
        mutate(earnings = factor(earnings))%>%
        mutate(workclass = ifelse(is.na(workclass), "unknown", workclass),
               occupation = ifelse(is.na(occupation), "unknown", occupation),
               native_country = ifelse(is.na(native_country), "unknown", native_country))

adult_test_set <- adult_test_set %>%
        select(-education, -fnlwgt) %>%
        mutate(earnings = factor(earnings))%>%
        mutate(workclass = ifelse(is.na(workclass), "unknown", workclass),
               occupation = ifelse(is.na(occupation), "unknown", occupation),
               native_country = ifelse(is.na(native_country), "unknown", native_country))


# Scale the data and center around zero

standardised <- preProcess(adult_train_set, method = c("center", "scale"))
adult_train_set <- data.frame(predict(standardised, newdata = adult_train_set))
adult_test_set <- data.frame(predict(standardised, newdata = adult_test_set))

dummies <- dummyVars(earnings ~ ., data = adult_train_set)
adult_train_set <- bind_cols(adult_train_set["earnings"],
                             data.frame(predict(dummies, newdata = adult_train_set)))
adult_test_set <- bind_cols(adult_test_set["earnings"],
                            data.frame(predict(dummies, newdata = adult_test_set)))

# adult_valid_ind <- createDataPartition(adult_train_set$earnings, p = .2, list = FALSE)
# adult_valid_set <- slice(adult_train_set, adult_valid_ind)
# adult_train_set <- slice(adult_train_set, -adult_valid_ind)

# Drop columns from the test set that don't exist in the training sets

adult_test_set <- adult_test_set[colnames(adult_test_set) %in% colnames(adult_train_set)]

#########################
# Save data to rds for ready access to machine learning files

rds_to_save <- c("abalone",
                 "abalone_train_set",
                 "abalone_test_set", 
                 "abalone_test_ind",
                 "adult_train_set",
                 "adult_test_set")


for (i in 1:length(rds_to_save)){
        saveRDS(get(rds_to_save[i]), file.path("..", "input", paste0(rds_to_save[i],".rds")))
        # save(list = rds_to_save[i], file = file.path("..", "input", paste0(rds_to_save[i],".rda")))
}

rm(packages, rds_to_save, i, abalone_test_ind, abalone)


