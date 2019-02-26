
set.seed(57683)


# packages <- c("ggplot2", "dplyr", "tidyr", "GGally", "grid", "gridExtra", "stringr", "C50", "caret", "knitr")
packages <- c("dplyr","caret", "stringr", "tidyr")
lapply(packages, library, character.only = TRUE, quietly = TRUE)

#####################
#Change this to download the data from the UCI Machine Learning Database
download_data <- FALSE
######################

################
# Make sure that the working directory is the same as the source file
# paste("File Directory: ", dirname(parent.frame(2)$ofile))

tryCatch(
        setwd(dirname(parent.frame(2)$ofile)),
        error = function(e) {setwd("code")}
)


####################################
#  load_abalone_data

if (download_data){
  # Alternatively, the file can be read from the ML repository at UCI
        abalone <- read.csv(file = 'https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', header = FALSE)
        names(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")
        write.csv(abalone, file = file.path("..", "raw-input", "abalone", "abalone.data"), row.names = FALSE)
} else{
  abalone <- read.csv(file.path("..", "raw-input", "abalone", "abalone.data"), header = TRUE)
  # names(abalone) <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight", "viscera_weight", "shell_weight", "rings")
}



####################################
#  process_abalone

# Bins 
abalone <- abalone %>% 
        mutate(age = cut(abalone$rings, 
                         breaks = c(0,8, 10, 29), 
                         labels = c("young", "middling", "old")))



standardise_abalone <- preProcess(abalone, method = c("center", "scale"))
abalone <- data.frame(predict(standardise_abalone, newdata = abalone))


abalone <- abalone %>% 
        mutate(indicator = 1) %>% 
        select(-rings) %>% 
        spread(key = sex, 
               value = indicator,
               fill = 0)


# Separate into training and validation sets
# abalone_test_ind <- createDataPartition(abalone$sex, p = 0.2, list = FALSE)
abalone_test_ind <- createDataPartition(abalone$age, p = 0.2, list = FALSE)
abalone_train_set <- slice(abalone, -abalone_test_ind)
abalone_test_set <- slice(abalone, abalone_test_ind)
abalone_cx_train_set <- abalone_train_set

# abalone_valid_ind <- createDataPartition(abalone_train_set$sex, p = 0.2/.8, list = FALSE)
abalone_valid_ind <- createDataPartition(abalone_train_set$age, p = 0.2/.8, list = FALSE)
abalone_valid_set <- slice(abalone_train_set, abalone_valid_ind)
abalone_train_set <- slice(abalone_train_set, -abalone_valid_ind)


#####################################
#  load_adult_data


if (download_data){
  # Alternatively, the file can be read from the ML repository at UCI
        adult_train_set <- read.csv(file = 'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', header = FALSE)
  names(adult_train_set) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "earnings")

        adult_test_set <- read.csv(file = 'https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test', header = FALSE, skip = 1)
  names(adult_test_set) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "earnings")
        
        write.csv(adult_train_set, file = file.path("..", "raw-input", "adult", "adult.data"), row.names = FALSE)
        write.csv(adult_test_set, file = file.path("..", "raw-input", "adult", "adult.test"), row.names = FALSE)
} else{
  # The adult dataset comes with both a training and a test set already separated out, so they are loaded
  # separately.
        adult_train_set <- read.csv(file = file.path("..", "raw-input", "adult", "adult.data"), header = TRUE, stringsAsFactors = FALSE)
  # names(adult_train_set) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "earnings")

        adult_test_set <- read.csv(file = file.path("..", "raw-input", "adult", "adult.test"), header = TRUE, stringsAsFactors = FALSE)
  # names(adult_test_set) <- c("age", "workclass", "fnlwgt", "education", "education_num", "marital_status", "occupation", "relationship", "race", "sex", "capital_gain", "capital_loss", "hours_per_week", "native_country", "earnings")
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


# There are potentially meaningful NAs in workclass and occupation
# table(adult_train_set$workclass, useNA = "ifany")
# table(adult_train_set$occupation, useNA = "ifany")
# table(adult_train_set$education, useNA = "ifany")
# table(adult_train_set$`marital_status`, useNA = "ifany")
# table(adult_train_set$relationship, useNA = "ifany")
# table(adult_train_set$race, useNA = "ifany")
# table(adult_train_set$sex, useNA = "ifany")
# table(adult_train_set$earnings, useNA = "ifany")
# table(adult_test_set$occupation, useNA = "ifany")
# table(adult_test_set$`marital_status`, useNA = "ifany")
# table(adult_test_set$relationship, useNA = "ifany")
# table(adult_test_set$race, useNA = "ifany")
# table(adult_test_set$sex, useNA = "ifany")
# table(adult_test_set$earnings, useNA = "ifany")




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

adult_cx_train_set <- adult_train_set

adult_valid_ind <- createDataPartition(adult_train_set$earnings, p = .2, list = FALSE)
adult_valid_set <- slice(adult_train_set, adult_valid_ind)
adult_train_set <- slice(adult_train_set, -adult_valid_ind)

# Drop columns from the test set that don't exist in the training sets

adult_test_set <- adult_test_set[colnames(adult_test_set) %in% colnames(adult_train_set)]

sum(is.na(adult_test_set))

########################################
# learning_curve_partitions

# Doing this by percentage of the training set is inadequate to get an idea of how the validation error really changes - 5% of 5,000 is very different to 5% of 50,000. Instead, will do by absolute sizes. This will make it easier to visually compare the learning curve graphs as well.
# part <- seq(.05, 1, by = .05)

part <- data.frame(size = 2^(3:15)) %>%
        mutate(abalone = size/dim(abalone_train_set)[1],
               adult = size/dim(adult_train_set)[1])

part$abalone[min(which(part$abalone >= 1))] <- 1
part$abalone[which(part$abalone > 1)] <- NA

part$adult[min(which(part$adult >= 1))] <- 1
part$adult[which(part$adult > 1)] <- NA

# Create a list of indexes for subsets of the training sets for the learning curve.
abalone_lc_train_indexes <- list()
adult_lc_train_indexes <- list()
for (i in 1:dim(part)[1]){
        if(!is.na(part$abalone[i])){
                abalone_lc_train_indexes[[i]] <- createDataPartition(abalone_train_set$age,
                                                                        p = part$abalone[i], # The %'age of samples going into this partition
                                                                        list = F)
                names(abalone_lc_train_indexes)[i] <- paste0("partition_", part$size[i])
        }
        
        adult_lc_train_indexes[[i]] <- createDataPartition(adult_train_set$earnings,
                                                              p = part$adult[i],
                                                              list = F)
        names(adult_lc_train_indexes)[i] <- paste0("partition_", part$size[i])
}


#########################
# Save data to rds for ready access to machine learning files

rds_to_save <- c("abalone",
                 "abalone_train_set",
                 "abalone_test_set",
                 "abalone_valid_set",
                 "abalone_cx_train_set",
                 "adult_train_set",
                 "adult_test_set",
                 "adult_valid_set",
                 "adult_cx_train_set",
                 "abalone_lc_train_indexes",
                 "adult_lc_train_indexes",
                 "part")


for (i in 1:length(rds_to_save)){
        saveRDS(get(rds_to_save[i]), file.path("..", "input", paste0(rds_to_save[i],".rds")))
        # save(list = rds_to_save[i], file = file.path("..", "input", paste0(rds_to_save[i],".rda")))
}

