
packages <- c("dplyr", "caret", "cluster", "mclust", "fastICA", "neuralnet", "nFactors")
lapply(packages, library, character.only = TRUE, quietly = TRUE)



abalone_train_set <- readRDS(file.path("..", "input", "abalone_train_set.rds"))
abalone_valid_set <- readRDS(file.path("..", "input", "abalone_valid_set.rds"))

abalone_train_set <- bind_rows(abalone_train_set, abalone_valid_set)

adult_train_set <- readRDS(file.path("..", "input", "adult_train_set.rds"))
adult_valid_set <- readRDS(file.path("..", "input", "adult_valid_set.rds"))

adult_train_set <- bind_rows(adult_train_set, adult_valid_set)


#########################################
# k-means abalone

k_abalone <- abalone_train_set %>% 
        select(-young, -middling, -old) %>% 
        kmeans(., 6)

clusplot(abalone_train_set, k_abalone$cluster,
         color=TRUE, shade=TRUE, 
         labels=4, lines=0,# xlim=c(-6,4), ylim=c(-6,6),
         main=NULL,xlab='',ylab='',sub=NULL)


