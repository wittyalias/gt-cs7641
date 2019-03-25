
packages <- c( "caret", "cluster", "mclust",  "neuralnet", "nFactors", "dplyr", "tidyr", "ClusterR", "fpc", "factoextra", "NbClust", "ica")
lapply(packages, library, character.only = TRUE, quietly = TRUE)

output_path <- file.path("..", "output")

library(gridExtra)

abalone_train_set <- readRDS(file.path("..", "input", "abalone_train_set.rds"))
abalone_test_set <- readRDS(file.path("..", "input", "abalone_test_set.rds"))

ab_train_set_x <- abalone_train_set %>% 
        select(-young, -middling, -old)

ab_train_set_x_alt <- ab_train_set_x %>% 
        select(-F, -M, -I)

ab_train_set_y <- abalone_train_set %>% 
        select(young, middling, old) %>% max.col(.) #%>% 
# c("young", "middling", "old")[.]

adult_train_set <- readRDS(file.path("..", "input", "adult_train_set.rds"))
adult_test_set <- readRDS(file.path("..", "input", "adult_test_set.rds"))

ad_train_set_x <- adult_train_set %>% 
        select(-earnings)

ad_train_set_x_alt <- ad_train_set_x %>% 
        select(age, education_num, capital_gain, capital_loss, hours_per_week)

ad_train_set_y <- adult_train_set %>% 
        select(earnings) %>% 
        mutate(indicator = 1, row = row_number()) %>% 
        spread(key = earnings, value = indicator, fill = 0) %>% 
        select(-row) %>% 
        max.col(.)

# Randomised projection - directly  Nathan Harmon's work
rca <- function(data, p = 2) {
        n <- ncol(data)
        u <- rnorm(n)
        u <- u/vecnorm(u)
        v <- rnorm(n)
        v <- v/vecnorm(v)
        Q <- cbind(u, v - sum(u * v) * u)
        dimnames(Q) <- NULL
        Data <- as.matrix(data) %*% Q
        Data
}


#########################################
# k-means abalone

gap_test_kmeans <- ab_train_set_x  %>% 
        clusGap(., FUN = kmeans, K.max = 10, 
                B = 100)

saveRDS(gap_test_kmeans, file.path(output_path, "gap_test_kmeans.rds"))

k_abalone <- ab_train_set_x %>% 
        kmeans(., 3)

saveRDS(k_abalone, file.path(output_path, "k_abalone.rds"))

f <- as.formula(paste("young + middling + old ~", paste(names(abalone_train_set)[!names(abalone_train_set) %in% c("young" ,"middling", "old")], collapse = "+"))) 

#########################################
# k-means abalone -ALT

gap_test_kmeans_alt <- ab_train_set_x_alt  %>% 
        clusGap(., FUN = kmeans, K.max = 10, 
                B = 100)

saveRDS(gap_test_kmeans_alt, file.path(output_path, "gap_test_kmeans_alt.rds"))

k_abalone_alt <- ab_train_set_x_alt %>% 
        kmeans(., 3)

saveRDS(k_abalone, file.path(output_path, "k_abalone_alt.rds"))



######################################
# EM abalone

mclust_gappy <- function(x, k){
        gappy <- Mclust(data = x, G = k)
        gappy$cluster <- max.col(gappy$z)
        return(gappy)
}

gap_test_em <- clusGap(ab_train_set_x, 
                       FUN = mclust_gappy, 
                       K.max = 10,
                       B = 1)

saveRDS(gap_test_em, file.path(output_path, "gap_test_em.rds"))

em_abalone <- Mclust(data = ab_train_set_x, 
                     G = 3)

saveRDS(em_abalone, file.path(output_path, "em_abalone.rds"))


####################################
# k-means adult

gap_kmeans_ad <- ad_train_set_x %>% 
        clusGap(., FUN = kmeans, K.max = 10, 
                B = 10)

saveRDS(gap_kmeans_ad, file.path(output_path, "gap_kmeans_ad.rds"))

k_adult <- ad_train_set_x %>% 
        kmeans(., 2)

saveRDS(k_adult, file.path(output_path, "k_adult.rds"))

####################################
# k-means adult - alt

gap_kmeans_ad_alt <- ad_train_set_x_alt %>% 
        clusGap(., FUN = kmeans, K.max = 10, 
                B = 10)

saveRDS(gap_kmeans_ad_alt, file.path(output_path, "gap_kmeans_ad_alt.rds"))

k_adult_alt <- ad_train_set_x_alt %>% 
        kmeans(., 2)

saveRDS(k_adult_alt, file.path(output_path, "k_adult_alt.rds"))

######################################
# EM adult
# 
# mclust_gappy <- function(x, k){
#         gappy <- Mclust(data = x, G = k)
#         gappy$cluster <- max.col(gappy$z)
#         return(gappy)
# }
# 
# gap_em_ad <- clusGap(ad_train_set_x, 
#                        FUN = mclust_gappy, 
#                        K.max = 10,
#                        B = 1)

# saveRDS(gap_em_ad, file.path(output_path, "gap_test_em.rds"))

em_adult <- Mclust(data = ad_train_set_x, 
                   G = 2)
# plot(em_adult, what = "classification")

saveRDS(em_adult, file.path(output_path, "em_adult.rds"))

######################################
# EM adult - ALT

# gap_em_ad_alt <- clusGap(ad_train_set_x_alt, 
#                      FUN = mclust_gappy, 
#                      K.max = 10,
#                      B = 1)
# 
# saveRDS(gap_em_ad_alt, file.path(output_path, "gap_test_em_alt.rds"))

em_adult_alt <- Mclust(data = ad_train_set_x_alt, 
                       G = 2)

# plot(em_adult_alt, what = "classification")

saveRDS(em_adult_alt, file.path(output_path, "em_adult_alt.rds"))


#########################################
# PCA abalone

pca_abalone <- princomp(ab_train_set_x)

# plot(pca_abalone, type = "lines")

pca_abalone_alt <- princomp(ab_train_set_x_alt)

saveRDS(pca_abalone, file.path(output_path, "pca_abalone.rds"))
saveRDS(pca_abalone_alt, file.path(output_path, "pca_abalone_alt.rds"))
plot(pca_abalone, type = "lines")

# biplot(pca_abalone)

#
# k-means PCA abalone

km_pca_ab <- kmeans(pca_abalone$scores[,1], 3)
saveRDS(km_pca_ab, file.path(output_path, "km_pca_ab.rds"))

# EM PCA abalone
em_pca_ab <- Mclust(data = pca_abalone$scores[,1], 
                    G = 2)
saveRDS(em_pca_ab, file.path(output_path, "em_pca_ab.rds"))

#########################################
# PCA adult

# pca_adult <- princomp(ad_train_set_x)

pca_adult_alt <- princomp(ad_train_set_x_alt)
saveRDS(pca_adult_alt, file.path(output_path, "pca_adult_alt.rds"))

plot(pca_adult_alt, type = "lines")

# k-means pca adult
km_pca_ad <- kmeans(pca_adult_alt$scores, 2)
saveRDS(km_pca_ad, file.path(output_path, "km_pca_ad.rds"))

# Em pca adult
em_pca_ad <- Mclust(data = pca_adult_alt$scores, 
                    G = 2)
saveRDS(em_pca_ad, file.path(output_path, "em_pca_ad.rds"))

#########################################
# ICA abalone

# ica_abalone <- fastICA(ab_train_set_x, 3)
# ica_abalone_alt <- fastICA(ab_train_set_x_alt, 5)

ica_abalone <- icafast(ab_train_set_x, 5)
ica_abalone_alt <- icafast(ab_train_set_x_alt, 5)

par(mfrow = c(1,2))
plot(ica_abalone$S, main = "ICA components")
plot(ica_abalone_alt$S, main = "ICA components")
par(mfrow = c(1,1))

saveRDS(ica_abalone, file.path(output_path, "ica_abalone.rds"))
saveRDS(ica_abalone_alt, file.path(output_path, "ica_abalone_alt.rds"))
# par(mfrow = c(1,3))
# plot(ica_abalone$X, main = "Pre-processed data")
# plot(ica_abalone$X %*% ica_abalone$K, main = "PCA components")
# plot(ica_abalone$S, main = "ICA components")
# par(mfrow = c(1,1))

# kmeans ica abalone
km_ica_ab <- kmeans(ica_abalone$S[,1:4], 3)
saveRDS(km_ica_ab, file.path(output_path, "km_ica_ab.rds"))

# em ica abalone
em_ica_ab <- Mclust(data = ica_abalone$S[,1:4], G = 3)
saveRDS(em_ica_ab, file.path(output_path, "em_ica_ab.rds"))


#########################################
# ICA adult

# ica_adult <- fastICA(ad_train_set_x, 4)
# ica_adult_alt <- fastICA(ad_train_set_x_alt, 4)

ica_adult <- icafast(ad_train_set_x, 4)
ica_adult_alt <- icafast(ad_train_set_x_alt, 4)

par(mfrow = c(1,2))
plot(ica_adult$S, main = "ICA components")
plot(ica_adult_alt$S, main = "ICA components")
par(mfrow = c(1,1))

saveRDS(ica_adult, file.path(output_path, "ica_adult.rds"))
saveRDS(ica_adult_alt, file.path(output_path, "ica_adult_alt.rds"))

# kmeans ica adult
km_ica_ad <- kmeans(ica_adult_alt$S, 2)
saveRDS(km_ica_ad, file.path(output_path, "km_ica_ad.rds"))

# Em pca adult
em_ica_ad <- Mclust(data = ica_adult_alt$S, G = 2)
saveRDS(em_ica_ad, file.path(output_path, "em_ica_ad.rds"))


#########################################
# Randomised projections abalone

rp_ab <- rca(ab_train_set_x, p = 4)
plot(rp_ab)
saveRDS(rp_ab, file.path(output_path, "rp_ab.rds"))

# kmeans rp abalone
km_rp_ab <- kmeans(rp_ab, 3)
saveRDS(km_rp_ab, file.path(output_path, "km_rp_ab.rds"))

# em ica abalone
em_rp_ad <- Mclust(data = rp_ab, G = 3)
saveRDS(em_rp_ad, file.path(output_path, "em_rp_ad.rds"))


#########################################
# Randomised projections adult

rp_ad <- rca(ad_train_set_x, p = 4)
plot(rp_ad)

saveRDS(rp_ad, file.path(output_path, "rp_ad.rds"))

# kmeans rp adult
km_rp_ad <- kmeans(rp_ad, 3)
saveRDS(km_rp_ad, file.path(output_path, "km_rp_ad.rds"))

# em ica adult
em_rp_ad <- Mclust(data = rp_ad, G = 3)
saveRDS(em_rp_ad, file.path(output_path, "em_rp_ad.rds"))

########################################
# Final dimension reducer

# # Leaps?
# f <- abalone_train_set %>% 
#         select(-M) %>% 
#         make_formula()

# regsub_ab <- abalone_train_set %>% 
#         select(-M) %>% 
#         regsubsets(f, .)

#glmnet

glmnet(as.matrix(ab_train_set_x), ab_train_set_y, family = "multinomial", type.multinomial = "grouped")

cvfit <- cv.glmnet(as.matrix(ab_train_set_x), ab_train_set_y, family = "multinomial", type.multinomial = "grouped")

coef(cvfit, s = "lambda.min")


###################################
# Neuarl networks on cluster algos

make_formula <- function(df){
        form <- as.formula(paste("young + middling + old ~", 
                                 paste(names(df)[!names(df) %in% c("young" ,"middling","old")], 
                                       collapse = "+"))) 
        return(form)
}

f <- make_formula(abalone_train_set)

net_table <- data.frame(time = c("Start time", "End time"))

start_time <- Sys.time()
km_nn_ab <- abalone_train_set %>% 
        mutate(cluster = k_abalone$cluster) %>% 
        neuralnet(formula = f, 
                  data = ., 
                  hidden = 10, 
                  algorithm = "backprop", 
                  threshold = 0.5, 
                  learningrate = 0.0001)
end_time <-  Sys.time()

net_table$km_nn_ab <- c(start_time, end_time)

start_time <- Sys.time()
em_nn_ab <- abalone_train_set %>% 
        mutate(cluster = em_abalone$classification) %>% 
        neuralnet(formula = f, 
                  data = ., 
                  hidden = 10, 
                  algorithm = "backprop", 
                  threshold = 0.5, 
                  learningrate = 0.0001)
end_time <-  Sys.time()

net_table$em_nn_ab <- c(start_time, end_time)

saveRDS(km_nn_ab, file.path(output_path, "km_nn_ab.rds"))
saveRDS(em_nn_ab, file.path(output_path, "em_nn_ab.rds"))


#########################################
# Neural network based on reduced dimensions - abalone


#PCA 

pca_nn_dat <- data.frame(pca_abalone$scores)[1] %>% 
        bind_cols( abalone_train_set[c("young", "middling", "old")])

f <- make_formula(pca_nn_dat)

start_time <- Sys.time()
pca_nn_ab <- pca_nn_dat %>% 
        neuralnet(formula = f, 
                  data = ., 
                  hidden = 10, 
                  algorithm = "backprop", 
                  threshold = 0.5, 
                  learningrate = 0.0001)
end_time <-  Sys.time()

net_table$pca_nn_ab <- c(start_time, end_time)

saveRDS(pca_nn_ab, file.path(output_path, "pca_nn_ab.rds"))

# ICA 

ica_nn_dat <- data.frame(ica_abalone$S)[,1:4] %>% 
        bind_cols(abalone_train_set[c("young", "middling", "old")])

f <- make_formula(ica_nn_dat)

start_time <- Sys.time()
ica_nn_ab <- ica_nn_dat %>% 
        neuralnet(formula = f, 
                  data = ., 
                  hidden = 10, 
                  algorithm = "backprop", 
                  threshold = 0.5, 
                  learningrate = 0.0001)
end_time <-  Sys.time()


net_table$ica_nn_ab <- c(start_time, end_time)

saveRDS(ica_nn_ab, file.path(output_path, "ica_nn_ab.rds"))

# Random projections

rp_nn_dat <- data.frame(rp_ab) %>% 
        bind_cols(abalone_train_set[c("young", "middling", "old")])

f <- make_formula(rp_nn_dat)

start_time <- Sys.time()
rp_nn_ab <- rp_nn_dat %>% 
        neuralnet(formula = f, 
                  data = ., 
                  hidden = 10, 
                  algorithm = "backprop", 
                  threshold = 0.5, 
                  learningrate = 0.0001)
end_time <-  Sys.time()


net_table$rp_nn_ab <- c(start_time, end_time)

saveRDS(rp_nn_ab, file.path(output_path, "rp_nn_ab.rds"))


