library(tidyverse)
library(ggthemes)
library(palmerpenguins)
library(MASS)
library(patchwork)



# -----------------------------
names(penguins)
dim(penguins)
# -----------------------------





tbl_pivoted_penguins <- penguins %>% dplyr::select(species, 
                                                   bill_length_mm, 
                                                   bill_depth_mm, 
                                                   flipper_length_mm, 
                                                   body_mass_g, 
                                                   sex) %>% 
  pivot_longer(cols = c(bill_length_mm, 
                        bill_depth_mm, 
                        flipper_length_mm, 
                        body_mass_g), 
               names_to = "measure_type", 
               values_to = "measure", 
               values_drop_na = TRUE) %>% 
  mutate(measure_type = case_when(str_detect(measure_type, "bill_length_mm") ~ "bill length", 
                                  str_detect(measure_type, "bill_depth_mm") ~ "bill depth", 
                                  str_detect(measure_type, "flipper_length_mm") ~ "flipper length", 
                                  str_detect(measure_type, "body_mass_g") ~ "body mass",
                                  TRUE ~ measure_type))


p_variations_by_species <- ggplot(data = tbl_pivoted_penguins, mapping = aes(x = species, y = measure)) + 
  geom_boxplot(mapping = aes(fill = species), 
               show.legend = FALSE, 
               varwidth = TRUE, 
               na.rm = TRUE) + 
  stat_summary(fun = "mean", color = "white", na.rm = TRUE, size = 0.25) + 
  facet_wrap(~measure_type, nrow = 2, scales = "free") + 
  scale_fill_brewer(palette = "Paired") +
  theme_grey() +
  labs(title = "Penguin Body Features", 
       subtitle = "Variation by Species")

p_variations_by_species

ggsave(filename = "D:\\R for Data Science\\R plots\\Tidy Tuesday - Palmer Penguins\\p_variations_by_species.png", plot = p_variations_by_species)


p_variations_by_sex <- filter(tbl_pivoted_penguins, !is.na(sex)) %>% ggplot(mapping = aes(x = sex, y = measure)) + 
  geom_boxplot(mapping = aes(fill = sex), 
               show.legend = FALSE, 
               na.rm = TRUE, 
               varwidth = TRUE) + 
  stat_summary(fun = "mean", color = "white", na.rm = TRUE, size = 0.25) + 
  scale_fill_brewer(palette = "Paired") +
  theme_grey() +
  facet_wrap(~measure_type, nrow = 2, scales = "free")  + 
  labs(title = "Penguin Body Features", 
       subtitle = "Variation by Gender/Sex")

p_variations_by_sex


ggsave(filename = "D:\\R for Data Science\\R plots\\Tidy Tuesday - Palmer Penguins\\p_variations_by_sex.png", plot = p_variations_by_sex)

p1 <- filter(penguins, !is.na(sex)) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = bill_depth_mm, y = bill_length_mm, color = species), 
             na.rm = TRUE, 
             show.legend = FALSE) + 
  scale_color_brewer(palette = "Set1") +
  labs(x = "bill depth", 
       y = "bill length")

p2 <- filter(penguins, !is.na(sex)) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = bill_length_mm, y = flipper_length_mm, color = species), 
             na.rm = TRUE, 
             show.legend = FALSE) + 
  scale_color_brewer(palette = "Set1") +
  labs(x = "bill length", 
       y = "flipper length")

p3 <- filter(penguins, !is.na(sex)) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = bill_depth_mm, y = flipper_length_mm, color = species), 
             na.rm = TRUE, 
             show.legend = FALSE) + 
  scale_color_brewer(palette = "Set1") +
  labs(x = "bill depth", 
       y = "flipper length")

p4 <- filter(penguins, !is.na(sex)) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species), 
             na.rm = TRUE, 
             show.legend  = TRUE) + 
  scale_color_brewer(palette = "Set1") +
  labs(x = "flipper length", 
       y = "body mass")


p_features_plots <- (p1 + p2) / (p3 + p4) + plot_layout(guides = 'collect') + 
  plot_annotation(title = "Penguins Body Features", 
                  subtitle = "Body features variation by species", theme = theme_grey())

ggsave(filename = "D:\\R for Data Science\\R plots\\Tidy Tuesday - Palmer Penguins\\p_features_plots.png", plot = p_features_plots)


tbl_filtered_penguins <- filter(penguins, !is.na(sex) & 
                                  !is.na(bill_length_mm) & 
                                  !is.na(bill_depth_mm) & 
                                  !is.na(flipper_length_mm) & 
                                  !is.na(body_mass_g))


subset_index <- sample(x = 1:nrow(tbl_filtered_penguins), size = round(0.8 * nrow(tbl_filtered_penguins), digits = 0))


penguins_train <- tbl_filtered_penguins[subset_index, ]
penguins_test <- tbl_filtered_penguins[-subset_index, ]


# A linear model for the penguins data set predicting Sex 
lda_fit <- lda(formula = sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = penguins_train)

# How the qualitative predictor has been split across  factors 
contrasts(penguins_train$sex)


# Prediction Outputs 

lda_pred <- predict(object = lda_fit, newdata = penguins_test)


# Correlation Matrix 
table(lda_pred$class, penguins_test$sex)


# success rate in prediction 
mean(lda_pred$class == penguins_test$sex)

# error rate in prediction 
mean(lda_pred$class != penguins_test$sex)

# Consistency of the model output ------------------

# Now lets try to split the input data set 
# into 80:20 ratio around 330 times (total record 
# count in the data set) by random sampling  and 
# calculate the error and success rates for each sample set
# We will get the average means of test success and test error rates

set.seed(1234)



fn_populate_rates <- function(){
  
  v_success_rates <- vector(mode = "double", length = nrow(tbl_filtered_penguins))
  v_error_rates <- vector(mode = "double", length = nrow(tbl_filtered_penguins))
  
  
  for(i in seq(nrow(tbl_filtered_penguins))){
    subset_val <- sample(x = 1:nrow(tbl_filtered_penguins), size = round(0.8 * nrow(tbl_filtered_penguins), digits = 0))
    lda_fit1 <- lda(formula = sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = tbl_filtered_penguins[subset_val, ])    
    lda_pred1 <- predict(object = lda_fit1, newdata = tbl_filtered_penguins[-subset_val, ])
    v_success_rates[[i]] <- mean(lda_pred1$class == tbl_filtered_penguins[-subset_val, ]$sex)
    v_error_rates[[i]] <- mean(lda_pred1$class != tbl_filtered_penguins[-subset_val, ]$sex)
    i <- i + 1
  }
  
  df_rates <- cbind(v_success_rates, v_error_rates)
  return(df_rates)
  
}

df_rates_new <- as_tibble(fn_populate_rates())

mean(df_rates_new$v_success_rates)

mean(df_rates_new$v_error_rates)

# Consistency of the model output ------------------



# Validation Set Approach  -----------------------

# Split the input data set into two halves 
# and study the test error and test success rates of the model 

set.seed(1234)


fn_cross_validation <- function(){
  
  v_success_rates <- vector(mode = "double", length = 1)
  v_error_rates <- vector(mode = "double", length = 1)
  
  subset_val <- sample(x = 1:nrow(tbl_filtered_penguins), size = round(0.5 * nrow(tbl_filtered_penguins), digits = 0))
  lda_fit1 <- lda(formula = sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = tbl_filtered_penguins[subset_val, ])    
  lda_pred1 <- predict(object = lda_fit1, newdata = tbl_filtered_penguins[-subset_val, ])
  
  v_success_rates[[1]] <- mean(lda_pred1$class == tbl_filtered_penguins[-subset_val, ]$sex)
  v_error_rates[[1]] <- mean(lda_pred1$class != tbl_filtered_penguins[-subset_val, ]$sex)
  
  df_rates <- cbind(v_success_rates, v_error_rates)
  return(df_rates)
  
}

df_rates_cross_validation <- as_tibble(fn_cross_validation())

mean(df_rates_cross_validation$v_success_rates)

mean(df_rates_cross_validation$v_error_rates)

# Cross Validation -----------------------


# Leave one out cross validation -----------------

# Use n - 1 observations as the training set and 
# 1 observation as the test set 
# calculate the test success and test error rates 

set.seed(1234)



fn_loov_rates <- function(){
  
  v_success_rates <- vector(mode = "double", length = nrow(tbl_filtered_penguins))
  v_error_rates <- vector(mode = "double", length = nrow(tbl_filtered_penguins))
  
  
  for(i in seq(nrow(tbl_filtered_penguins))){
    
    lda_fit1 <- lda(formula = sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = tbl_filtered_penguins[-i, ])    
    lda_pred1 <- predict(object = lda_fit1, newdata = tbl_filtered_penguins[i, ])
    v_success_rates[[i]] <- mean(lda_pred1$class == tbl_filtered_penguins[i, ]$sex)
    v_error_rates[[i]] <- mean(lda_pred1$class != tbl_filtered_penguins[i, ]$sex)
    i <- i + 1
  }
  
  df_rates <- cbind(v_success_rates, v_error_rates)
  return(df_rates)
  
}

df_loov_rates <- as_tibble(fn_loov_rates())

mean(df_loov_rates$v_success_rates)

mean(df_loov_rates$v_error_rates)

# Leave one out cross validation -----------------

# K-Fold Cross Validations -----------------

# K = 10 fold cross validation -------------------

# Split the data set into 10 subsets 
# Run the model on these 9 subsets as the training set 
# and 1 subset as the test set 

set.seed(1234)

sub_df <- as.data.frame(x = 1:nrow(tbl_filtered_penguins))

colnames(sub_df) <- "row_index_no"

sub_df$bucket <- trunc(sub_df$row_index_no / round(nrow(sub_df)/10, digits = 0))



fn_ktenfold_rates <- function(){
  
  v_success_rates <- vector(mode = "double", length = max(sub_df$bucket))
  v_error_rates <- vector(mode = "double", length = max(sub_df$bucket))
  
  
  for(i in seq(max(sub_df$bucket))){
    
    lda_fit1 <- lda(formula = sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = tbl_filtered_penguins[-sub_df[sub_df$bucket == i-1, ]$row_index_no
                                                                                                                                   , ])    
    lda_pred1 <- predict(object = lda_fit1, newdata = tbl_filtered_penguins[sub_df[sub_df$bucket == i-1, ]$row_index_no, ])
    v_success_rates[[i]] <- mean(lda_pred1$class == tbl_filtered_penguins[sub_df[sub_df$bucket == i-1, ]$row_index_no, ]$sex)
    v_error_rates[[i]] <- mean(lda_pred1$class != tbl_filtered_penguins[sub_df[sub_df$bucket == i-1, ]$row_index_no, ]$sex)
    i <- i + 1
  }
  
  df_rates <- cbind(v_success_rates, v_error_rates)
  return(df_rates)
  
}

df_ktenfold_rates <- as_tibble(fn_ktenfold_rates())

mean(df_ktenfold_rates$v_success_rates)

mean(df_ktenfold_rates$v_error_rates)


# K = 10 fold cross validation -------------------


# K = 5 fold cross validation -------------------

set.seed(1234)

sub_df <- as.data.frame(x = 1:nrow(tbl_filtered_penguins))

colnames(sub_df) <- "row_index_no"

sub_df$bucket <- trunc(sub_df$row_index_no / round(nrow(sub_df)/5, digits = 0))



fn_kfivefold_rates <- function(){
  
  v_success_rates <- vector(mode = "double", length = max(sub_df$bucket))
  v_error_rates <- vector(mode = "double", length = max(sub_df$bucket))
  
  
  for(i in seq(max(sub_df$bucket))){
    
    lda_fit1 <- lda(formula = sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = tbl_filtered_penguins[-sub_df[sub_df$bucket == i-1, ]$row_index_no
                                                                                                                                   , ])    
    lda_pred1 <- predict(object = lda_fit1, newdata = tbl_filtered_penguins[sub_df[sub_df$bucket == i-1, ]$row_index_no, ])
    v_success_rates[[i]] <- mean(lda_pred1$class == tbl_filtered_penguins[sub_df[sub_df$bucket == i-1, ]$row_index_no, ]$sex)
    v_error_rates[[i]] <- mean(lda_pred1$class != tbl_filtered_penguins[sub_df[sub_df$bucket == i-1, ]$row_index_no, ]$sex)
    i <- i + 1
  }
  
  df_rates <- cbind(v_success_rates, v_error_rates)
  return(df_rates)
  
}

df_kfivefold_rates <- as_tibble(fn_kfivefold_rates())

mean(df_kfivefold_rates$v_success_rates)

mean(df_kfivefold_rates$v_error_rates)


# K = 5 fold cross validation -------------------




# Generalizing to k = N groups 
# and using this generalization to study the 
# model result of k = 3 to k = 50 groups in the observation 

set.seed(1234)

fn_kfold_rates_new <- function(k) {
  
  sub_df <- as.data.frame(x = 1:nrow(tbl_filtered_penguins))
  
  colnames(sub_df) <- "row_index_no"
  
  sub_df$bucket <- trunc(sub_df$row_index_no / round(nrow(sub_df)/k, digits = 0))
  
  
  v_success_rates <- vector(mode = "double", length = max(sub_df$bucket))
  v_error_rates <- vector(mode = "double", length = max(sub_df$bucket))
  
  
  for(i in seq(max(sub_df$bucket))){
    
    lda_fit1 <- lda(formula = sex ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = tbl_filtered_penguins[-sub_df[sub_df$bucket == i-1, ]$row_index_no
                                                                                                                                   , ])    
    lda_pred1 <- predict(object = lda_fit1, newdata = tbl_filtered_penguins[sub_df[sub_df$bucket == i-1, ]$row_index_no, ])
    v_success_rates[[i]] <- mean(lda_pred1$class == tbl_filtered_penguins[sub_df[sub_df$bucket == i-1, ]$row_index_no, ]$sex)
    v_error_rates[[i]] <- mean(lda_pred1$class != tbl_filtered_penguins[sub_df[sub_df$bucket == i-1, ]$row_index_no, ]$sex)
    i <- i + 1
  }
  
  
  df_rates <- data.frame(cbind(v_success_rates, v_error_rates))
  colnames(df_rates) <- c("success_rate", "error_rate")
  
  k_value <- rep(k, nrow(df_rates))
  
  df_rates <- cbind(df_rates, k_value)
  
  colnames(df_rates) <- c("success_rate", "error_rate", "k_value")
  return(df_rates)
  
}



# Set the input so that this can be called
# iteratively with purrr::map function to 
# to call the above function

v_k_folds <- c(3:50)


df_kfold_rates_new <- purrr::map_df(.x = v_k_folds, .f = fn_kfold_rates_new)


tbl_kfold_rates <- as_tibble(df_kfold_rates_new)



p_error_rate <-  tbl_kfold_rates %>% group_by(k_value) %>% 
  summarize(mean_test_error = mean(error_rate)) %>%
  ungroup() %>% 
  ggplot(mapping = aes(x = k_value, y = mean_test_error)) + 
  geom_point(colour = "red", size = 2.0) + 
  geom_line(colour = "blue", size = 0.75) + 
  labs(x = "K Value - Degree of Flexibility", 
       y = "Mean Test Error")

p_success_rate <-  tbl_kfold_rates %>% group_by(k_value) %>% 
  summarize(mean_test_success = mean(success_rate)) %>%
  ungroup() %>% 
  ggplot(mapping = aes(x = k_value, y = mean_test_success)) + 
  geom_point(colour = "red", size = 2.0) + 
  geom_line(colour = "blue", size = 0.75) + 
  labs(x = "K Value - Degree of Flexibility", 
       y = "Mean Test Success")

p_kfold_plots <- (p_error_rate / p_success_rate) + plot_annotation(title = "K-Fold Cross Validation Results", 
                                                  subtitle = "Mean Test Error/Success Rates", theme = theme_grey())

ggsave(filename = "D:\\R for Data Science\\R plots\\Tidy Tuesday - Palmer Penguins\\p_kfold_plots.png", plot = p_kfold_plots)

