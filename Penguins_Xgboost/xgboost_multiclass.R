library(tidyverse)
library(palmerpenguins)
library(xgboost)


# Load the data into a data frame 
penguins_data_full <- palmerpenguins::penguins


penguins_data_full <- as.data.frame(penguins_data_full, 
                                    stringsAsFactors = FALSE)


# describe the data 
dim(penguins_data_full)
names(penguins_data_full)

# the penguins species and sex distribution in the data 
penguins_data_full %>% count(species, sort = TRUE)
penguins_data_full %>% count(sex, sort = TRUE)


# check which variables NA values 

v_na_values <- apply(X = penguins_data_full, 
                     MARGIN = 2, 
                     FUN = function(x) sum(is.na(x)))


v_na_values[v_na_values > 0]


# populate missing values with mean of each numeric variables

penguins_data_full[, c(3, 4, 5, 6)] <- apply(X = penguins_data_full[, c(3, 4, 5, 6)], 
                                             MARGIN = 2, 
                                             FUN = function(x) ifelse(is.na(x), round(mean(x, na.rm = TRUE), 
                                                                                      digits = 1), 
                                                                      x))

# populate missing sex values with maximum occurrences of sex type
penguins_data_full$sex <- ifelse(is.na(penguins_data_full$sex), 
                                 names(table(penguins$sex)[table(penguins$sex) == max(table(penguins$sex))]), 
                                 as.character(penguins_data_full$sex))

# removing unwanted variables island and year 
# from modeling - island and year  
penguins_data <- penguins_data_full[, -c(2, 8)]

# converting the factor variables into a sparse integer matrix
# for xgboost inputs 

penguins_data$sex <- as.integer(as.factor(penguins_data$sex))

# IMPORTANT TO NOTE :- 
# the response classes needs to be converted to 
# integers STARTING from 0. Hence a -1 is done

penguins_data$species <- as.integer(as.factor(penguins_data$species)) - 1


# Start of the Modeling Process 
# Objective :- To predict the species as response
# with the selected set of predictors except island and year

set.seed(1234)


# splitting the data set into train and test 
train_index <- sample(1:nrow(penguins_data), round(0.8*nrow(penguins_data), 
                                                   digits = 0))
test_index <- setdiff(1:nrow(penguins_data), 
                      train_index)


# create the training variables and label variables for training 
penguins_train <- as.matrix(penguins_data[train_index, -1])
label_train <- as.matrix(penguins_data[train_index, 1])

# create the test variables and label variables for testing 
penguins_test <- as.matrix(penguins_data[test_index, -1])
label_test <- as.matrix(penguins_data[test_index, 1])


# create the xgb.Dmatrix objects for train and test
xgb_train <- xgb.DMatrix(data = penguins_train, 
                         label = label_train)
xgb_test <- xgb.DMatrix(data = penguins_test, 
                        label = label_test)

# Find the number of classification outcomes 
no_classes <- length(unique(penguins_data$species))

# Create the parameter list for xgboost 
param_list <- list(booster = "gbtree", 
                   eta = 0.001, 
                   max_depth = 5, 
                   gamma = 3, 
                   subsample = 0.75, 
                   colsample_bytree = 1, 
                   objective = "multi:softprob", # for multiple class response predictions 
                   eval_metric = "mlogloss", 
                   num_class = no_classes)

xgb_fit <- xgb.train(params = param_list, 
                     data = xgb_train, 
                     nrounds = 10000,
                     early_stopping_rounds = 10, 
                     watchlist = list(val1 = xgb_train, 
                                      val2 = xgb_test), 
                     verbose =  1)

# Results converging after 4035 iterations 
xgb_fit


# predicting new responses 
xgb_pred <- predict(object = xgb_fit, 
                    newdata = penguins_test, 
                    reshape = TRUE)

xgb_pred <- as.data.frame(xgb_pred, stringsAsFactors = FALSE)

xgb_pred$max_prediction <- apply(X = xgb_pred, 
                                 MARGIN = 1, 
                                 FUN = function(x) colnames(xgb_pred)[which.max(x)])


# Calculating prediction accuracy 

v_species_lkp <- levels(penguins_data_full$species)
names(v_species_lkp) <- c("V1", "V2", "V3")

xgb_pred$species <- unname(v_species_lkp[xgb_pred$max_prediction])

final_penguins_data <- cbind(penguins_data_full[test_index, ], 
                             "pred_species" = xgb_pred$species)

# first 6 rows of the predicted data set 
head(final_penguins_data)


# distribution & reconciliation counts 
# per group 

as_tibble(final_penguins_data) %>% count(species, 
                                         pred_species, 
                                         sort = TRUE) %>% 
  mutate(recon = ifelse(species == pred_species, 
                        "matched", 
                        "not matched")) %>% 
  arrange(recon, species, pred_species) 


# prediction accuracy figure  
pred_accuracy <- round(sum(ifelse(final_penguins_data$species == final_penguins_data$pred_species, 1, 0))/nrow(final_penguins_data) * 100, 
                       digits = 2)

# Print - Prediction Accuracy value of xgboost prediction 
paste0("Prediction Accuracy = ", pred_accuracy, "%")



# Visualization of Confusion Matrix

p_confusion_matrix <- as_tibble(final_penguins_data) %>% count(species, 
                                         pred_species, 
                                         sort = TRUE) %>% 
  mutate(recon = ifelse(species == pred_species, 
                        "matched", 
                        "not matched")) %>% 
  arrange(recon, species, pred_species) %>% 
  ggplot() +
  geom_point(mapping = aes(x = species, 
                           y = pred_species, 
                           color = recon), 
             show.legend = FALSE, 
             size = 10) + 
  geom_text(mapping = aes(x = species, 
                          y = pred_species, 
                          label = n), 
            show.legend = FALSE, 
            size = 4, 
            color = "black", 
            fontface = "bold") + 
  scale_color_brewer(palette = "Accent") + 
  theme(axis.title = element_text(face = "bold"), 
        plot.subtitle = element_text(face = "bold"), 
        plot.title = element_text(face = "bold")) + 
  labs(x = "Actual Species", 
       y = "Predicted Species", 
       title = "XGBoost - Multi-Class Prediction", 
       subtitle = "Confusion Matrix")

p_confusion_matrix




