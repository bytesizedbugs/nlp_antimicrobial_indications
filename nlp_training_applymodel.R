source("nlp_training_setup.R")

# Read the models back into R
all_models <- readRDS("freetext_nlp_models.rds")

# Run the models

#convert to matrices
dtm_train_mat <- as.matrix(dtm_df_subset) #train
dtm_test_mat <- as.matrix(dtm_test) #test

#all the trained models will have the same feature names - extract feature names from the first model
model_feature_names <- all_models[[1]][["finalModel"]][["feature_names"]]

#dtm_test_mat may be missing columns that are present as feature names in the model
missing_columns <- setdiff(model_feature_names, colnames(dtm_test_mat))

# Add missing columns with default values (0)
for (col in missing_columns) {
  dtm_test_mat <- cbind(dtm_test_mat, 0)
  colnames(dtm_test_mat)[ncol(dtm_test_mat)] <- col
}

# Ensure only valid column names are used and maintain the order
dtm_test_mat_subset <- dtm_test_mat[, model_feature_names, drop = FALSE]

colnames(dtm_test_mat_subset)

#can check that the contents of each model matches the column names in dtm_test_mat_subset
names(all_models)
#e.g.
all_models[["class_no_info"]][["finalModel"]][["feature_names"]]

#Generate predictions using the trained model

#nlpsensspec <- data.frame(Model = character(), Sensitivity = numeric(), Specificity = numeric(), stringsAsFactors = FALSE)

#if the feature names stored in object and newdata are different check the feature names in the model
#all_models[["class_no_info"]][["finalModel"]][["feature_names"]]
#colnames(dtm_test_mat_subset)


# Create an empty data frame to store the original text (from d_test$INDICATION_spellcheck), predictions, and actual results
predictions_df <- data.frame(INDICATION = d_test$INDICATION_spellcheck, stringsAsFactors = FALSE)

# Create an empty data frame to store the results
nlpsensspec <- data.frame(Model = character(), Sensitivity = numeric(), Specificity = numeric(), AUC = numeric(), Lower_CI = numeric(), Upper_CI = numeric(), stringsAsFactors = FALSE)

# Create an empty list to store misclassified cases for each model
misclassified_cases <- list()

for (model_name in names(all_models)) {
  # Extract the corresponding model from the 'all_models' list
  current_model <- all_models[[model_name]]
  
  # Apply the model and get probabilities
  current_probabilities <- predict(current_model, newdata = dtm_test_mat_subset, type = "prob")
  
  # Assuming the positive class is 'X1', extract probabilities for 'X1'
  current_prob_X1 <- current_probabilities[, "X1"]
  
  # Store the probabilities in the predictions dataframe
  predictions_df[[paste0("prob.", model_name)]] <- current_prob_X1
  
  # Predict class labels from probabilities
  current_predictions <- ifelse(current_prob_X1 > 0.5, "X1", "X0")
  
  # Store the binary predictions in the predictions dataframe
  predictions_df[[paste0("pred.", model_name)]] <- current_predictions
  
  # Convert actual values to character to match predictions format
  actual_values_raw <- as.character(unlist(d_test[, model_name]))
  actual_values_raw[actual_values_raw == "1"] <- "X1"
  actual_values_raw[actual_values_raw == "0"] <- "X0"
  
  # Store the actual values (true labels) in the predictions dataframe
  predictions_df[[paste0("actual.", model_name)]] <- actual_values_raw
  
  prediction_values_raw <- as.character(current_predictions)
  
  # Ensure predictions and actual values have the same levels
  common_levels <- c("X1", "X0")
  actual_values <- factor(actual_values_raw, levels = common_levels)
  predictions <- factor(prediction_values_raw, levels = common_levels)
  
  # Evaluate the predictions for the current model
  current_confusion_matrix <- caret::confusionMatrix(predictions, actual_values)
  
  # Extract sensitivity and specificity
  current_sensitivity <- current_confusion_matrix$byClass["Sensitivity"]
  current_specificity <- current_confusion_matrix$byClass["Specificity"]
  
  # Generate ROC curve and calculate AUC
  roc_curve <- roc(actual_values, current_prob_X1, levels = rev(levels(actual_values)))
  auc_value <- auc(roc_curve)
  
  # Calculate the confidence interval for the AUC
  ci_auc <- ci.auc(roc_curve)
  lower_ci <- ci_auc[1]
  upper_ci <- ci_auc[3]
  
  # Store the results in the results dataframe
  nlpsensspec <- rbind(nlpsensspec, data.frame(Model = model_name, Sensitivity = current_sensitivity, Specificity = current_specificity, AUC = auc_value, Lower_CI = lower_ci, Upper_CI = upper_ci, stringsAsFactors = FALSE))
  
  # Identify misclassified cases
  misclassified <- which(predictions != actual_values)
  misclassified_cases[[model_name]] <- misclassified
  
  # Print the confusion matrix for the current model
  cat("Confusion matrix for", model_name, ":\n")
  print(current_confusion_matrix)
  cat("\n")
  
  # Print the misclassified cases
  cat("Misclassified cases for", model_name, ":\n")
  print(misclassified)
  cat("\n")
  
  # Print the AUC value and its confidence interval
  cat("AUC for", model_name, ":", auc_value, "\n")
  cat("95% CI for AUC:", lower_ci, "-", upper_ci, "\n\n")
}

# Print the results dataframe
print(nlpsensspec)

# Print the misclassified cases for each model
for (model_name in names(misclassified_cases)) {
  cat("Misclassified cases for", model_name, ":\n")
  print(misclassified_cases[[model_name]])
  cat("\n")
}

# Print the dataframe with original text, predictions, and actual values
print(predictions_df)



###
#Calculate simple average of AUC

#keep only models of interest
models_to_exclude <- c("class_bacie", "class_fungal", "class_namedfunviral")

# Filter out rows for models not of interest
filtered_nlpsensspec <- nlpsensspec[!nlpsensspec$Model %in% models_to_exclude, ]

#average of ROCs of models to be used
average_auc <- mean(filtered_nlpsensspec$AUC)




# Visualise trees and importance matrix

# Can be run independently by loading in the saved model
# Uses the following packages
library(caret)
library(xgboost)
library(DiagrammeR)
library(ggplot2)

test_model  <- readRDS("freetext_nlp_models.rds")

#Extract model from caret
test_model_1 <-test_model[[1]]
final_test_model_1 <- test_model_1$finalModel
#xgb.plot.tree(model = final_test_model_1, n_first_trees = 1)
xgb.plot.tree(model = final_test_model_1, n_first_trees = 1, trees = 0)


test_model_9 <-test_model[[9]]
final_test_model_9 <- test_model_9$finalModel
#xgb.plot.tree(model = final_test_model_1, n_first_trees = 1)
xgb.plot.tree(model = final_test_model_9, n_first_trees = 1, trees = 1:5)


importance_matrix <- xgb.importance(model = final_test_model_9)

#This will only show items that were present in the DTM and therefore in the model
# e.g. stone was omitted and doesn't feature in importance matrix for urine


#filter to explore misclassified cases e.g. for bacteraemia/line infections

bacline_missclass <- predictions_df %>%
  filter(pred.class_bacline == 'X0' & actual.class_bacline == 'X1') %>%
  select(INDICATION, pred.class_bacline, actual.class_bacline)

bacmodel <- test_model[["class_bacline"]]
final_bacmodel <- bacmodel$finalModel
xgb.plot.tree(model = final_bacmodel, n_first_trees = 1, trees = 0)
xgb.plot.tree(model = final_bacmodel, n_first_trees = 1, trees = 1)


#for DFI
dfi_missclass <- predictions_df %>%
  filter(pred.class_dfi == 'X1' & actual.class_dfi == 'X0') %>%
  select(INDICATION, pred.class_dfi, actual.class_dfi)


TP <- sum(predictions_df$pred.class_dfi == 'X1' & predictions_df$actual.class_dfi == 'X1')

# True Negatives (TN): Both prediction and actual are 'X0'
TN <- sum(predictions_df$pred.class_dfi == 'X0' & predictions_df$actual.class_dfi == 'X0')

# False Positives (FP): Prediction is 'X1', but actual is 'X0'
FP <- sum(predictions_df$pred.class_dfi == 'X1' & predictions_df$actual.class_dfi == 'X0')

# False Negatives (FN): Prediction is 'X0', but actual is 'X1'
FN <- sum(predictions_df$pred.class_dfi == 'X0' & predictions_df$actual.class_dfi == 'X1')


# Calculate Positive Predictive Value (PPV)
PPV <- TP / (TP + FP)

# Calculate Negative Predictive Value (NPV)
NPV <- TN / (TN + FN)