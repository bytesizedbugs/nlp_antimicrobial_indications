source("nlp_training_setup.R")

# Train model #################################################
ctrl <- trainControl(method = "cv",
                     number = 5,
                     search = "random",
                     verboseIter = TRUE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     allowParallel = TRUE,
                     sampling = "smote")  # Use SMOTE for class imbalance

# Initialize an empty list to store models
all_models <- list()


#need to convert the 1, 0 to X0, X1
class_columns <- grep("^class_", names(d_train), value = TRUE)

# Apply make.names() to contents of selected columns
d_train[class_columns] <- lapply(d_train[class_columns], make.names)

# Use the paste0 function to create a character vector 'topics'

#run from here when running batched models
all_topics <- names(d_train)[22:24] #full list is 5:24, change as needed to batch them

# Loop over topics
for (topic in all_topics) {
  # Prepare the target variable
  t <- factor(unlist(d_train[, topic]), levels = c("X1", "X0"))
  
  # Train the model with SMOTE
  model <- caret::train(dtm_df_subset, t, method = "xgbTree",
                        trControl = ctrl)
  
  # Store the model in the list
  all_models[[topic]] <- model
}


# saving and loading models
# Save the models to a file
#saveRDS(all_models, file = "freetext_nlp_models.rds")

