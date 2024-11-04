source("nlp_training_setup.R")

###
#Calculate simple average of AUC of models of interest
# exclude unwanted models
models_to_exclude <- c("class_bacie", "class_fungal", "class_namedfunviral")

# Filter out rows for models not of interest
filtered_nlpsensspec <- nlpsensspec[!nlpsensspec$Model %in% models_to_exclude, ]

#average of ROCs of models
average_auc <- mean(filtered_nlpsensspec$AUC)



# Visualise trees
#Extract model of interest from caret
#Example
all_models_1 <-all_models[[1]]
final_all_models_1 <- all_models_1$finalModel
xgb.plot.tree(model = final_all_models_1, n_first_trees = 1, trees = 0)


all_models_9 <-all_models[[9]]
final_all_models_9 <- all_models_9$finalModel
xgb.plot.tree(model = final_all_models_9, n_first_trees = 1, trees = 1:5)

#Importance matrix
#This will only show items that were present in the DTM and therefore in the model
# e.g. stone was omitted and doesn't feature in importance matrix for urine
importance_matrix <- xgb.importance(model = final_all_models_9)


#Filter to explore misclassified cases e.g. for bacteraemia/line infections
bacline_missclass <- predictions_df %>%
  filter(pred.class_bacline == 'X0' & actual.class_bacline == 'X1') %>%
  select(INDICATION, pred.class_bacline, actual.class_bacline)

bacmodel <- all_models[["class_bacline"]]
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