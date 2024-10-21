############################################################
# Project: Categorising free text antibiotic indications using natural language processing in R
# Script: github_nlp_indications_validation.R
# Description: This script takes a dataframe of free-text antibiotic indications from an inpatient 
#    Electronic Prescribing and Medicines Administration (EPMA) system which have been manually 
#    labelled as belonging to one or more categories (e.g. a prescription for a chest or intra-abdominal infection),
#    pre-processes the text and produces a Document-Term-Matrix. 
#    A series of pre-trainedXgboost models are applied to the dataset to predict whether the free text indication 
#    belongs to a given category and the discrimination of the models is calculated.
# Author: Dr Alexander Martin
# Date: 2024-10-16
############################################################


# Data structure: d_new
# INDICATION: A free-text field containing the prescriber's free text indication for the antimicrobial prescription
# class_no_info: Binary (1 or 0) - No useful information is provided in the prescription.
# class_proph: Binary (1 or 0) - Prophylactic antimicrobial.
# class_sepsisnonneut: Binary (1 or 0) - Sepsis without neutropenia.
# class_neut: Binary (1 or 0) - Neutropenic sepsis.
# class_entmaxop: Binary (1 or 0) - ENT or maxillofacial infection.
# class_resp: Binary (1 or 0) - Respiratory tract infection.
# class_abdo: Binary (1 or 0) - Intra-abdominal infection.
# class_urin: Binary (1 or 0) - Urinary tract infection.
# class_skin: Binary (1 or 0) - Skin or soft tissue infection, excluding diabetic foot infection.
# class_cns: Binary (1 or 0) - Central nervous system infection.
# class_osteovasc: Binary (1 or 0) - A combined category of bone/joint or vascular infection.
# class_dfi: Binary (1 or 0) - Diabetic foot infection.
# class_perin: Binary (1 or 0) - Maternity/perinatal infection.
# class_fungal: Binary (1 or 0) - Fungal infection.
# class_named_viral_fungal: Binary (1 or 0) - Prescription references a named bacterial, viral or fungal pathogen.
# class_genit: Binary (1 or 0) - Genitourinary infection.
# class_ie: Binary (1 or 0) - Infective endocarditis.
# class_bacline: Binary (1 or 0) - Bacteraemia or line infection.



############################################################
# Load necessary libraries
############################################################
# This section loads  the required libraries for the analysis.

required_packages <- c("readxl", "stringdist", "tm", "caret",  "pROC")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readxl) #reads excel
library(stringdist) #spell check string distance function
library(tm) #text mining functions
library(caret) #model package
library(pROC) #ROC curves

#other packages used in model development
# library(stringr)
# library(plyr)
# library(dplyr)
# library(tidytext)
# library(xgboost)
# library(rpart.plot)
# library(tictoc)
# library(NLP)
# library(DiagrammeR)
# library(ggplot2)


############################################################
#Load the dataset to be tested as d_new
############################################################

d_new <- read_excel("jan24_internalvalidation.xlsx")

############################################################
#read in the models
############################################################
all_models <- readRDS("freetext_nlp_models.rds")

############################################################
# Preprocess the new dataset 
############################################################

# Substitute medical abbreviation symbol '#' with word 'fracture'
d_new$INDICATION <- gsub("#", "fracture", d_new$INDICATION)

# Remove other punctuation/symbols
d_new$INDICATION <- gsub("[^a-zA-Z]+", " ", d_new$INDICATION)

# Convert all strings to lower case
d_new$INDICATION <- tolower(d_new$INDICATION)

# Spellcheck for commonly misspelled medical words.
# A package would be more robust but this basic string distance approach allows custom medical words e.g. tazocin.
# This basic approach and small dictionary can cause short words to be improperly changed e.g. access being changed to abscess. 
# This is mitigated in part by including both words.

common_misspelled <- data.frame(word = c("infection", "infective", "infectious", "insertion", "sepsis", "septic", "urosepsis", "septicaemia", "stasis", "tonsillitis",
                                         "abscess", "access", "assess", "cellulitis",
                                         "neutropenic", "neutropenia", 
                                         "cholecystitis", "cholangitis", "cholecystectomy", "cholelithiasis", "choledocholithiasis", "biliary",
                                         "abdominal", "appendicitis", "diverticulitis", "peritonitis", "intraabdominal", "oesophageal", "pyelonephritis",
                                         "pyrexial", "pyrexia", "inflammatory",
                                         "bacteraemia", "exacerbation", "diabetic", "diabetes",
                                         "pseudomonas", "candidiasis", "candida", "influenza", "tazocin",
                                         "meningitis", "meningoencephalitis", "encephalitis",
                                         "prophylactic", "prophylaxis",
                                         "pneumonia", "empyema", "bronchiectasis",
                                         "osteomyelitis", "endocarditis", "hickmann"))

#Define the spellcheck function
spellcheck_word <- function(word, dictionary) { 
  distances <- stringdist(word, dictionary)
  if (any(is.na(distances))) {
    return(word)  # Return the original word if there are missing values in distances
  } else {
    min_distance <- min(distances)
    if (min_distance <= 2) { # Set a threshold for what is considered a close match
      closest_match <- dictionary[which.min(distances)]
      return(closest_match)
    } else {
      return(word) # Return the original word if no close match is found
    }
  }
}

spellcheck_string <- function(text, dictionary) {
  words <- unlist(strsplit(text, " "))
  corrected_words <- sapply(words, spellcheck_word, dictionary = dictionary)
  corrected_words <- na.omit(corrected_words) # Remove NA values
  corrected_text <- paste(corrected_words, collapse = " ")
  return(corrected_text)
}

#Apply the spellcheck function
d_new$INDICATION <- sapply(d_new$INDICATION, spellcheck_string, dictionary = common_misspelled$word)

# Create a Document-Term Matrix for the test data
corpus_new <- Corpus(VectorSource(d_new$INDICATION))
corpus_new <- tm_map(corpus_new, removeWords, stopwords("en"))
corpus_new <- tm_map(corpus_new, stemDocument)
dtm_new <- DocumentTermMatrix(corpus_new)

#convert to matrix
dtm_new_mat <- as.matrix(dtm_new) #test

#all the trained models have the same feature names - extract feature names from the first model
model_feature_names <- all_models[[1]][["finalModel"]][["feature_names"]]

#dtm_test_mat may be missing columns that are present as feature names in the model, these need to be added as blanks
missing_columns <- setdiff(model_feature_names, colnames(dtm_new_mat))
for (col in missing_columns) {
  dtm_new_mat <- cbind(dtm_new_mat, 0)
  colnames(dtm_new_mat)[ncol(dtm_new_mat)] <- col
}

# Ensure only valid column names are used and maintain the order
dtm_new_mat_subset <- dtm_new_mat[, model_feature_names, drop = FALSE]

############################################################
#Test the model on the new dataset
############################################################

# Dataframe for original strings(from d_new$INDICATION, plus predictions and true values
predictions_df_new <- data.frame(INDICATION = d_new$INDICATION, stringsAsFactors = FALSE)

# Empty data frame to store the results
nlpsensspec_new <- data.frame(Model = character(), Sensitivity = numeric(), Specificity = numeric(), AUC = numeric(), Lower_CI = numeric(), Upper_CI = numeric(), stringsAsFactors = FALSE)

# Create an empty list to store misclassified cases for each model
misclassified_cases_new <- list()

selected_models <- c("class_no_info", "class_proph", "class_sepsisnonneut", "class_neut", 
                     "class_entmaxop", "class_resp", "class_abdo", "class_urin", 
                     "class_skin", "class_cns", "class_osteovasc", "class_dfi", 
                     "class_perin", "class_fungal", "class_named_viral_fungal", 
                     "class_genit", "class_ie", "class_bacline")


# Loop over only the selected models
for (model_name in selected_models) {
  # Extract the model from the 'all_models' list
  current_model <- all_models[[model_name]]
  
  # Apply the model and get probabilities
  current_probabilities <- predict(current_model, newdata = dtm_new_mat_subset, type = "prob")
  
  # Extract probabilities for 'X1'
  current_prob_X1 <- current_probabilities[, "X1"]
  
  # Store the probabilities in the predictions dataframe
  predictions_df_new[[paste0("prob.", model_name)]] <- current_prob_X1
  
  # Predict class labels from probabilities, threshold set as >0.5
  current_predictions <- ifelse(current_prob_X1 > 0.5, "X1", "X0")
  
  # Store the predictions
  predictions_df_new[[paste0("pred.", model_name)]] <- current_predictions
  
  # Predictions use X1/X0, convert to this format
  actual_values <- as.character(unlist(d_new[, model_name]))
  actual_values[actual_values == "1"] <- "X1"
  actual_values[actual_values == "0"] <- "X0"
  
  # Store the actual values in the predictions dataframe
  predictions_df_new[[paste0("actual.", model_name)]] <- actual_values
  
  prediction_values_raw <- as.character(current_predictions)
  
  # Ensure predictions and actual values have the same levels
  common_levels <- c("X1", "X0")
  actual_values <- factor(actual_values, levels = common_levels)
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
  nlpsensspec_new <- rbind(nlpsensspec_new, data.frame(Model = model_name, Sensitivity = current_sensitivity, Specificity = current_specificity, AUC = auc_value, Lower_CI = lower_ci, Upper_CI = upper_ci, stringsAsFactors = FALSE))
  
  # Print the confusion matrix for the current model
  cat("Confusion matrix for", model_name, ":\n")
  print(current_confusion_matrix)
  cat("\n")
  

  # Print the AUC value and its confidence interval
  cat("AUC for", model_name, ":", auc_value, "\n")
  cat("95% CI for AUC:", lower_ci, "-", upper_ci, "\n\n")
}

# Print the results dataframe
print(nlpsensspec_new)

# Print the dataframe with original text, predictions, and actual values
print(predictions_df_new)







# To investigate misclassified cases, substitute example "genit" with the model of interest 
colname <- "genit"

# Construct the column names using the defined base name
pred_col <- paste0("pred.class_", colname)
actual_col <- paste0("actual.class_", colname)

# Filter the rows where the predicted class doesn't match the actual class
mismatch_df <- predictions_df_new[
  predictions_df_new[[pred_col]] != predictions_df_new[[actual_col]], 
]

# Select the relevant columns: INDICATION, pred.class_genit, actual.class_genit
mismatch_df_subset <- mismatch_df[, c("INDICATION", pred_col, actual_col)]

# Display the result
mismatch_df_subset