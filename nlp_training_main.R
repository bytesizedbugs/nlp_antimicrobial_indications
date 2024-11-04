############################################################
# Project: Categorising free text antibiotic indications using natural language processing in R
# Script: github_nlp_indications.R
# Description: This script takes a dataframe of free-text antibiotic indications from an inpatient 
#    Electronic Prescribing and Medicines Administration (EPMA) system which have been manually 
#    labelled as belonging to one or more categories (e.g. a prescription for a chest or intra-abdominal infection),
#    pre-processes the text and produces a Document-Term-Matrix. 
#    A series of models (Xgboost using SMOTE) are trained on 70% of the dataset using package Caret. 
#    The models categorise predict whether the free text indication belongs to a given category. 
#    Predictions are then applied to the holdout dataset (30%) to assess discrimination.
# Author: Dr Alexander Martin
# Date: 2024-10-16
############################################################

# Data structure: d_labelled.xlsx
# INDICATION: A free-text field containing the prescriber's free text indication for the antimicrobial prescription
# class_no_info: Binary (1 or 0) - No useful information is provided.
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


source("nlp_training_setup.R")
source("nlp_training_preprocessing.R")
#source("nlp_training_trainmodel.R") - only run if training model again
source("nlp_training_applymodel.R")
