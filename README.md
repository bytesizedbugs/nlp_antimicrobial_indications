# nlp_antimicrobial_indications

# Project Title
Natural language processing of antimicrobial indications

Electronic Prescribing and Medicines Administration (EPMA) systems hold large amounts of data. This data can be useful for antimicrobial stewardship, but only if it can be interpreted. Antimicrobial prescriptions with free text indications are a challenge due to the variety of ways a disease condition can be expressed, misspellings, and the use of acronyms. Natural language processing (NLP) is a machine learning methodology which can be used to convert this raw data into actionable knowledge and support antimicrobial stewardship.

This project aims to use R to apply NLP to analyse free-text antimicrobial prescriptions. The goal is to classify these prescriptions into predefined categories based on the suspected source of infection.


# To clone the repository:
```bash
git clone https://github.com/bytesizedbugs/nlp_antimicrobial_indications.git
```

# Files
nlp_sampledata is a sample dataset to illustrate how the manually labelled data look
This data structure is also described in nlp_training_main.R

The trained models are stored in a list (freetext_nlp_models.rds)

# Scripts
To run the models on the sample data:
Run nlp_example.R

To train models:
Ensure data is in the format described in nlp_training_main.R and stored in the working directory
Run nlp_training_main.R
This calls nlp_training_setup.R, nlp_training_preprocessing.R, (nlp_training_trainmodel.R), nlp_training_applymodel.R, and nlp_trainin_eval.R.
(nlp_training_trainmodel.R is by default commented out because it takes time to train new models. Instead the trained models can be loaded in.)



# Acknowledgements
Thank you to pharmacy staff at James Cook University Hospital.
