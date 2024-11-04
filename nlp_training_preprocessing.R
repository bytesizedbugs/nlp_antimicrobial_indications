source("nlp_training_setup.R")

#Read in labelled data

#data stored as xlsx as some strings start with a character that causes a #NAME? error when saved in csv format e.g. '+' for 'positive'
d_labelled <- read_excel("indications_nlp.xlsx")

#Pre-processing for NLP

# substitute medical abbreviation symbol '#' with word 'fracture'
d_labelled$INDICATION <- gsub("#", "fracture", d_labelled$INDICATION)

# remove other punctuation/symbols
d_labelled$INDICATION <- gsub("[^a-zA-Z]+", " ", d_labelled$INDICATION)

# convert all strings to lower case
d_labelled$INDICATION <- tolower(d_labelled$INDICATION)

# Spellcheck for commonly misspelled medical words.
# A package would be more robust but this basic string distance approach allows custom medical words e.g. tazocin.
# This basic approach and small dictionary can cause short words to be improperly changed e.g.
# access being changed to abscess. This is mitigated in part by including both words.

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


# Function to check the spelling of a word and return the closest match from a dictionary

# Comparing words to a dictionary. If a dictionary word exists within an edit distance of 2, the function returns that word as the corrected spelling. 
# If no close match is found or if there's a distance calculation error, it simply returns the original word.

# Define the function
spellcheck_word <- function(word, dictionary) { 
  # Calculate the string distance between each word and the words in the dictionary
  distances <- stringdist(word, dictionary)
  
  # Check for any NA values in distances, indicating a problem with the distance calculation
  # If any NA values are found, return the original word as-is
  if (any(is.na(distances))) {
    return(word)  # Return the original word if there are missing values in distances
  }
  
  # Find the minimum distance in the distances vector
  min_distance <- min(distances)
  
  # Check if the closest match is within an acceptable edit distance (threshold of 2)
  if (min_distance <= 2) { 
    # If the minimum distance is within the threshold, identify the closest match
    closest_match <- dictionary[which.min(distances)]
    return(closest_match)  # Return the closest match as the corrected spelling
  } else {
    # If no close match is found within the threshold, return the original word
    return(word)
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
d_labelled$INDICATION_spellcheck <- sapply(d_labelled$INDICATION, spellcheck_string, dictionary = common_misspelled$word)

#Troubleshoot
#differing_rows <- d_labelled[d_labelled$INDICATION != d_labelled$INDICATION_spellcheck, c("INDICATION", "INDICATION_spellcheck")]

#Split data into training (0.7) and testing (0.3) subsets
set.seed(2021)
sample2 <- sample(1:nrow(d_labelled), round(0.7 * nrow(d_labelled), 0), F) #standard split should be 0.7
d_train <- d_labelled[sample2,]
d_test <- d_labelled[-sample2,]


#######
#Preprocess the training dataset

# Preprocess the training Corpus

#Define a corpus (tm package)
corpus <- VCorpus(VectorSource(d_train$INDICATION_spellcheck))

#remove numbers
corpus <- tm_map(corpus, removeNumbers)

#remove stopwords (e.g. articles, prepositions)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
#stem words (reduce words to their root)
corpus <- tm_map(corpus, stemDocument)

# Create a Document-Term Matrix for the training dataset
dtm <- DocumentTermMatrix(corpus)

# Convert DTM to a data frame
dtm_df <- as.data.frame(as.matrix(dtm))


# Preprocess the test corpus
# Create a Document-Term Matrix for the test data
corpus_test <- Corpus(VectorSource(d_test$INDICATION))
corpus_test <- tm_map(corpus_test, removeNumbers)
corpus_test <- tm_map(corpus_test, removeWords, stopwords("en"))
corpus_test <- tm_map(corpus_test, stemDocument)

dtm_test <- DocumentTermMatrix(corpus_test)



# The resultant training DTM is very large and sparse with 3301 columns, this results in slow training
# To address this I defined terms of interest based on those which appeared frequently or were high yield for each category
# This is subjective and can leave out important terms - e.g. in this version "stone" is missing
# A more optimised training model might allow more unselected terms to be used in training in reasonable timeframe

# Define the terms of interest (all lower case, needs to match DTM) - key items and differentials

terms <- c(
  "infect", "sourc", "per", "micro", "sepsi", "dose", "patient", "oral", "weight", "advic", "unknown", "switch", "crp", "step", "day", "temp", 
  "abx", "rais", "spike", "stat", "marker", "origin", "kardex", "cultur", "stepdown", "post", "high", "abscess", "prophylaxi", "surgic", "pre", 
  "med", "scr", "lscs", "regular", "reg", "given", "cathet", "cath", "lab", "surgeri", "prophylact", "prevent", "chang", "uti", "antibiot", "ercp", 
  "oper", "long", "term", "medic", "tavi", "splenectomi", "urosepsi", "chest", "biliari", "abdomin", "intra", "septic", "intraabdomin", "suspect", 
  "cap", "gram", "urinari", "abdo", "cover", "like", "allerg", "caus", "unclear", "pen", "hap", "neutropen", "neutropenia", "febril", "neut", 
  "fever", "chemotherapi", "pgd", "haematolog", "allergi", "start", "chemo", "probabl", "protocol", "tazocin", "tonsil", "dental", "neck", "acut", 
  "quinsi", "otiti", "left", "ulcer", "tonsillectomi", "fractur", "mastoid", "peritonsillar", "urti", "cellul", "bleed", "mandibl", "media", 
  "parot", "right", "sinus", "sore", "mucos", "externa", "noe", "lrti", "pneumonia", "aspir", "iecopd", "copd", "acquir", "exacerb", "communiti", 
  "possibl", "asp", "empyema", "curb", "atyp", "consolid", "hospit", "asthma", "bronchiectasi", "urin", "cholecyst", "append", "diverticul", "cholang", 
  "perfor", "collect", "coliti", "sbp", "liver", "periton", "perf", "bowel", "perian", "appendix", "sigmoid", "leak", "pyelonephr", "coli", "recurr", 
  "sensit", "posit", "cauti", "ecoli", "associ", "lower", "klebsiella", "msu", "wound", "leg", "skin", "hand", "swab", "site", "bite", "facial", "foot", 
  "groin", "arm", "tissu", "staph", "soft", "dog", "sever", "aureus", "mening", "enceph", "viral", "meningoenceph", "cns", "bacteri", "csf", "brain", 
  "hsv", "epidur", "meningo", "spinal", "cerebellar", "cerebr", "current", "rule", "shunt", "open", "osteomyel", "knee", "arthriti", "joint", "disciti", 
  "finger", "metalwork", "pji", "hip", "graft", "bone", "thigh", "stump", "elbow", "bursiti", "flexor", "tibia", "diabet", "toe", "dfi", "gangren", 
  "necrot", "hallux", "heel", "necrosi", "opat", "sampl", "wet", "amput", "team", "gbs", "labour", "pyrexia", "pprom", "group", "strep", "pid", "srom", 
  "forcep", "intrapartum", "deliveri", "section", "prolong", "tear", "chorioamnion", "bmi", "degre", "srm", "postnat", "preterm", "elect", "hour", 
  "diff", "bacteraemia", "pseudomona", "blood", "influenza", "rod", "cocci", "negat", "flu", "orchiti", "epididymo", "ovarian", "scrotal", "penil", 
  "tubo", "genit", "epididymoorch", "epididym", "around", "cecal", "fat", "pole", "strand", "bodi", "epididymorch", "peni", "priapism", "balant", 
  "candida", "endocard", "valv", "prosthet", "pacemak", "enterococcus", "empir", "metal", "suspicion", "cours", "hickmann", "line", "now", "treat", 
  "vancomycin", "mssa", "advis", "yeast",
  "neuro", "neurosurgeri", "lumbar", "ventral", "ventricul", "cervic", "meningococc", "stimul", "cranioplasti",
  "olecranon", "spine", "sheath", "shoulder",  "thumb", "radial", "sacral",  "tkr", "thr", "metalwar", "revis", "evar", "periprosthet", "pin", "pinsit", "dair", "tendon")

#important to ensure there are no duplicates in the list

# Subset the dataframe by selecting columns that contain terms
dtm_df_subset <- dtm_df[, unlist(lapply(terms, function(term) {
  grep(paste0("^", term, "$"), names(dtm_df), value = TRUE)
}))]

########## to troubleshoot
selected_columns <- unlist(lapply(terms, function(term) {
  grep(paste0("^", term, "$"), names(dtm_df), value = TRUE)
}))
missing_terms <- setdiff(terms, selected_columns) #should return character(0)

