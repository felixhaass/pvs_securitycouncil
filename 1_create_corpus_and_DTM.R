
# Replication data for:

# INSERT TITLE

# Authors:

# Felix S. Bethke
# Felix Haass
# Julia Strasheim


# Goal of this script is to read in all .txt files of the UNSC resolutions
# and generate a document-term matrix for further analysis.


# Load libraries ----------------------------------------------------------

library(quanteda)
library(readtext)
library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)



# Data input --------------------------------------------------------------

# Important: This reads data *without* appendices 

qd_sc_corpus <- corpus(readtext::readtext(file = "./Data/Resolutions Textfiles Complete, 1946-2015 - Ohne Annex/*"))

docvars(qd_sc_corpus, "resid") <- as.numeric(str_extract(docnames(qd_sc_corpus), "(?<=RES-)(.*)(?=\\()"))
docvars(qd_sc_corpus, "year") <- as.numeric(str_extract(docnames(qd_sc_corpus), "(?<=\\()(.*)(?=\\))"))


# 2. Define a dictionary of UN terms --------------------------------------

# this is not strictly necessary for the replication of the PVS analayses,
# but allows for further analysis of other possibly relevant terms in 
# UN security council resolutions.

# Note that for an analysis of UN peacekeeping you might want to include
# all names of UN missions as separate dictionary entries.

# MYRESANALYSIS is the term we use for further analysis of responsibility
# upper case & strange name is necessary to avoid further stemming

sc_dict <- dictionary(list(secretary_general = c("Secretary General", "SecretaryGeneral", 
                                                 "Secretary-General", "secretary-general", 
                                                 "secretary general", "Secretary-General's"),
                           united_nations = c("United Nations", "united nations",
                                              "United Nations'"),
                           rule_of_law = c("rule of law", "Rule of Law",
                                           "Rule of Law's"),
                           security_council = c("Security Council", "security council",
                                                "Security Council's"),
                           peace_agreement = c("peace agreement"),
                           sovereignty = c("sovereignty", "sovereign", 
                                           "Sovereign", "Sovereign", "Sovereignty" ),
                           international_community = c("international community",
                                                       "international community's"),
                           humanitarian_law = c("humanitarian law"),
                           armed_conflict = c("armed conflict", "armed conflicts"),
                           african_union = c("african union",
                                             "African Union",
                                             "African Union's"),
                           displaced_persons = c("displaced persons", "IDP"),
                           south_sudan = c("south sudan", "South Sudan", "South Sudan's"),
                           DRC = c("Democratic Republic of the Congo", 
                                   "Democratic Republic of the Congo's", 
                                   "democratic republic of the congo"),
                           sexual_violence = c("sexual violence"),
                           arms_embargo = c("arms embargo",
                                            "arms embargos",
                                            "arms embargo's"),
                           CAR = c("Central African Republic", "Central African Republic's"),
                           Bosnia_and_Herzegovina = c("Bosnia and Herzegovina"),
                           SSR = c("Security Sector Reform", "security sector reform"),
                           human_rights = c("human rights", "Human Rights"),
                           MYRESANALYSIS =  c("responsible", 
                                              "responsibility", 
                                              "Responsibility")))

# create thesaurus from the dictionary which is later used to 
# substitute the terms in creating the document-term matrix

# create a copy of the original corpus, because now we change words in the corpus directly
qd_sc_corpus_dict <- tokens_compound(tokens(qd_sc_corpus), sc_dict)


# Create Document-Term-Matrix ---------------------------------------------

dfm_sc <- dfm(qd_sc_corpus_dict, 
              remove_numbers = T, 
              remove_punct = TRUE, 
              thesaurus = lapply(sc_dict, function(x) gsub("\\s", "_", x)),
              remove_separators = TRUE,
              stem = T, 
              remove = stopwords("english"))

# clean feature list

# clean feature list

# remove all session numbering
dfm_sc <- dfm_remove(dfm_sc, featnames(dfm_sc)[grepl("[0-9]th|[0-9]st|[0-9]nd|[0-9]rd", featnames(dfm_sc))])

# remove all www. references
dfm_sc <- dfm_remove(dfm_sc, featnames(dfm_sc)[grep("www.", featnames(dfm_sc))])

# remove all .html references
dfm_sc <- dfm_remove(dfm_sc, featnames(dfm_sc)[grep(".html", featnames(dfm_sc))])

# remove all punctuation terms
dfm_sc <- dfm_remove(dfm_sc, featnames(dfm_sc)[grepl("\\.", featnames(dfm_sc))])

# remove parts of the document symbol numbers
dfm_sc <- dfm_remove(dfm_sc, c("s", "res"))

# to work with the entire document feature matrix we save it as data frame
dfm_sc_df <- as.data.frame(dfm_weight(dfm_sc, "frequency"))

dfm_sc_df$year <- as.numeric(str_extract(docnames(qd_sc_corpus), "(?<=\\()(.*)(?=\\))")) 
dfm_sc_df$resolution <- str_extract(docnames(qd_sc_corpus), "(?<=RES-)(.*)(?=\\()")
dfm_sc_df$resolution <- as.numeric(dfm_sc_df$resolution)

# length of individual resolution
dfm_sc_df$reslength <- ntoken(dfm_sc)
# save data
save(dfm_sc, file = "./Data/dfm_sc.rdata")

