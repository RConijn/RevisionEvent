#------------------------------------------------------------------------------#
#                                                                              #
# Code for running ml models on an unseen dataset                              #
#                                                                              #
# Pre-requisites:                                                              #
#    - The extended inputlog log data: all_logs_extended.csv from              #
#      rule_based-inputlog.R                                                   #
#    - metadata about the log data: all_summary.csv from load_data_inputlog.R  #
#    - best performing model from predict_revision_start.R                     #
#      (decision tree, hard-coded below)                                       #
#    - best performing model from predict_revision_end.R                       #
#      with eye data: models/rf_eyekey_10cv.rds                                #
#      without eye data: models/rf_key_10cv.rds                                #
#                                                                              #
# For a full example with visualizations, see case_study.R                     #
#                                                                              #
#------------------------------------------------------------------------------#
# DISCLAIMER: The models are all trained and tested on the dataset described   # 
#             in the manuscript. The accuracy for the revision start is 99.6%  #
#             The accuracy for the revision end is 71% (83% for one character  #
#             off). The performance might be worse on datasets that share      #
#             little properties with the dataset used for training. Hence,     #
#             we suggest the researcher to take caution when interpreting      #
#             the results and, where necessary, perform some manual checks.    #
#                                                                              #
#------------------------------------------------------------------------------#

#load R packages
library(tidyverse)
library(rjson)
library(caret)
library(stringdist)
library(stringr)
options(scipen = 1000)

#load data
logs <- read.csv("data/all_logs_extended.csv", row.names = NULL)
summary <- read.csv("data/all_summary.csv")


################ Run 'true revision' model #####################################
logs_add <- logs %>%
  mutate(revisionstart = ifelse(
    #1. The writer starts deleting OR
    (removed_chars != "" & lag(revision_no) != revision_no) |
      #2. the writer moves the cursor to a different location in the text, 
      #except for the leading edge (excluding invisible characters), 
      #and then begins producing new characters
      (revision_no != 0 & lag(revision_no) != revision_no &
         !leadingedge), 1, 0)
  ) %>%
  left_join(select(summary, logguid, text.language, task))

logs_rev <- logs_add %>%
  filter(revision_no != 0) %>%
  group_by(logguid, text.language, task, revision_no) %>%
  summarize(revisionstart = first(revisionstart), 
            ins = first(insertion),
            del = first(deletion))

#summary table:
nrow(logs_rev) ## number of approximated revision events
sum(logs_rev$revisionstart == 1) ## number of true revision events.

# summarize revision events per condition: task and/or language
logs_rev_sum <- logs_rev %>%
  group_by(logguid, text.language, task) %>%
  summarize(nof_revision = sum(revisionstart == 1)) %>%
  group_by(task, text.language) %>%
  summarize(
    mean_rev = mean(nof_revision),
    sd_rev = sd(nof_revision)
  )


################ Data pre-processing revision end ##############################

# add info
logs_rev_add <- logs_add %>%
  mutate(
    is_delete = deletion,
    n_del_act = sum(is_delete),
    lagby = (rev_char_no*2) - 1,
    is_capital = as.numeric(output %in% LETTERS),
    is_space = as.numeric(output %in% c("SPACE", "TAB", "RETURN")),
    is_character = as.numeric(output %in% c(LETTERS, letters)),
    is_periodcomma = as.numeric(output %in% c(",", "."))
  ) %>%
  group_by(logguid, revision_no) %>%
  mutate(n_del = str_count(removed_chars),
         n_del_words = str_count(removed_chars, "\\S+"),
         n_del_sentences = str_count(removed_chars, "\\. ") +1,
         n_ins = str_count(typed_text),
         n_ins_words = str_count(typed_text, "\\S+"),
         n_ins_sentences = str_count(typed_text, "\\. ") +1) %>% 
  group_by(logguid) %>%
  mutate(
         ins_sofar =  substring(typed_text, 1, prod_char_no),
         dist_ins_del = stringdist(removed_chars, ins_sofar),
         stringdist_improved = ifelse(row_number() != 1, 
                                      dist_ins_del - lag(dist_ins_del), 0),
         n_ins_sofar = str_count(ins_sofar),
         n_ins_sofar = ifelse(is.na(n_ins_sofar), 0, n_ins_sofar),
         n_ins_words_sofar = str_count(ins_sofar, "\\S+"),
         typed_is_deleted_chars = as.numeric(n_del == n_ins_sofar),
         typed_is_deleted = as.numeric(removed_chars == ins_sofar),
         rev_no = revision_no,
         from_edge_chars = doclengthFull - positionFull,
         from_edge_words = floor((doclengthFull - positionFull)/5)+1,
         from_edge_sentences = floor((doclengthFull - positionFull)/80)+1,
         offset_start_final = doclengthFull,
         offset_start = charProduction,
         iki = pauseTime,
         time_start = startTime, 
         start_iki = ifelse(revisionstart == 1 | row_number() == 1,
                            pauseTime, NA),
         duration = cumsum(pauseTime),
         n_backspaces = ifelse(rev_char_no > 0, cumsum(output == "BACK"), NA),
         no_change = as.numeric(typed_text == removed_chars),
         immediate = as.numeric(positionFull == lag(positionFull) +1)
  ) %>%
  group_by(logguid) %>%
  fill(start_iki, n_backspaces, n_del)


## add revision starts with info
firstkey <- logs_add %>%
  filter(revision_no != 1, rev_char_no == 1)  %>%
  #add_info
  mutate(starts_with_capital = as.numeric(substr(typed_text, 0, 1) %in% LETTERS),
         starts_with_space = as.numeric(substr(typed_text, 0, 1) == " "),
         starts_with_periodcomma = as.numeric(substr(typed_text, 0,1) %in% c(",", "."))) %>%
  select(logguid, revision_no, starts_with_capital, starts_with_space,
         starts_with_periodcomma)

# replace NA values
logs_rev_add2 <- left_join(logs_rev_add,firstkey) %>%
  group_by(logguid)  %>%
  mutate(starts_with_periodcomma = ifelse(is.na(starts_with_periodcomma), 0, 
                                          starts_with_periodcomma),
         starts_with_space = ifelse(is.na(starts_with_space), 0, 
                                    starts_with_space),
         starts_with_capital = ifelse(is.na(starts_with_capital), 0, 
                                      starts_with_capital)) %>%
  map_if(is.numeric,~ifelse(is.na(.x),0,.x)) %>%
  as.data.frame()


# write final set
write.csv(logs_rev_add2, "data/logs_rev_add.csv")


################ Run revision end model ########################################
#load best model (Random Forest) - keystroke data only
bestmodel <- readRDS("models/rf_key_10cv_s.rds")

test_pred <- predict(bestmodel, logs_rev_add2, type = "prob") 

test3 <- logs_rev_add2 %>%
  cbind(test_pred) %>%
  group_by(logguid, text.language, task, rev_no) %>%
  summarize(maxyes = max(yes))

# select the keystroke with the highest class probability
test4 <- logs_rev_add2 %>%
  cbind(test_pred) %>%
  left_join(test3) %>%
  group_by(logguid, text.language, task, rev_no) %>%
  mutate(
    predicted_Y = factor(ifelse(yes == maxyes, "yes", "no"),
                         levels = c("yes", "no")))

# summarize length of revisions
sum_revisions <- test4 %>%
  group_by(logguid, text.language, task, rev_no) %>%
  summarize(
    chars_rev_pred = first(rev_char_no[predicted_Y == "yes"])
  ) 

revisions_add <- test4 %>%
  left_join(sum_revisions) %>%
  mutate(revision_end = (chars_rev_pred == rev_char_no & rev_char_no !=0)) %>%
  select(logguid, text.language, task, id, rev_no, revision_end, removed_chars,
         ins_sofar)

# write final set
write.csv(revisions_add, "data/revisions_ml.csv")
