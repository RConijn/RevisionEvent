library(tidyverse)
library(rio)
library(caret)
library(stringdist)
library(stringr)
options(scipen = 1000)

# read data 
eye_key <- import("data/all-eyekeys.csv") 

#add_info
eye_key_add <- eye_key %>%
  mutate(
         text = ifelse(revision != 0 & text == "" &  is_delete == 0, " ", text),
         lagby = (rev_char_no*2) - 1,
         is_capital = as.numeric(text %in% LETTERS),
         is_space = as.numeric(text == " "),
         is_character = as.numeric(text %in% c(LETTERS, letters)),
         is_periodcomma = as.numeric(text %in% c(",", ".")),
         ) %>%
  group_by(token) %>%
  mutate(deleted_text = text[if_else(row_number() - lagby > 0 & is_delete == 1,
                                     row_number() - lagby, 
                                     as.numeric(NA))],
         typed_is_deleted = deleted_text[if_else(row_number() - n_del > 0 & n_del > 0,
                                         row_number() - n_del, 
                                         as.integer(NA))] == text,
         typed_is_deleted = ifelse(is.na(typed_is_deleted), 0,
                                   as.numeric(typed_is_deleted))) %>%
  filter(revision != 0) %>%
  group_by(token, rev_no) %>%
  mutate(del_sofar =  ifelse(!is.na(deleted_text),
                             Reduce(paste0, as.character(deleted_text), 
                             accumulate = TRUE), NA),
         ins_sofar = ifelse(!is.na(text), 
                            Reduce(paste0,as.character(text), 
                                   accumulate = TRUE), NA),
         dist_ins_del = stringdist(del, ins_sofar),
         stringdist_improved = ifelse(row_number() != 1, 
                                      dist_ins_del - lag(dist_ins_del), 0),
         n_ins_sofar = str_count(ins_sofar),
         n_ins_words_sofar = str_count(ins_sofar, "\\S+"),
         typed_is_deleted_chars = as.numeric(n_del == n_ins_sofar),
         # binary variables from eye data
         static = as.numeric(saccade_from == "stat"),
         progressive = as.numeric(saccade_from == "progr"),
         regressive = as.numeric(saccade_from == "regr")
         ) %>%
  ungroup() 

## add revision starts with info
firstkey <- eye_key %>%
  filter(rev_no != 1, rev_char_no == 1, !is.na(fileno))  %>%
  #add_info
  mutate(starts_with_capital = as.numeric(substr(ins, 0, 1) %in% LETTERS),
         starts_with_space = as.numeric(substr(ins, 0, 1) == " "),
         starts_with_periodcomma = as.numeric(substr(ins, 0,1) %in% c(",", "."))) %>%
  select(fileno, rev_no, starts_with_capital, starts_with_space,
         starts_with_periodcomma)

eye_key_add <- left_join(eye_key_add,firstkey)


## Feature selection
pred_end <- eye_key_add %>%
  #remove string vars / index vars
  select(-removed_chars, -typed_chars, -z, -text, -ins, -del, -ins_bef_end,
         -debug_from_edge, -cur_phase, -del_sofar, -ins_sofar, -revision,
         -deleted_text, -revision_end, -token, -t, -fix_idx,
         -saccade_from, -type) %>%
  #remove many missing vars
  select(-from_sentence_start,  -i_count) %>%
  # remove zero variance vars
  select(-just_edited) %>%
  # remove info over full revision
  select(-offset_end_final, -offset_end, -n_ins_bef_end, -offset, 
         -n_ins_words_bef_end,  -n_del_sentences_bef_end,  -lagby,
         # all predictors of revision start
         #-from_edge_chars, -from_edge_words, -from_edge_sentences,
         #-offset_start_final, -start_iki, -offset_start, -time_start, 
         #-n_del, -n_del_words, -n_ins_words, -immediate, 
         #-duration, -n_backspaces,  -n_ins, -n_del_sentences,
         #-n_ins_words, -no_change, 
         ) %>%
  mutate(revision_ended =  as.factor(ifelse(revision_ended, "yes", "no")))

# Summarize
summary(pred_end)
sum <- pred_end %>% 
  select(-revision_ended) %>%
  summarise_each(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))


# many NA values for eye-fixation data, set to zero
pred_end <- pred_end %>%
  mutate(no_eye_data = ifelse(is.na(fix_dur), 1, 0),
         offset_start_final = ifelse(is.na(offset_start_final), 0, 
                                     offset_start_final),
         fix_dur = ifelse(is.na(fix_dur), 0, fix_dur),
         saccade = ifelse(is.na(saccade), 0, saccade),
         leading_edge = ifelse(is.na(leading_edge), FALSE, leading_edge),
         static = ifelse(is.na(static), 0, static),
         progressive = ifelse(is.na(progressive), 0, progressive),
         regressive = ifelse(is.na(regressive), 0, regressive)
         )

# 70% of files in train 
trainlength <- round(length(unique(pred_end$fileno)) *.7)
trainfiles <- sample(unique(pred_end$fileno), trainlength, replace = F)

train <- pred_end %>%
  filter(fileno %in% trainfiles) %>%
  select(-fileno, -rev_char_no) 
  #remove eye data

trainnoeye <- train %>%
  select(-fix_dur, -saccade, -leading_edge, -static, -progressive, -regressive,
         -no_eye_data)

write.csv(train,"data/train_eyekey.csv")

# 30% test
test <- pred_end %>%
  filter(!fileno %in% trainfiles) 

write.csv(test,"data/test_eyekey.csv")


## MODELING
run_model <- function(data, modeltype){
  set.seed(107)
  ctrl <- trainControl(method = "cv", number = 10,
                       classProbs = T,
                       summaryFunction = prSummary,
                       returnResamp = "final")
  plsFit <- train(
    revision_ended ~ .,
    data = data,
    method = modeltype,
    preProc = c("center", "scale"),
    na.action = na.omit,
    trControl = ctrl,
    metric = "F",
    maximize = T
  )
}

# run three ML algorithms
resultsvm <- run_model(train, "svmRadial") 
# save model
saveRDS(resultsvm, file = "models/svm_eyekey_10cv.rds")

resultrf <- run_model(train, "rf")
saveRDS(resultrf, file = "models/rf_eyekey_10cv.rds")

resulttree <- run_model(train, "rpart")
saveRDS(resulttree, file = "models/tree_eyekey_10cv.rds")


get_results_table <- function(model, modeltype){
  # predict class probabilities for each keystroke
  test2 <- test[complete.cases(test), ]
  test_pred <- predict(model, test, type = "prob") 

  test3 <- test2 %>%
      cbind(test_pred) %>%
      group_by(fileno, rev_no) %>%
      summarize(maxyes = max(yes))

  # select the keystroke with the highest class probability
  test4 <- test2 %>%
      cbind(test_pred) %>%
      left_join(test3) %>%
      group_by(fileno, rev_no) %>%
      mutate(
        predicted_Y = factor(ifelse(yes == maxyes, "yes", "no"),
                         levels = c("yes", "no")),
        revision_ended = factor(revision_ended, levels = c("yes", "no")))

  # summarize agreements per revision
  sum_agree <- test4 %>%
      group_by(fileno, rev_no) %>%
      summarize(
        chars_rev = max(rev_char_no),
        chars_rev_obs = first(rev_char_no[revision_ended == "yes"]),
        chars_rev_pred = first(rev_char_no[predicted_Y == "yes"]),
        chars_away = chars_rev_obs - chars_rev_pred,
        true_positives = chars_away == 0,
        true_positives_one_off = (abs(chars_away) == 1 | chars_away == 0)
      )

  conf <- confusionMatrix(test4$predicted_Y, test4$revision_ended)
  results <- data.frame(
      trainmethod = modeltype,
      TN = conf$table[1,1],
      FN = conf$table[1,2],
      FP = conf$table[2,1],
      TP = conf$table[2,2]) %>%
    mutate(
      accuracy = (TP+TN)/(TP+TN+FP+FN),
      accuracy_0 = sum(sum_agree$true_positives)/nrow(sum_agree),
      accuracy_1 = sum(sum_agree$true_positives_one_off)/nrow(sum_agree),
      accuracy_charsaway_mean = mean(abs(sum_agree$chars_away)), 
      accuracy_charsaway_sd= sd(abs(sum_agree$chars_away)), 
      precision = TP/(TP+FP),
      recall = TP/(TP+FN),
      f1 = (2*precision*recall)/(precision+recall)
  )
}

# create outcome tables
result_svm <- get_results_table(resultsvm, "svmRadial") 
write.csv(result_svm, "models/svm_eyekey_10cv_results2.csv")
result_rf <- get_results_table(resultrf, "rf")
write.csv(result_rf, "models/rf_eyekey_10cv_results2.csv")
result_tree <- get_results_table(resulttree, "tree")
write.csv(result_tree, "models/tree_eyekey_10cv_results.csv")


## PREDICT BASELINE: Typed = deleted
test2 <- test[complete.cases(test), ]

# predict Y via typed is deleted
test4 <- test2 %>%
  group_by(fileno, rev_no) %>%
  mutate(
    predicted_Y = factor(ifelse(n_del == n_ins_sofar |row_number() == n(),
                                "yes", "no"),
                         levels = c("yes", "no")),
    revision_ended = factor(revision_ended, levels = c("yes", "no")))

# summarize agreements per revision
sum_agree <- test4 %>%
  group_by(fileno, rev_no) %>%
  summarize(
    chars_rev = max(rev_char_no),
    chars_rev_obs = first(rev_char_no[revision_ended == "yes"]),
    chars_rev_pred = first(rev_char_no[predicted_Y == "yes"]),
    chars_away = chars_rev_obs - chars_rev_pred,
    true_positives = chars_away == 0,
    true_positives_one_off = (abs(chars_away) == 1 | chars_away == 0)
  )

conf <- confusionMatrix(test4$predicted_Y, test4$revision_ended)
results <- data.frame(
  trainmethod = "baseline",
  TN = conf$table[1,1],
  FN = conf$table[1,2],
  FP = conf$table[2,1],
  TP = conf$table[2,2]) %>%
  mutate(
    accuracy = (TP+TN)/(TP+TN+FP+FN),
    accuracy_0 = sum(sum_agree$true_positives)/nrow(sum_agree),
    accuracy_1 = sum(sum_agree$true_positives_one_off)/nrow(sum_agree),
    accuracy_charsaway_mean = mean(abs(sum_agree$chars_away)), 
    accuracy_charsaway_sd= sd(abs(sum_agree$chars_away)), 
    precision = TP/(TP+FP),
    recall = TP/(TP+FN),
    f1 = (2*precision*recall)/(precision+recall)
  )














