library(tidyverse)
library(rio)
library(caret)
options(scipen = 1000)

# read data 
all_data <- import("data/all-keystrokes.csv") %>%
  # keep automatically identified info not related to revision end
  select(-c(revision_end:anot_keep), -n_ins_bef_end, -ins_bef_end,
         -n_ins_words_bef_end, -n_del_sentences_bef_end)

# predict start [revision]
all_data_short <- all_data %>%
  filter(rev_no != 1, rev_char_no == 1, !is.na(fileno))  %>%
  #add_info
  mutate(starts_with_capital = as.numeric(substr(ins, 0, 1) %in% LETTERS),
       starts_with_space = as.numeric(substr(ins, 0,1) == " "),
       starts_with_periodcomma = as.numeric(substr(ins, 0,1) %in% c(",", ".")))

# Feature selection
pred_start <- all_data_short %>%
  #remove string vars
  select(-removed_chars, -typed_chars, -z, -text, -ins, -del, 
         -debug_from_edge, -cur_phase, -type) %>%
  # remove many missing vars
  select(-from_sentence_start,  -i_count) %>%
  # remove zero variance vars
  select(-just_edited, -rev_char_no, -fileno) %>%
  # remove vars with redundant info
  select(-iki, -offset_end, -offset_end_final) %>%
  mutate(revision = ifelse(revision == 1, "yes", "no"))

# summary
summary(pred_start)
sum <- pred_start %>% 
  summarise_each(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))
table(pred_start$revision)
#479 non-revisions (6.7%), 6641 revisions (total 7120)


## MODELING
run_model <- function(data, modeltype){
  set.seed(107)
  ctrl <- trainControl(method = "cv", number = 10,
                                   classProbs = T,
                                   summaryFunction = prSummary,
                                   returnResamp = "final")
  plsFit <- train(
    revision ~ .,
    data = data,
    method = modeltype,
    #preProc = c("center", "scale"),
    na.action = na.omit,
    trControl = ctrl,
    metric = "F",
    maximize = T
  )
}

get_results_table <- function(model, modeltype){
  results <- model$results %>%
    filter(`F` == max(`F`)) %>%
    select(AUC:FSD) %>%
    mutate(trainmethod = modeltype,
           TN = confusionMatrix(model)$table[1,1],
           FN = confusionMatrix(model)$table[1,2],
           FP = confusionMatrix(model)$table[2,1],
           TP = confusionMatrix(model)$table[2,2],
           accuracy = TP+TN)
}

# run three ML algorithms
resultsvm <- run_model(pred_start, "svmRadial") 
result_svm <- get_results_table(resultsvm, "svm")

resultrf <- run_model(pred_start, "rf")
result_rf <- get_results_table(resultrf, "rf")

resulttree <- run_model(pred_start, "rpart")
result_tree <- get_results_table(resulttree, "tree")

# compare outcomes
results <- result_svm %>%
  rbind(result_rf) %>%
  rbind(result_tree)  

# decision tree & random forest work best, svm a bit less
#### Choice for decision tree (only rules) - because easiest to interpret -->
#### we can proceed with using just rules.

# interpret decision tree
confusionMatrix(resultstree)
print(resultstree)
plot(resultstree$finalModel)
text(resultstree$finalModel)


