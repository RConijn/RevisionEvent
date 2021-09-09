#------------------------------------------------------------------------------#
#                                                                              #
# Code to inspect and interpret the machine learned models of revision end     #
#                                                                              #
# Pre-requisites:                                                              #
#    - ML models (in .rds format) from predict_revision_end.R                  #
#                                                                              #
#------------------------------------------------------------------------------#

#load R packages

library(tidyverse)
library(rio)
options(scipen = 1000)


#load models
#eyekeyrf <- readRDS("models/rf_eyekey_10cv.rds")
#keyrf <- readRDS("models/rf_eyekey_10cv.rds")
eyekeytree <- readRDS("models/tree_eyekey_10cv.rds")

# interpret outcomes
confusionMatrix(eyekeytree) # pretty useless, most will be always predicted as 0
print(eyekeytree)
plot(eyekeytree$finalModel) # only for rpart
text(eyekeytree$finalModel) # only for rpart

# get feature importance
varImp(eyekeytree)
varimp <- varImp(eyekeyrf)
varImp(keyrf)

varimp2 <- varimp$importance %>% 
  filter(Overall > 15) %>%
  rownames_to_column() %>%
  rename("Variable" = rowname,
         "Importance" = Overall) %>%
  arrange((Importance), Variable) %>%
  mutate(Variable = factor(Variable, ordered = T,
                           levels = c("duration", "n_ins", 
                                      "n_ins_sofar", "iki", 
                                      "dist_ins_del", "n_ins_words_sofar",
                                      "start_iki", "time_start", "rev_no",
                                      "n_ins_words", "offset_start", 
                                      "offset_start_final", "fix_dur",
                                      "is_delete", "saccade", "is_character"),
                           labels =
                             c("Duration", "Number of inserted characters",
                               "Number of inserted characters so far",
                               "Interkeystroke interval",
                               "String distance typed and deleted",
                               "Number of inserted words so far",
                               "Start interkeystroke interval",
                               "Time revision start", "Revision number",
                               "Number of inserted words", 
                               "Number of characters produced from the start",
                               "Number of characters from start of product",
                               "Fixation duration",
                               "Keystroke is deletion",
                               "Saccade length", "Keystroke is character"))) 

# plot feature importance (Figure 3)
ggplot(varimp2) +  
  geom_col(aes(x = fct_rev(Variable), y = Importance)) +
  coord_flip()+
  theme_bw()+
  theme(text = element_text(size=14)) +
  xlab("Variable")


# ----only for rpart models----
# plot decision tree (Figure 2)
plot(resulttree$finalModel) 
text(resulttree$finalModel) 