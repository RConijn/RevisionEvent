#------------------------------------------------------------------------------#
#                                                                              #
# Code to add manually annotated revision end to CyWrite data                  #
#                                                                              #
# Pre-requisites:                                                              #
#    - all_keystrokes.csv from load_data_cywrite.R                             #
#                                                                              #
#------------------------------------------------------------------------------#

#load R packages
library(tidyverse)
library(rio)
library(caret)
library(stringdist)
library(stringr)
options(scipen = 1000)

# read data 
all_data <- import("data/all_keystrokes.csv") %>%
  select(-c(surface_change:annotator), -c(background:anot_keep))

# predict end [revision_ended]
all_data_short <- all_data %>%
  filter(revision == 1) %>%
  group_by(fileno, rev_no) %>%
  # the first are deletes, up to the number of deletions if text is ""
  mutate(revision_end = gsub("____", "_", revision_end),
         n_ins_bef_end = first(gregexpr("/|\\\\", revision_end))[1] - 1,
         is_delete = ifelse(revision != 0 & text == "" & row_number() %in% 
                              c(1:n_del), 1, 0),
         n_del_act = sum(is_delete),
         # add counter for revision end
         revision_ended2 = rev_char_no == n_ins_bef_end + n_del_act,
         no_end = sum(revision_ended2, na.rm = T) == 0,
         revision_ended = ifelse(no_end, rev_char_no == last(rev_char_no),
                                 revision_ended2)
  ) %>%
  select(-no_end, -revision_ended2) %>%
  ungroup()
    
# check revision ends
checkallended <- all_data_short %>%
  group_by(fileno, rev_no) %>%
  summarize(n = sum(revision_ended),
           revision_end_no = first(rev_char_no[revision_ended == TRUE]),
           rev_char_tot = max(rev_char_no),
           anot = first(revision_end)) %>%
  mutate(check_end = revision_end_no == rev_char_tot)


# write final set
export(all_data_short, "data/all_rev_end.csv")
