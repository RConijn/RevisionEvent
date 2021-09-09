#------------------------------------------------------------------------------#
#                                                                              #
# Code to pre-process the fixation data from CyWrite                           #
#                                                                              #
# Pre-requisites:                                                              #
#    - all_rev_end.csv from get_revision_end.R                                 #
#    - all_fixations.csv from load_data_cywrite.R                              #
#                                                                              #
#------------------------------------------------------------------------------#

#load R packages
library(tidyverse)
library(rio)
library(magrittr)

# load data
files_eye <- import("data/all-fixations.csv") 
files_keys <- import("data/all-rev_end.csv") 

# pre-process eye tracking data
eye <- files_eye %>% drop_na(offset) %>%
  group_by(token, k) %>%
  dplyr::mutate(fix_idx = 1:n()) %>%
  filter(k == "end") %>% 
  group_by(token) %>% 
  dplyr::mutate(t = as.double(t),    
    t = t - min(t) + 2,
    offset = as.integer(offset)) %>% select(-k,-type) %>%
  dplyr::rename(fix_dur = dur)

# Recode fixations into saccades
# Saccade is the distance between fixation and previous fixation in number of 
# characters. Fixation terminated by a saccade of type saccade_from
eye_sacc <- eye %>% 
  group_by(token) %>%
  dplyr::mutate(saccade = offset - lag(offset),
         saccade_to = if_else(saccade < 0, "regr", 
                                if_else(saccade > 0, "progr", 
                                        if_else(saccade == 0, "stat", ""))),
         saccade_from = lead(saccade_to),
         saccade = lead(saccade)) %>% select(-saccade_to) %>%
  dplyr::mutate(n = sequence(rle(saccade_from)$lengths)) %>% select(-n) %>%
  ungroup()


# Get leading edge indicator
eye_sacc$leading_edge <- FALSE
for (tok in unique(eye_sacc$token)) {
  tmp <- filter(eye_sacc, token == tok)
  min_offs <- eye_sacc[eye_sacc$token == tok,][1,]$offset
  eye_sacc[eye_sacc$token == tok,][1,]$leading_edge <- TRUE
  for(i in 2:nrow(tmp)){
    proposal <- eye_sacc[eye_sacc$token == tok,][i,]$offset
    if(proposal >= min_offs){
      eye_sacc[eye_sacc$token == tok,][i,]$leading_edge <- TRUE
      min_offs <- proposal
    }
  }
}


# For multiple fixations without changing z extract only the last fixation
# so we have the saccade associated with when editing continues.
eye_sacc_filtered <- eye_sacc %>%
  group_by(token, z) %>%
  dplyr::mutate(z = z - 1, # z_fixation will almost always be z_insertion + 1
                n = 1:n(),
                max_n = max(n)) %>% 
  filter(n == max_n) %>% select(-n,-max_n) %>% ungroup()

# Join data frames (key and eye)
eye_key <- left_join(files_keys, eye_sacc_filtered, by = c("z", "token")) 


# write data
write_csv(eye_key, "data/all_eye_keys.csv")



