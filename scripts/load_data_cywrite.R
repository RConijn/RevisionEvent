#------------------------------------------------------------------------------#
#                                                                              #
# Code to create a data frame with raw keystroke data from CyWrite             #
#                                                                              #
# Pre-requisites:                                                              #
#    - Zipfolder (here named automatic.zip) & (fixations.zip) with json        #
#      files for keystrokes and eyetracking from CyWrite                       #
#                                                                              #
#------------------------------------------------------------------------------#

#load R packages
library(tidyr)
library(rjson)
library(rio)
options(scipen = 1000)

################ KEYSTROKE DATA
# read zip file
json_list <- import_list("automatic.zip")
json_files <- do.call(rbind, json_list)

files <- json_files %>%
  # add rownames as column
  mutate(token = gsub("\\..*", "", rownames(json_files))) %>%
  rename(start_iki = iki,
         rev_no = index) %>%
  # expand iki in subsequent rows
  unnest(cols = ikis) %>%
  # add number per character in the revision
  group_by(token, rev_no) %>% 
  mutate(rev_char_no = row_number(),
         type = "keystroke")

# note "" if the operation was a deletion

# add manual annotations of revisions
annotation <- import("data/all_data_full2.csv") %>%
  rename(token = Token) %>%
  select(rev_no:fileno, -X, revision_end) %>%
  #fix coding error
  mutate(revision = ifelse(revision == 0 & rev_no == 54 & fileno == 13,
                           1, revision))

# combine the two data sets
all_data <- annotation %>%
  full_join(files, by = c("token", "rev_no")) 

# write final set
export(all_data, "data/all_keystrokes.csv")


################ EYE TRACKING
# read zip file 
json_list_eye <- import_list("fixations.zip")
json_files_eye <- do.call(rbind, json_list_eye)

files_eye <- json_files_eye %>%
  # add rownames as column
  mutate(token = gsub("fixations_(.+)\\..*", "\\1", rownames(json_files_eye)),
         type = "fixations") 

# write final set
export(files_eye, "data/all_fixations.csv")

