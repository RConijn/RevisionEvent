#------------------------------------------------------------------------------#
#                                                                              #
# Code to run rule-based revision event algorithm for manual annotation and    #
#    automated annotation for Inputlog data                                    #
#                                                                              #
# Pre-requisites:                                                              #
#    - csv file with all keystroke data from the general analysis (GA)         #
#      all_logs.csv from load_data_inputlog.R                                  #
#                                                                              #
#------------------------------------------------------------------------------#

#load R packages
require(tidyverse)
require(rio)
require(stringi)

# load file --adapt this if necessary--
logs <- read.csv("data/all_logs.csv")

###------------ EXTRACT REVISION EVENT -------------------------------------####

# Indicate deletion and insertion 
logs_extended <- logs %>%
  # remove typing/clicking outside word document
  filter(!(is.na(position) & !type %in% c("replacement", "insert")),
         type %in% c("insert", "keyboard", "replacement"),
         ) %>%
  select(logguid, id, type, output, positionFull, doclengthFull, charProduction,
         pauseTime, pauseLocationFull, startTime) %>%
  group_by(logguid) %>%
  mutate(
    arrowkeys = output %in% c("RIGHT", "LEFT", "DOWN", "UP"), 
    leadingedge = (positionFull == doclengthFull), 
    
    # Deletion
    deletion = ifelse((
      # backspace key or doclength reduced
      output == "BACK" | lead(doclengthFull) < doclengthFull) |
      #selection + backspace
      (type == "replacement" & lead(output) == "BACK" & 
         positionFull == lead(positionFull) & pauseTime != 0 &
         # selection resulted in reduced doc length
         lead(doclengthFull,2) < lead(doclengthFull)), 1, 0),
    # start new deletion if position is changed during deletion
    deletion = ifelse((type == "replacement" &
                           lag(output) == "BACK" & deletion == 1) | 
                        (((type == "replacement" & 
                               lag(type) == "replacement") | 
                               (output == "BACK" & 
                                  lag(output) == "BACK")) &
                               abs(lag(positionFull) - positionFull) > 1),
                      lag(deletion) + 1, deletion),
    deletion = ifelse(is.na(deletion), 0, deletion),
    
    # Insertion
    insertion = ifelse(type == "insert" |
                        (((lag(arrowkeys) | !(positionFull - lag(positionFull)) 
                          %in% c(0,1)) & !lag(deletion) &
                          type == "keyboard" &
                          !arrowkeys & !leadingedge & !deletion)), 1, 0), 
    insertion = ifelse(is.na(insertion), 0, insertion),
    revision_no = ifelse(deletion >= 1 | insertion >= 1,
                         cumsum(deletion-lag(deletion) > 0 | 
                                  row_number() == 1| 
                                  insertion-lag(insertion) > 0) - 1,
                         0),
    # add index for text production in between revisions
    text_prod = ifelse(deletion == 0 & insertion == 0, 1, 0),
    text_prod_no = ifelse(text_prod == 1,
                          cumsum(text_prod-lag(text_prod) > 0 | 
                                   row_number() == 1 | 
                                   (revision_no - lag(revision_no) == 1 & 
                                      lag(revision_no) != 0)),
                          0),
    # add rows for characters with pasted insertion
    typed_char = ifelse(type == "insert", gsub("\\[|\\]", "", output), 
                        NA),
    output_pasted = strsplit(as.character(typed_char), "")
  )

logs_extended0 <- logs_extended %>%
  unnest(output_pasted) %>%
  group_by(revision_no) %>%
  # update positionFull for pasted characters
    mutate(
      positionFull = ifelse(type == "insert",
                            cumsum(type == "insert") + positionFull + 1
                            - nchar(output),
                            positionFull),
      output = ifelse(type == "insert", output_pasted, output)
    ) %>%
  ungroup() 



###------------ ADD REVISION EVENT INFO FOR ANNOTATION ---------------------####
# extend log with revision information
logs_extended1 <- logs_extended0 %>%
  mutate(deleted_position = ifelse(deletion >= 1, positionFull - 1,
                                   NA),
         index = row_number()) 

# find location of deleted character
logs_extended2 <- logs_extended1 %>%
  rowwise() %>%  
  mutate(
    index_pos = list(which(logs_extended1$positionFull %in% 
                           deleted_position & logs_extended1$type == "keyboard"&
                           !logs_extended1$arrowkeys & 
                            logs_extended1$output != "BACK" &
                           !str_detect(logs_extended1$output, 
                                       "OEM|CTRL|SHIFT|ALT"))),
    deleted_char_index = ifelse(length(na.omit(which(unlist(index_pos) 
                                                     < index)))> 0, 
                                index_pos[(max(which(unlist(index_pos) 
                                                     < index)))], 
                                NA))

# output deleted character
logs_extended3 <- logs_extended2 %>%
  ungroup() %>%
  mutate(deleted_char = output[logs_extended2$deleted_char_index],
         deleted_selection = ifelse(type == "replacement" & deletion >= 1, 
                                    sub(".*] ", "", output), NA)) %>%
  select(-index, -deleted_char_index, -index_pos)


# aggregate table to summarize per revision event
revisions <- logs_extended3 %>%
  filter(revision_no != 0) %>%
  mutate(deleted_char = ifelse(deleted_char %in% c("SPACE", "TAB", "RETURN"), 
                               "_", deleted_char)) %>%
  group_by(logguid, revision_no) %>%
  summarize(
    length = n(),
    start_id = first(id),
    revtype = ifelse(first(insertion) >= 1, "insertion", "deletion"),
    removed_chars = stri_reverse(paste(na.omit(deleted_char), collapse= "")),
    removed_sel_chars = paste(na.omit(deleted_selection), collapse= ""),
    removed_chars = ifelse(!is.na(removed_sel_chars) & removed_sel_chars != "", 
                           gsub(" ", "_", removed_sel_chars), removed_chars),
    typed_chars = paste(na.omit(output_pasted), collapse= ""),
  ) %>%
  select(-removed_sel_chars)


# aggregate text production after a revision per revision event
text_production <- logs_extended3 %>%
  filter(text_prod_no != 0, type == "keyboard", !arrowkeys,
         !output %in% c("CAPS LOCK", "BACK", "RCTRL", "DELETE"), 
         !str_detect(output, "OEM|CTRL|SHIFT|ALT")) %>%
  mutate(output = ifelse(output %in% c("SPACE", "TAB", "RETURN"), 
                               "_", output)) %>%
  group_by(logguid, text_prod_no) %>%
  summarize(
    typed_text = paste(na.omit(output), collapse= "")) %>%
  mutate(revision_no = text_prod_no - 1) 


# combine text production with revision log
revision_annotation <- full_join(revisions, text_production, 
                                 by = c("logguid","revision_no")) %>%
  arrange(logguid, revision_no) %>%
  mutate(typed_text = ifelse(!is.na(typed_text) & !is.na(typed_chars), 
                             paste0(gsub(" ", "_", typed_chars), typed_text), 
                      ifelse(is.na(typed_chars), typed_text, 
                             typed_chars))) %>%
  select(-text_prod_no, -typed_chars, -length) %>%
  mutate(start_id = ifelse(is.na(start_id), 0, start_id))


# write revision table for manual annotation
# required output:
#Revision number
#Removed characters
#Typed characters
#Revision start (empty)
#Revision end (= typed characters)
write.csv(revision_annotation, "data/revision_annotation.csv")


# add revision counter
logs_extended4 <- logs_extended3 %>%
  filter(revision_no !=0) %>%
  group_by(revision_no) %>%
  mutate(
    rev_char_no = row_number()) 

# add text production counter
logs_extended5 <- logs_extended3 %>%
  filter(text_prod_no !=0) %>%
  group_by(text_prod_no) %>%
  mutate(
    prod_char_no = row_number()) 

# add typed text from revision_annotation table
logs_extended6 <- logs_extended3 %>%
  left_join(logs_extended4) %>%
  left_join(logs_extended5) %>%
  left_join(revision_annotation) %>%
  group_by(logguid) %>%
  mutate(typed_text = ifelse(revision_no == 0 & text_prod_no > 1,
                             NA, typed_text),
         typed_text = gsub("_", " ", typed_text)) %>%
  fill(typed_text) %>%
  fill(removed_chars) 

# write extended log file for automated annotation
write.csv(logs_extended6, "data/all_logs_extended.csv")



