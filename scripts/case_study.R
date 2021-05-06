library(tidyverse)
library(rio)
library(rjson)
library(ggplot2)
library(caret)
library(stringdist)
library(stringr)
options(scipen = 1000)

################ Import dataset ################################################
## keystrokes
# read zip file
json_list <- import_list("casestudy/automatic.zip")
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

# write final set
export(files, "data/case_keys.csv")


## Eye tracking
# read zip file 
json_list_eye <- import_list("casestudy/fixations.zip")
json_files_eye <- do.call(rbind, json_list_eye)

files_eye <- json_files_eye %>%
  # add rownames as column
  mutate(token = gsub("fixations_(.+)\\..*", "\\1", rownames(json_files_eye)),
         type = "fixations") 

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
# Sorry about the for-loop
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
eye_key <- left_join(files, eye_sacc_filtered, by = c("z", "token")) 



## Add general info
general_list <- rjson::fromJSON(file = "casestudy/general_features.json") 
for (i in 1:length(general_list)){
  general_list[[i]]$summary <- NULL
}
general <- bind_rows(general_list)

general2 <- general %>%
  mutate(token = names(general_list))

conditions <- import("casestudy/dataset_description.txt") 
conditions2 <- conditions %>%
  pivot_longer(cols = -`Participant ID`, 
               names_to = "language", 
               values_to = "token") %>%
  mutate(language = ifelse(language == "Session ID L1 (Turkish)", "L1_Turkish",
                           "L2_English")) %>%
  rename(pid = `Participant ID`)

eye_key_gen <- left_join(eye_key, general2, by = "token") %>%
  left_join(conditions2, by = "token")

# write final set
export(eye_key_gen, "data/case_eyekeys_gen.csv")


################ Run 'true revision' model #####################################
eye_key_gen2 <- eye_key_gen %>%
  mutate(revision = ifelse(
    #1. The writer starts deleting OR
    n_del > 0 |
    #2. the writer moves the cursor to a different location in the text, 
    #except for the leading edge (excluding invisible characters), 
    #and then begins producing new characters
      (immediate == 0 & from_edge_chars != 0), 1, 0)
  ) %>%
  group_by(pid, language, rev_no) %>%
  summarize(revision = first(revision),
            ins = first(ins),
            del = first(del))

#summary table for article:
nrow(eye_key_gen2)
sum(eye_key_gen2$revision == 1)

#92.7% indicated as true revision
sum(eye_key_gen2$revision == 1)/nrow(eye_key_gen2)

eye_key_gen_sum <- eye_key_gen2 %>%
  group_by(pid, language) %>%
  summarize(nof_revision = sum(revision == 1)) %>%
  group_by(language) %>%
  summarize(
    mean_rev = mean(nof_revision),
    sd_rev = sd(nof_revision)
  )


################ Data pre-processing revision end ##############################
# add info
eye_key_add <- eye_key_gen %>%
  left_join(eye_key_gen2, by = c("rev_no", "ins", "del", "pid", "language")) %>%
  mutate(
    is_delete = ifelse(revision != 0 & text == "" & row_number() %in% 
                         c(1:n_del), 1, 0),
    n_del_act = sum(is_delete),
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
         no_eye_data = ifelse(is.na(fix_dur), 1, 0),
         static = as.numeric(saccade_from == "stat"),
         progressive = as.numeric(saccade_from == "progr"),
         regressive = as.numeric(saccade_from == "regr")
  )


## add revision starts with info
firstkey <- eye_key_gen %>%
  filter(rev_no != 1, rev_char_no == 1)  %>%
  #add_info
  mutate(starts_with_capital = as.numeric(substr(ins, 0, 1) %in% LETTERS),
         starts_with_space = as.numeric(substr(ins, 0, 1) == " "),
         starts_with_periodcomma = as.numeric(substr(ins, 0,1) %in% c(",", "."))) %>%
  select(token, language, pid, rev_no, starts_with_capital, starts_with_space,
         starts_with_periodcomma)

eye_key_add <- left_join(eye_key_add,firstkey)

# many NA values for eye-fixation data, set to zero
eye_key_add <- eye_key_add %>%
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

# write final set
export(eye_key_add, "data/case_eyekeys_add.csv")


################ Run revision end model ########################################
#load best model
bestmodel <- readRDS("models/rf_eyekey_10cv.rds")


test_pred <- predict(bestmodel, eye_key_add, type = "prob") 

test3 <- eye_key_add %>%
  cbind(test_pred) %>%
  group_by(language, pid, rev_no) %>%
  summarize(maxyes = max(yes))

# select the keystroke with the highest class probability
test4 <- eye_key_add %>%
  cbind(test_pred) %>%
  left_join(test3) %>%
  group_by(language, pid, rev_no) %>%
  mutate(
    predicted_Y = factor(ifelse(yes == maxyes, "yes", "no"),
                         levels = c("yes", "no")))

# summarize length of revisions
sum_revisions <- test4 %>%
  group_by(pid, language, rev_no) %>%
  summarize(
    chars_rev_pred = first(rev_char_no[predicted_Y == "yes"]),
  ) 

revisions_add <- sum_revisions %>%
  inner_join(test4) %>%
  filter(chars_rev_pred == rev_char_no) %>%
  select(del, chars_rev_pred, ins_sofar, n_del, n_ins_sofar, rev_char_no,
         dist_ins_del)


################ Interpret results #############################################
# summarize revision length
sum_rev <- revisions_add %>%
  group_by(language) %>%
  summarize(
    m_length = mean(chars_rev_pred),
    sd_length = sd(chars_rev_pred)
  )

#plot inserted vs. deleted characters
plot <- revisions_add %>%
  group_by(language) %>%
  rename(Language = language) %>%
  summarise(Deleted = mean(n_del),
            Inserted = mean(n_ins_sofar)) %>%
  pivot_longer(cols = c(Deleted, Inserted), 
               names_to = "Mean number of characters", 
               values_to = "Characters")

ggplot(plot, aes(x = Language, y = Characters, 
                 fill = `Mean number of characters`)) + 
  geom_bar(stat ="identity", position = 'dodge') +
  theme_bw()


## revision types
revisions_add2 <- revisions_add %>%
  mutate(n_words_del = str_count(del, "\\S+"),
         n_words_ins = str_count(ins_sofar, "\\S+"),
         revision_type = 
           #ifelse(n_words_del <= 1 & n_words_ins == 0 & n_del <= 3, "subword-level deletion",
           #ifelse(n_words_del == 0 & n_words_ins <= 1, "subword-level insertion",
           ifelse(n_words_del <= 1 & n_words_ins <= 1 & n_del <= 3, "subword-level revision",
           ifelse(n_words_del <= 2 & n_words_ins <= 2 , "word-level revision",
           ifelse(n_words_del > 2 & n_words_ins <= 1, "above word-level deletion",
           ifelse(n_words_del <= 1 & n_words_ins > 2, "above word-level insertion",
             "above word-level revision")))),
         revision_type = factor(revision_type, levels = c(
          # "subword-level deletion",
          # "subword-level insertion",
           "subword-level revision",
                                           "word-level revision",
                                           "above word-level deletion",
                                           "above word-level insertion",
                                           "above word-level revision")))

ggplot(revisions_add2, aes(x = language, fill = revision_type)) + 
  geom_bar(position = 'dodge') +
  theme_bw() +
  labs(x = "Language",
        y = "Number of revisions",
        fill = "Revision type") 


## typed = deleted
ggplot(revisions_add, aes(x = dist_ins_del, fill = language)) + 
  geom_histogram(alpha = 0.3, position = 'identity', binwidth = 2) +
  theme_bw() 
  

