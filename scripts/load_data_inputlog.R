#------------------------------------------------------------------------------#
#                                                                              #
# Code to create a data frame with raw keystroke data from Inputlog            #                                                 
#                                                                              #
# Pre-requisites:                                                              #
#    - Zipfolder (here named Inputlogfiles.zip) with XML files from Inputlog   #
#      from all participants                                                   #
#                                                                              #
#------------------------------------------------------------------------------#

#load R packages
require(tidyverse)
require(rio)
require(XML)

#load necessary helper functions
source("functions/list_zipfiles.R")
source("functions/fix_zipfilenames.R")
source("functions/load_general.R")
source("functions/load_summary.R")

#Define XML file-name and path --adapt this if necessary--
xmlfolder <- "data/Inputlogfiles.zip" 

###------------ LOAD LOG DATA ----------------------------------------------####
# Make a temporary file (tf) and a temporary folder (tdir) to unzip
tf <- tempfile(tmpdir = tdir <- tempdir())

# list XML files and unzip them in temporary folder 
list_zipfiles(xmlfolder, analysis = "GA")

# fix special characters in filenames
files_ga <- fix_zipfilenames(tf)

# load all general (GA) logs and write them to a csv file
logs <- load_general(files_ga) %>%
  select(-RevisionInfo)
write.csv(logs, "data/all_logs.csv", row.names = F)

# Delete temporary files
unlink(tdir, T, T)


###------------ LOAD SUMMARY SESSION INFO ----------------------------------####
# Make a temporary file (tf) and a temporary folder (tdir) to unzip
tf <- tempfile(tmpdir = tdir <- tempdir())

# list XML files and unzip them in temporary folder 
list_zipfiles(xmlfolder, analysis = "SU")

# fix special characters in filenames
files_su <- fix_zipfilenames(tf)

# load all summary (SU) logs and write them to a csv file
summary <- load_summary(files_su) 
write.csv(summary, "data/all_summary.csv")

# Delete temporary files
unlink(tdir, T, T)


