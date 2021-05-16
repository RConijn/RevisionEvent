# Function to list all XML files in zipfolder and unzip them in temporary folder
list_zipfiles <- function(zipfolder, analysis = "GA"){
  
  # list files (depending on the analysis)
  files <- grep(paste0('output/.*_', analysis, '.*.xml'), 
                unzip(zipfolder, list=TRUE)$Name, 
                ignore.case=TRUE, value=TRUE)
  
  # unzip wanted XML files in temporary folder
  unzip(zipfolder, files=files, exdir = tf)
}