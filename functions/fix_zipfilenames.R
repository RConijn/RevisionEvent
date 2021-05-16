# Function to rename filenames with special characters in temporary folder
fix_zipfilenames <- function(unzippedfolder){
  invisible(sapply(list.files(tf, recursive = T, full.names = T), 
                   FUN = function(eachPath){
                     file.rename(from = eachPath,
                                 to = sub(pattern="ÃÂ«|ÃÂ©", replacement="e", 
                                          eachPath))
                   }))
  
  # output filenames
  list.files(tf, recursive = T, full.names = T)
}
