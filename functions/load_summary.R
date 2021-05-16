# Function to load Summary XML files and combine them into one dataframe
load_summary <- function(filenames) {
  do.call(bind_rows, lapply(filenames, function(filename) {
    print(filename)
    data <- xmlParse(filename)
    
    # separate load different components from XML file
    proces_info <- as.data.frame(do.call(rbind, 
                                         xpathApply(data, "//module[@name='Process Information']//element", 
                                                    xmlAttrs)), stringsAsFactors = FALSE) %>%
      mutate(name = gsub("- Total", "Total keys", name),
             name = gsub("- Per", "Keys per", name),
             name = gsub("- ", "", name),
             name = gsub("^Per Minute", "Words Per Minute", name),
             name = paste0("proc_", name)) %>%
      spread("name", "value")
    
    if(length(xpathApply(data, "//module[@name='Product Information']//element",
                         xmlGetAttr,"content")) > 0){
      product_info <- as.data.frame(do.call(rbind, 
                                            xpathApply(data, "//module[@name='Product Information']//element", 
                                                       xmlAttrs)), stringsAsFactors = FALSE) %>%
        mutate(name = paste0("prod_", name),
               name = gsub("prod_Total Characters", "prod_Total initial", name)) %>%
        spread("name", "value") } else {
          product_info <- NA }
    if(length(xpathApply(data, "//module[@name='Product/Process']//element",
                         xmlGetAttr,"content")) > 0) {
      process_prod <- as.data.frame(do.call(rbind, 
                                            xpathApply(data, "//module[@name='Product/Process']//element", 
                                                       xmlAttrs)), stringsAsFactors = FALSE) %>%
        mutate(name = paste0("proc/prod_", name)) %>%
        spread("name", "value")
    } else {
      process_prod <- NA }
    process_time <- as.data.frame(do.call(rbind, 
                                          xpathApply(data, "//module[@name='Process Time']/block[@name= 'General']/element", 
                                                     xmlAttrs)), stringsAsFactors = FALSE)%>%
      mutate(name = paste0("time_", name)) %>%
      spread("name", "value")
    mode_keyboard <- as.data.frame(do.call(rbind, 
                                           xpathApply(data, "//module[@name= 'Writing Mode']/block[@name= 'Keyboard']/element", 
                                                      xmlAttrs)), stringsAsFactors = FALSE) %>%
      mutate(name = gsub("^Total", "Keyboard Total", name),
             name = paste0("wm_", name)) %>%
      spread("name", "value")
    mode_mouse <- as.data.frame(do.call(rbind, 
                                        xpathApply(data, "//module[@name= 'Writing Mode']/block[@name= 'Mouse']/element", 
                                                   xmlAttrs)), stringsAsFactors = FALSE) %>%
      mutate(name = gsub("^Total", "Mouse Total", name),
             name = paste0("wm_", name)) %>%
      spread("name", "value")
    
    # combine summary components
    summary <- cbind(proces_info, product_info, process_prod, process_time,
                     mode_keyboard, mode_mouse)
    
    # read session info
    sessioninfo <- as.data.frame(
      do.call(rbind, xpathApply(data, "//entry", xmlAttrs)),
      stringsAsFactors = FALSE) %>%
      spread("name", "value")
    
    
    # rename variable names for different versions of Inputlog
    # 1. all variable names lower case & remove whitespacing surronding names
    names(sessioninfo) <- tolower(names(sessioninfo))
    names(sessioninfo) <- trimws(names(sessioninfo))
    # 2. all variable names to English
    if("taak" %in% names(sessioninfo)){
      sessioninfo <- sessioninfo %>% 
        rename(task = taak)
    }else{
    }
    
    #combine all
    all <- cbind(sessioninfo, summary)  
  }))
}