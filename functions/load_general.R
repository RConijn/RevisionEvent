# Function to load General XML files and combine them into one dataframe
load_general <- function(filenames) {
  do.call(bind_rows, lapply(filenames, function(filename) {
    print(filename)
    data <- xmlParse(filename)
    # revision analysis only (only subtracts first 'revision event')
    revisions <-  xmlToDataFrame(data, 
                                 nodes = getNodeSet(data,
                                       "//session/event/RevisionInfo[1]" ), 
                                 stringsAsFactors = FALSE)
    
    
    # general analysis without revisions
    data2 <- xmlParse(filename)
    # remove revision analysis (if available for event)
    remchild <- function(i) {
      for(i in 1:length(nodes_all)){
      if("RevisionInfo" %in% names(nodes_all[[i]])){
        removeChildren(nodes_all[[i]], "RevisionInfo")
      }else{
        nodes_all[[i]]
      }}
    }
    nodes_all <- getNodeSet(data2, "//session/event")
    nodes_minusrev <- xpathSApply(data2, "//session/event", fun = remchild())
    events <- xmlToDataFrame(data2, nodes = nodes_minusrev, 
                             stringsAsFactors = FALSE)
    
    # combine revision analysis with general analysis
    # add empty row (if revision info not present for last event)
      if(nrow(revisions)==nrow(events)){
        events <- cbind(events, revisions)
        
        }else if(nrow(revisions) + 1==nrow(events)) {
            revisions[nrow(revisions) + 1, ] <- NA
            events <- cbind(events, revisions)
     }
      
      
    sessioninfo <- as.data.frame(
      do.call(rbind, xpathApply(data, "//entry", xmlAttrs)),
      stringsAsFactors = FALSE) %>%
      spread("name", "value")
    
    # rename variable names for different versions of Inputlog
    # 1. all variable names lower case & remove whitespacing surrounding names
    names(sessioninfo) <- tolower(names(sessioninfo))
    names(sessioninfo) <- trimws(names(sessioninfo))
    # 2. all variable names to English
    if("taak" %in% names(sessioninfo)){
      sessioninfo <- sessioninfo %>% 
        rename(task = taak)  }
    # 3. remove groep variable (same as group)
    if("group" %in% names(sessioninfo)){
      sessioninfo$group <- NA}
    
    all <- cbind(events, sessioninfo)  
  }))
}