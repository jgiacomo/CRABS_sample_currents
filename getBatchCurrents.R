getBatchCurrents <- function(runlog, crabsANA){
    # This function will use the CRABS analysis file to determine which
    # measurements are active and which have been disabled (based on the 14C).
    # Then it will find the mean he13C current for each sample using only the
    # active measurements (i.e. excluding the removed outliers).
    #
    # The runlog and crabsANA inputs should be pointers to the respective files
    # (i.e. file objects or character strings of the path to the file).
    
    library(dplyr)
    
    source("NEC_runlog_to_dataframe.R")
    
    # Create a data frame with the runlog row numbers which have disabled 14C.
    # In the .ana file, the disabled elements are between the "disabled" and 
    # subsequent "end" tags. Each row contains 3 comma separated integers:
    # the first is the runlog row number, the second is the disable bit for 14C,
    # the third is the disable bit for the d13C. 0 is disabled, 1 is enabled.
    # Also, the "first" and "last" tags in the analysis file are used to filter
    # the results to only the sample positions included in the analysis.
    
    dis14C <- readLines(crabsANA)
    firstSample <- as.integer(dis14C[which(dis14C=="first")+1])
    lastSample <- as.integer(dis14C[which(dis14C=="last")+1])
    beginDisabled <- which(dis14C=="disabled")+1
    endDisabled <- which(dis14C=="end")[which(dis14C=="end")>beginDisabled][1]-1
    dis14C <- dis14C[beginDisabled:endDisabled]
    dis14C <- strsplit(dis14C, split = ",")
    dis14C <- matrix(unlist(dis14C),ncol=3,byrow=T)
    dis14C <- as.data.frame(dis14C, stringsAsFactors=FALSE)[,1:2]
    names(dis14C) <- c("rowNum","enabled")
    dis14C$rowNum <- as.integer(dis14C$rowNum)
    dis14C$enabled <- as.integer(dis14C$enabled)
    
    # Create data frame from runlog
    runlogdf <- suppressWarnings(NECrunlogToDF(file=runlog,lab="accium"))
    
    # Add a rowNum column
    runlogdf$rowNum <- seq(1:nrow(runlogdf))
    
    # Filter only the runs included in the analysis
    rowsIncluded <- seq(firstSample, lastSample, 1)
    runlogdf <- filter(runlogdf, rowNum %in% rowsIncluded)
    
    # Join the disabled/enabled informaion from the .ana file to the runlog
    runlogdf <- left_join(runlogdf,dis14C,by="rowNum")
    
    # Convert the NAs produced by the join to "enabled" bits
    runlogdf[which(is.na(runlogdf$enabled)),]$enabled <- 1
    
    # Find the mean current for each sample using only the enabled runs
    results <- runlogdf %>% filter(enabled==1) %>%
        group_by(Pos,Sample.Name) %>%
        summarize(he12C = mean(he12C), he13C = mean(he13C))
    
    return(results)
}