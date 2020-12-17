source("common.r")

# Preprocess latency data.

####################################
## Preprocessing / Sanity Checks. ##
####################################

clockDuration <- 1 / 3393e6

dataDir <- "../data/"
dataFolders <- dir(dataDir)
dataFolders <- paste(dataDir, "/", dataFolders[grepl("exp", dataFolders)], "/", sep = "")

allData.lat <- data.table()

progbar <- txtProgressBar(style = 3)

for (ii in 1:length(dataFolders)) {
    
    curFileList <- dir(dataFolders[ii])
    curFileList <- curFileList[grepl("histogram.*csv", curFileList)]
    
    setTxtProgressBar(progbar, ii / length(dataFolders))
    
    parts.folder <- 
        dataFolders[ii] %>%
        str_split("/") %>%
        unlist() %>%
        nth(length(.) - 1) %>% 
        str_split("-|_") %>% 
        unlist()
    
    # => beta
    currentMaxFrameSize <- parts.folder[3] %>% as.numeric()
    # => pktsize
    currentPacketSize <- parts.folder[5] %>% as.numeric()
    # => L
    currentQueueSize <- parts.folder[7] %>% as.numeric()
    
    for (jj in 1:length(curFileList)) {
        
        parts <- str_match(
            curFileList[jj],
            "histogram-([[:alnum:]]+)-([[:digit:]]+)\\.csv")
        
        currentRate <- parts[3] %>% as.numeric()
        currentExpType <- parts[2]
        currentRepetition <- 1
        
        currentFilePath <- paste(dataFolders[ii], curFileList[jj], sep = "")
        
        currentData <- 
            fread(currentFilePath, col.names = c("latns", "n")) %>%
            uncount(n) %>% 
            mutate(rate = currentRate,
                   rep = currentRepetition,
                   exptype = currentExpType,
                   maxfs = currentMaxFrameSize,
                   pktsize = currentPacketSize,
                   qsize = currentQueueSize)
        
        allData.lat <- rbind(allData.lat, currentData)
        
    }
}

allData.lat <- 
    allData.lat %>% 
    mutate(
        exptype = factor(exptype),
        maxfs = factor(maxfs),
        rep = factor(rep),
        ratef = factor(rate))

# Aggregated latency information.
allData.lat.agg <- 
    allData.lat %>% 
    group_by(exptype, maxfs, pktsize, qsize, rate) %>% 
    summarise(
        medlat = median(latns / 1e3),
        meanlat = mean(latns / 1e3),
        cvlat = sd(latns) / mean(latns)) %>% 
    pivot_longer(medlat:cvlat, names_to = "metric", values_to = "val")
