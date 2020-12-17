source("common.r")

# Preprocess data and save interarrival times, processing time distributions, and vector size distributions.

####################################
## Preprocessing / Sanity Checks. ##
####################################

clockDuration <- 1 / 3393e6

dataDir <- "../data/"
dataFolders <- dir(dataDir)
dataFolders <- paste(dataDir, "/", dataFolders[grepl("exp", dataFolders)], "/", sep = "")

allData <- data.table()

progbar <- txtProgressBar(style = 3)

for (ii in 1:length(dataFolders)) {
    
    curFileList <- dir(dataFolders[ii])
    curFileList <- curFileList[grepl("clock.*dat", curFileList)]
    
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
            "clock-([[:alnum:]]+)-([[:digit:]]+)\\.dat")
        
        currentRate <- parts[3] %>% as.numeric()
        currentExpType <- parts[2]
        currentRepetition <- 1
        
        currentFilePath <- paste(dataFolders[ii], curFileList[jj], sep = "")
        
        currentData <- fread(currentFilePath,
                             select = c(4, 6, 8, 12),
                             col.names = c("t1", "vecsize", "t2", "t3")) %>%
            mutate(rate = currentRate,
                   rep = currentRepetition,
                   exptype = currentExpType,
                   maxfs = currentMaxFrameSize,
                   pktsize = currentPacketSize,
                   qsize = currentQueueSize,
                   t1 = as.integer64(t1),
                   t2 = as.integer64(t2),
                   t3 = as.integer64(t3)) %>%
            mutate(proctime = as.double(t3 - t1),
                   proctime2 = as.double(t3 - t2)) %>%
            filter(proctime > 0, vecsize <= currentMaxFrameSize) %>%
            # dt1 / dt3 correspond more closely to the paper's / model's definition of B since it's the
            # time between consecutive embeddings.
            mutate(dt1 = t1 - lag(t1, 1),
                   dt3 = t3 - lag(t3, 1),
                   pt1 = lead(t1, 1) - t1) %>%
            # FIXME: lead / lag issues with integer64 at margins..
            filter(dt1 > 0, dt3 > 0, pt1 < 600000, pt1 > 0)
        
        allData <- rbind(allData, currentData)
        
    }
}

allData <- 
    allData %>% 
    mutate(
        exptype = factor(exptype),
        maxfs = factor(maxfs),
        rep = factor(rep),
        ratef = factor(rate))

configs <-
    allData %>%
    group_by(exptype, pktsize, maxfs, qsize) %>%
    summarise(n = n()) %>% 
    mutate(maxfs = as.numeric(as.character(maxfs)))

############################################################################
## Create linear fits for the batch service times for all configurations. ##
############################################################################

progbar <- txtProgressBar(style = 3)

for (ii in 1:nrow(configs)) {
    
    setTxtProgressBar(progbar, ii / nrow(configs))
    
    ## Get data, filter outliers.
    curData <- allData %>%
        filter(
            exptype == configs[ii, ]$exptype,
            pktsize == configs[ii, ]$pktsize,
            maxfs == configs[ii, ]$maxfs,
            qsize == configs[ii, ]$qsize) %>% 
        mutate(clocks = as.double(pt1))
    
    curData.filtered <- data.frame()
    
    curData$isOutlier <- 0
    
    # Outlier detection per rate.
    for (kk in seq(500, 10e3, 500)) {
        
        winsize <- 100
        
        curSlice <- curData %>% 
            filter(rate == kk)
        
        medbs <- median(curSlice$vecsize)
        
        for (jj in 1:floor(nrow(curSlice) / 100)) {
            currange <- (((jj-1)*winsize)+1) : (jj * winsize)
            curmean <- mean(curSlice$vecsize[currange])
            # if (curmean > medbs * 1.1) {
            # Second condition to avoid being too strict at low rates.
            if (curmean > medbs * 1.1 && abs(curmean - medbs) > 1) {
                curSlice$isOutlier[currange] <- 1
            }
        }
        
        curData.filtered <- rbind(curData.filtered, curSlice)
        
        cat(sprintf("rate %d, outlier ratio: %f\n", kk, nrow(curSlice %>% filter(isOutlier == 1)) / nrow(curSlice)))
        
    }
    
    nrow(curData.filtered %>% filter(isOutlier == 1)) / nrow(curData.filtered)
    
    curData.filtered <- curData.filtered %>% 
        filter(isOutlier == 0)
    
    
    # Outlier detection done, export stats
    
    exportBatchSizeDependentMeanProcessingTime(
        curData.filtered,
        mark = F,
        beta = configs[ii, ]$maxfs,
        theFun = median,
        filename = 
            sprintf("stats/medianE8_filtered_moon_nosleep_all_%s_pktsize_%d_beta_%d_L_%d.csv",
                    configs[ii, ]$exptype,
                    configs[ii, ]$pktsize,
                    configs[ii, ]$maxfs,
                    configs[ii, ]$qsize))
    
}
