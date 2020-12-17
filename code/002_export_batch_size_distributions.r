source("common.r")

#############################################################
## Export batch size distributions for each configuration. ##
#############################################################

# Note: run 001_preproc_service_time_data.r prior to this.

clockDuration <- 1 / 3393e6

configs <-
    allData %>%
    group_by(exptype, pktsize, maxfs, qsize) %>%
    summarise(n = n()) %>% 
    mutate(maxfs = as.numeric(as.character(maxfs)))

progbar <- txtProgressBar(style = 3)

for (ii in 1:nrow(configs)) {
    
    setTxtProgressBar(progbar, ii / nrow(configs))
    
    curSlice <- 
        allData %>%
        filter(
            exptype == configs[ii, ]$exptype,
            pktsize == configs[ii, ]$pktsize,
            maxfs == configs[ii, ]$maxfs,
            qsize == configs[ii, ]$qsize) %>% 
        mutate(clocks = as.double(pt1))
    
    # Construct pdfs.
    allCombinations <- 
        expand.grid(
            vecsize = 0:configs[ii, ]$maxfs,
            rate = sort(unique(curSlice$rate)))
    
    nObservationsPerRateAndVecsize <-
        curSlice %>%
        group_by(rate, vecsize) %>%
        summarise(nVecMbps = n())
    
    nObservationsPerRateAndVecsize <-
        left_join(allCombinations, nObservationsPerRateAndVecsize) %>%
        replace_na(list(nVecMbps = 0))
    
    nObservationsPerRate <-
        curSlice %>%
        group_by(rate) %>%
        summarise(nMbps = n())
    
    probabilities <- left_join(nObservationsPerRateAndVecsize, nObservationsPerRate)
    probabilities$p <- probabilities$nVecMbps / probabilities$nMbps
    
    for (jj in 1:20) {
        fwrite(
            probabilities %>%
                filter(rate == jj * 500) %>%
                select(vecsize, p),
            file = sprintf("stats/vecsize__%s_pktsize_%d_beta_%d_L_%d_%05d_mbps.csv",
                           configs[ii, ]$exptype,
                           configs[ii, ]$pktsize,
                           configs[ii, ]$maxfs,
                           configs[ii, ]$qsize,
                           jj * 500),
            col.names = F)
    }
    
}

meanBatchSizes <- 
    allData %>%
    group_by(exptype, pktsize, maxfs, qsize, rate) %>%
    summarise(meanbs = mean(vecsize))
