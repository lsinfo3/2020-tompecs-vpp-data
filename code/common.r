library(bit64)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

# Return and export batch size dependent mean processing time (unit: 1e-8 sec).
# Unavailable / unobserved batch sizes are interpolated via a linear fit.
exportBatchSizeDependentMeanProcessingTime <- function(parsedData, filename = F, theFun = mean, mark = F, beta = 256, minN = 50) {
    meanE8.all <- parsedData %>%
        group_by(vecsize) %>%
        summarise(meanE8 = theFun(clocks) * clockDuration * 1e8, n = n()) %>%
        filter(n > minN) %>%
        select(vecsize, meanE8)
    # Use linear fit for vector sizes that have not been observed.
    e8.fit <- lm(meanE8 ~ vecsize, data = meanE8.all)
    e8.pred <- data.frame(meanE8 = predict(e8.fit, data.frame(vecsize = setdiff(0:beta, meanE8.all$vecsize))), vecsize = setdiff(0:beta, meanE8.all$vecsize))
    if (mark) {
        e8.join <- rbind(meanE8.all %>% mutate(type = "Meas"), e8.pred %>% mutate(type = "fit")) %>% arrange(vecsize)
    } else {
        e8.join <- rbind(meanE8.all, e8.pred) %>% arrange(vecsize)
    }
    if (is.character(filename)) {
        write.table(e8.join, file = filename, row.names = F, col.names = F, sep = ",")
    }
    return(e8.join)
}

# Convert bitrate [bps] and packet size [B] to packet interarrival time [s].
rate2iat <- function(theRate, packetSize = 64) {
    return((theRate / 8 / packetSize) ^ -1)
}

# Convert rate in Mbps to interarrival time in multiples of 1e-8sec (10ns).
rate2e8iat <- function(therate) {
    return((84 * 8) / (therate * 1e6) * 1e8)
}
