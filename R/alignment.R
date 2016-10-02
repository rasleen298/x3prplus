bulletAlign_nist <- function (data, value = "l30", mincor = .8)  {
    bullet <- NULL
    b12 <- unique(data$bullet)
    if (length(b12) != 2) 
        stop("Two surfaces should be compared\n\n")
    data$val <- data.frame(data)[, value]
    
    subLOFx1 <- subset(data, bullet == b12[1])
    subLOFx2 <- subset(data, bullet == b12[2])
    subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
    subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)
    
    whichmin <- which.min(c(length(subLOFx1$y), length(subLOFx2$y)))
    shorter <- list(subLOFx1$val, subLOFx2$val)[[whichmin]]
    longer <- list(subLOFx1$val, subLOFx2$val)[[3 - whichmin]]
    
    longer_na <- c(rep(0, length(shorter)), longer, rep(0, (length(shorter))))
    
    mycors <- NULL
    for (i in 1:(length(longer_na) - length(shorter))) {
        longersub <- longer_na[i:(i + length(shorter) - 1)]
        
        corval <- cor(shorter, longersub, use = "pairwise.complete.obs")
        
        mycors <- c(mycors, corval)
    }
    
    lag <- which.max(mycors) - length(shorter)
    if (max(mycors, na.rm = TRUE) < mincor) lag <- 0
    
    incr <- min(diff(sort(unique(subLOFx1$y))))
    
    mydat <- if (whichmin == 1) subLOFx1 else subLOFx2
    mydat2 <- if (whichmin == 1) subLOFx2 else subLOFx1
    
    if (lag < 0) {
        mydat2$y <- mydat2$y + lag * incr
    } else {
        mydat$y <- mydat$y + lag * incr
    }
    
    bullets <- rbind(data.frame(mydat), data.frame(mydat2))
    list(ccf = max(mycors, na.rm = TRUE), lag = lag * incr, bullets = bullets)
}

bulletAlign_dtw <- function (data, value = "l30")  {
    bullet <- NULL
    b12 <- unique(data$bullet)
    if (length(b12) != 2) 
        stop("Two surfaces should be compared\n\n")
    data$val <- data.frame(data)[, value]
    miny <- min(data$y, na.rm = T)
    subLOFx1 <- subset(data, bullet == b12[1])
    subLOFx2 <- subset(data, bullet == b12[2])
    subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
    subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)
    
    whichmin <- which.min(c(length(subLOFx1$val), length(subLOFx2$val)))
    minval <- min(c(length(subLOFx1$value), length(subLOFx2$value)))
    shorter <- list(subLOFx1$value, subLOFx2$value)[[whichmin]]
    longer <- list(subLOFx1$value, subLOFx2$value)[[3 - whichmin]]
    
    regular_nona <- longer
    regular_nona[is.na(regular_nona)] <- 0
    degraded_nona <- shorter
    degraded_nona[is.na(degraded_nona)] <- 0
    
    #qplot(y, value, data = b1sub, colour = I("red"), geom = "line") + theme_bw() +
    #    geom_line(data = b2sub, aes(x = y, y = value), colour = I("blue"))
    
    alignment <- dtw(regular_nona, degraded_nona)
    
    mapping <- data.frame(index1 = alignment$index1, index2 = alignment$index2)
    mapping <- mapping %>%
        group_by(index1) %>%
        summarise(index2_new = max(index2))
    result <- mapping %>% group_by(index2_new) %>% summarise(index1 = tail(index1, n = 1))
    mydiffs <- apply(result, 1, diff)
    
    #qplot(b1sub$y, b1sub$value, colour = I("red"), geom = "line") + 
    #   geom_line(aes(x = b2sub$y, y = b2sub$value), colour = I("blue"))
    
    #result <- which.max(table(alignment$index2)[!(names(table(alignment$index2)) %in% tail(sort(unique(alignment$index2))))])
    #index.val <- min.ind <- as.numeric(names(result))
    #min.ind <- tail(which(alignment$index2 == index.val), n = 1)
    #min.ind <- 478
    #min.ind <- min(result$index1)
    lag <- as.numeric(names(table(mydiffs))[which.max(table(mydiffs))])
    incr <- min(diff(sort(unique(subLOFx1$y))))
    
    mydat <- if (whichmin == 1) subLOFx1 else subLOFx2
    mydat2 <- if (whichmin == 1) subLOFx2 else subLOFx1
    
    mydat$y <- mydat$y + lag * incr
    bullets <- rbind(data.frame(mydat), data.frame(mydat2))
    list(ccf = NA, lag = lag * incr, bullets = bullets)
}

#' Align two surface cross cuts according to maximal correlation
#' 
#' The bullet with the first name serves as a reference, the second bullet is shifted.
#' @param data data frame consisting of at least two surface crosscuts as given by function \code{bulletSmooth}.
#' @param value string of the variable to match. Defaults to l30, the variable returned from function \code{bulletSmooth}.
#' @return list consisting of a) the maximal cross correlation, b) the lag resulting in the highest cross correlation, and c) same data frame as input, but y vectors are aligned for maximal correlation between the 
#' @export
bulletAlign <- function(data, value = "l30") {
    bullet <- NULL
    b12 <- unique(data$bullet)
    
    if (length(b12) != 2) stop("Two surfaces should be compared\n\n")
    
    data$val <- data.frame(data)[, value]
    miny <- min(data$y, na.rm=T)
    
    subLOFx1 <- subset(data, bullet==b12[1])
    subLOFx2 <- subset(data, bullet==b12[2]) 
    
    # ccf assumes that both time series start at the same y
    # shift series into the same origin
    subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
    subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)
    
    ccf <- ccf(subLOFx1$val, subLOFx2$val, plot = FALSE, lag.max=1500, 
               na.action = na.omit)
    lag <- ccf$lag[which.max(ccf$acf)]
    incr <- min(diff(sort(unique(subLOFx1$y))))
    
    subLOFx2$y <- subLOFx2$y +  lag * incr # amount of shifting 
    bullets <- rbind(data.frame(subLOFx1), data.frame(subLOFx2))
    #  bullets$y <- bullets$y + miny # we can, but we don't have to shift the series back. This is rather cosmetic.
    list(ccf=max(ccf$acf), lag = lag * incr, bullets=bullets)
}

