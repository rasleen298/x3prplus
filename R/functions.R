#' Convert a list of x3d file into a data frame
#' 
#' x3d format consists of a list with header info and a 2d matrix of scan depths. 
#' fortify_x3p turn the matrix into a variable within a data frame, using the parameters of the header as necessary.
#' @param x3d a file in x3d format as return by function read.x3d
#' @return data frame with variables x, y, and value
#' @export
fortify_x3p <- function(x3d) {
  info <- x3d[[1]]
  
  df <- data.frame(expand.grid(x=1:info$num.pts.line, y=1:info$num.lines), 
                   value=as.vector(t(x3d[[2]])))
  df$x <- (df$x-1) * info$x.inc
  df$y <- (df$y-1) * info$y.inc
  
  attr(df, "info") <- info
  
  df
}

#' Convert a data frame into an x3d file
#' 
#' @param df A data frame produced by fortify_x3p
#' @return An x3d object
#' @export
unfortify_x3p <- function(df) {
    my.info <- attr(df, "info")
    my.lst <- list(header.info = my.info, 
                   surface.matrix = matrix(df$value, 
                                           nrow = length(unique(df$y)), 
                                           ncol = length(unique(df$x)),
                                           byrow = TRUE))
    
    return(my.lst)
}

#' Read a crosscut from a 3d surface file
#' 
#' @importFrom x3pr read.x3p
#' @param path path to an x3p file. The path will only be considered, if bullet is not specified.
#' @param x level of the crosscut to be taken. If this level does not exist, the crosscut with the closest level is returned.
#' @param bullet alternative access to the surface measurements. 
#' @return data frame 
#' @export
get_crosscut <- function(path = NULL, x = 243.75, bullet = NULL) {
  if (is.null(bullet)) bullet <- read.x3p(path)
  dbr111 <- fortify_x3p(bullet)
  
  pickx <- dbr111$x[which.min(abs(x - unique(dbr111$x)))]
  
  dbr111.fixx <- dbr111[dbr111$x == pickx,]
  
  return(dbr111.fixx)
}
  
#' Deprecated function use get_crosscut
#' 
#' @param path The path to the x3p file
#' @param x The crosscut value
#' @export
get_bullet <- function(path, x = 243.75) {
  cat("Use function get_crosscut instead of get_bullet\n\n")
  get_crosscut(path, x=x)
}

#' Find the grooves of a bullet land
#' 
#' @param bullet data frame with topological data
#' @param smoothfactor The smoothing window to use
#' @param smoothplot Whether to show smoothed data on the resulting plot
#' @param adjust positive number 
#' @export
#' @import ggplot2
#' @importFrom zoo rollapply
#' @importFrom zoo na.fill
get_grooves <- function(bullet, smoothfactor = 35, smoothplot = FALSE, adjust = 10) {
    value_filled <- na.fill(bullet$value, "extend")
    smoothed <- rollapply(value_filled, smoothfactor, function(x) mean(x))
    smoothed_truefalse <- rollapply(smoothed, smoothfactor, function(x) mean(x))
    
    lengthdiff <- length(bullet$value) - length(smoothed_truefalse)
    
    peak_ind_smoothed <- head(which(rollapply(smoothed_truefalse, 3, function(x) which.max(x) == 2)), n = 1)
    peak_ind <- peak_ind_smoothed + floor(lengthdiff / 2)
    groove_ind <- head(which(rollapply(tail(smoothed_truefalse, n = -peak_ind_smoothed), 3, function(x) which.min(x) == 2)), n = 1) + peak_ind

    peak_ind2_smoothed_temp <- head(which(rollapply(rev(smoothed_truefalse), 3, function(x) which.max(x) == 2)), n = 1)
    peak_ind2_temp <- peak_ind2_smoothed_temp + floor(lengthdiff / 2)
    groove_ind2_temp <- head(which(rollapply(tail(rev(smoothed_truefalse), n = -peak_ind2_smoothed_temp), 3, function(x) which.min(x) == 2)), n = 1) + peak_ind2_temp
    
    peak_ind2 <- length(bullet$value) - peak_ind2_temp + 1
    groove_ind2 <- length(bullet$value) - groove_ind2_temp + 1
    
    ## Check that it actually FOUND a groove...
    if (length(groove_ind) == 0 || groove_ind > 300) groove_ind <- 1
    if (length(groove_ind2) == 0 || groove_ind2 < length(bullet$value) - 300) groove_ind2 <- length(bullet$value)
    
    xvals <- bullet$y
    yvals <- bullet$value
    
    plot_peak_ind <- peak_ind
    plot_groove_ind <- groove_ind
    plot_peak_ind2 <- peak_ind2
    plot_groove_ind2 <- groove_ind2
    
    if (smoothplot) {
        xvals <- 1:length(smoothed_truefalse)
        yvals <- smoothed_truefalse
    }
    if (smoothplot) {
        plot_peak_ind <- peak_ind_smoothed
        plot_groove_ind <- groove_ind - floor(lengthdiff / 2)
        plot_peak_ind2 <- peak_ind2 - floor(lengthdiff / 2)
        plot_groove_ind2 <- groove_ind2 - floor(lengthdiff / 2)
    }
    
    p <- qplot(xvals, yvals) +
        theme_bw() +
        geom_vline(xintercept = xvals[plot_peak_ind], colour = "red") +
        geom_vline(xintercept = xvals[plot_groove_ind], colour = "blue") +
        geom_vline(xintercept = xvals[plot_peak_ind2], colour = "red") +
        geom_vline(xintercept = xvals[plot_groove_ind2], colour = "blue")
    
    return(list(groove = c(bullet$y[groove_ind + adjust], 
                           bullet$y[groove_ind2 - adjust]), plot = p))
}

#' Identify the location and the depth of peaks and heights at a crosscut
#' 
#' @param loessdata export from rollapply 
#' @param smoothfactor set to default of 35. Smaller values will pick up on smaller changes in the crosscut.
#' @return list of several objects: 
#' @importFrom zoo rollapply
#' @import ggplot2
#' @export
get_peaks <- function(loessdata, smoothfactor = 35) {
  y <- NULL
  xmin <- NULL
  xmax <- NULL
  
    smoothed <- rollapply(loessdata$resid, smoothfactor, function(x) mean(x))
    smoothed_truefalse <- rollapply(smoothed, smoothfactor, function(x) mean(x))
    
    test <- rollapply(smoothed_truefalse, 3, function(x) which.max(x)==2)
    test2 <- rollapply(smoothed_truefalse, 3, function(x) which.min(x)==2)

    
    peaks <- loessdata$y[which(test) + smoothfactor]
    valleys <- loessdata$y[which(test2) + smoothfactor]
    peaks.heights <- smoothed_truefalse[which(test) + 1]
    valleys.heights <- loessdata$resid[which(test2) + smoothfactor]
    
    # adding on some extra stats
    extrema <- c(peaks, valleys)
    heights <- c(peaks.heights, valleys.heights)
    type <- c(rep(1, length(peaks)), rep(-1, length(valleys)))
    idx <- order(extrema)
    extrema <- extrema[idx]
    heights <- heights[idx]
    type <- type[idx]
    diffs <- diff(extrema)
    lines <- data.frame(xmin = extrema-c(diffs[1],diffs)/3,
                        xmax = extrema+c(diffs,diffs[length(diffs)])/3, 
                        type = type, extrema = extrema, heights = heights)    
    dframe <- data.frame(y=loessdata$y[smoothfactor:(length(loessdata$y) - smoothfactor + 1)], smoothed=smoothed_truefalse)
    p <- qplot(data=dframe, x=y, y=smoothed, geom = "line") +
      theme_bw() +
      geom_rect(aes(xmin=xmin, xmax=xmax), ymin=-6, ymax=6, data=lines, colour="grey60", alpha=0.2, inherit.aes = FALSE) +
      geom_vline(xintercept = loessdata$y[which(test) + smoothfactor], colour = "red") +
      geom_vline(xintercept = loessdata$y[which(test2) + smoothfactor], colour = "blue") 
    
    return(list(peaks = peaks, valleys = valleys, extrema = extrema, 
                peaks.heights = peaks.heights, valleys.heights = valleys.heights, 
                lines=lines, plot = p))
}

#' Fit a LOESS model with bootstrap samples
#' 
#' @param bullet Bullet as returned from fortify_x3p
#' @param groove Groove as returned from get_grooves
#' @param B number of Bootstrap samples
#' @param alpha The significance level
#' @export
#' @importFrom plyr rdply
#' @importFrom dplyr summarize
boot_fit_loess <- function(bullet, groove, B=1000, alpha=0.95) {
  value <- NULL
  y <- NULL

  bullet_filter <- subset(bullet, !is.na(value) & y > groove$groove[1] & y < groove$groove[2])
  my.loess <- loess(value ~ y, data = bullet_filter)
  bullet_filter$fitted <- fitted(my.loess)
  bullet_filter$resid <- resid(my.loess)

  N <- nrow(bullet_filter)
  resids <- plyr::rdply(B, function(n) {
    bf <- bullet_filter[sample(N,N, replace=TRUE),]
    my.loess <- loess(value ~ y, data = bf)
    
    dframe <- data.frame(y=bullet_filter$y, fitted=predict(my.loess, newdata=bullet_filter))
    dframe$resid <- bullet_filter$value-dframe$fitted
    dframe
  })
  
  quantiles <- resids %>% group_by(y) %>% summarize(
    nas = sum(is.na(resid)),
    low = quantile(resid, probs=(1-alpha)/2, na.rm=TRUE),
    high = quantile(resid, probs=1 - (1-alpha)/2, na.rm=TRUE)
  )
  quantiles
}

#' Fit a loess curve to a bullet data frame
#' 
#' First, the surface measurements of the bullet land is trimmed to be within left and right groove as specified by vector \code{groove}.
#' A loess regression is fit to the remaining surface measurements and residuals are calculated.
#' The most extreme 0.25% of residuals are filtered from further consideration.
#' The result is called the signature of the bullet land.
#' @param bullet The bullet object as returned from fortify_x3p
#' @param groove vector of two numeric values indicating the location of the left and right groove. 
#' @return a list of a data frame of the original bullet measurements extended by loess fit, residuals, and standard errors and two plots: a plot of the fit, and a plot of the bullet's land signature. 
#' @export
fit_loess <- function(bullet, groove) {
  value <- NULL
  y <- NULL
  chop <- NULL
  
    bullet_filter <- subset(bullet, !is.na(value) & y > groove$groove[1] & y < groove$groove[2])
    my.loess <- loess(value ~ y, data = bullet_filter)
    bullet_filter$fitted <- fitted(my.loess)
    bullet_filter$resid <- resid(my.loess)
    bullet_filter$se <- predict(my.loess, se=TRUE)$se.fit
    
    # filter out most extreme residuals
    bullet_filter$abs_resid <-  abs(bullet_filter$resid)
    cutoff <- quantile(bullet_filter$abs_resid, probs = c(0.9975))
    bullet_filter$chop <- bullet_filter$abs_resid > cutoff
    
    bullet_filter <- subset(bullet_filter, !chop)
    
    poly <- with(bullet_filter, 
                 data.frame(x=c(y, rev(y)), 
                            y=c(resid-1.96*se, rev(resid+1.96*se))))
    p2 <- ggplot(aes(x=y, y=resid), data=bullet_filter) + 
 #     geom_polygon(aes(x=x,y=y), fill="#0066cc", data=poly) +
 #      geom_line(size=0.1) +
      geom_line()+ 
        theme_bw() 

    p1 <- qplot(data = bullet_filter, y, value) +
        theme_bw() +
        geom_smooth()


    #p2 <- qplot(data = bullet_filter, y, resid, geom="line") +
    #    theme_bw()
    
    return(list(data = bullet_filter, fitted = p1, resid = p2))
}

#' @importFrom plotly plot_ly
plot_3d_land <- function(path, bullet, groove, x = 99.84) {
    br111 <- read.x3p(path)
    inds <- which(bullet$y > groove$groove[1] & bullet$y < groove$groove[2])
    surfmat <- br111$surface.matrix
    
    plot_ly(z = surfmat[inds,], type = "surface")
}

#' Sample every X element of a data frame
#' 
#' Sample every X element of a data frame in x and y direction
#' @param dframe data frame with x and y variable
#' @param byxy (vector) of numeric value indicating the sapling resolution. If a single number, the same resolution is used for x and y.
#' @return subset of the input variable
#' @export
sample.x3d <- function(dframe, byxy=c(2,2)) {
  x <- NULL
  y <- NULL
  # use fortified data set
  # use only every byxy sample in x and y direction 
  if(length(byxy)==1) byxy <- rep(byxy, length=2)
  xn <- sort(as.numeric(unique(dframe$x)))
  yn <- sort(as.numeric(unique(dframe$y)))
  
  xseq <- xn[seq(1, length(xn), by=byxy[1])] 
  yseq <- yn[seq(1, length(yn), by=byxy[2])] 
  subset(dframe, (x %in% xseq) & (y %in% yseq))
}

#' Estimate center and radius
#' 
#' Assuming the variables x and y are describing points located on a circle, the function uses a likelihood approach to estimate center and radius of the circle.
#' @param x numeric vector of values
#' @param y numeric vector of values
#' @return three dimensional vector of the circle center (x0, y0) and the radius
#' @export
getCircle <- function(x, y) {
  nas <- which(is.na(y))
  x <- x[-nas]
  y <- y[-nas]
  
  mx <- mean(x)
  my <- mean(y)
  
  x <- x - mx
  y <- y - my
  
  c1 <- sum(x^3) + sum(x*y^2)
  c2 <- sum(x^2*y) + sum(y^3)
  
  syy <- sum(y^2)
  sxy <- sum(x*y)
  sxx <- sum(x^2)
  
  D <- sxx*syy - sxy^2
  
  a <- (c1*syy - c2*sxy)/(2*D)
  b <- (c2*sxx - c1*sxy)/(2*D)
  r <- mean((x-a)^2 + (y-b)^2)
  cbind(x0=a+mx, y0=b+my, radius=sqrt(r))
}

#' Estimate predictions and residuals for a circle fit of x and y
#' 
#' estimate a circle, find predictive values and resiudals. depending on specification, vertical (regular) residuals or orthogonal residuals are computed.
#' @param x vector of numeric values
#' @param y vector of numeric values
#' @param resid.method character, one of "response" or "ortho"(gonal)
#' @return data frame with predictions and residuals
#' @export
predCircle <- function(x, y, resid.method="response") {
  pars <- data.frame(getCircle(x, y))
  theta <- acos((x-pars$x0)/pars$radius)/pi*180
  ypred <- pars$y0+pars$radius*sin(theta/180*pi)
  
  dframe <- data.frame(ciry=ypred)
  if ("response" %in% resid.method) {
    dframe$resid = y-ypred
  }  
  if ("ortho" %in% resid.method) {
    dframe$oresid = sqrt( (y-pars$y0)^2 + (x - pars$x0)^2) - pars$radius
  }

  dframe
}


#' Estimate predictions and residuals for a smooth of x and y
#' 
#' Fit a smooth line throught x and y, find predictive values and resiudals. 
#' @param x vector of numeric values
#' @param y vector of numeric values
#' @return data frame with predictions and residuals
#' @export
predSmooth <- function(x, y) {
  dframe <- data.frame(x, y)

    # return NA if more than two thirds of the values are missing
  if (sum(is.na(y)) > 2*length(y)/3) {
    dframe$smPred <- NA
    dframe$smResid <- NA

    return(dframe)
  }
  data.lo <- loess(y~x)

  dframe$smPred <- predict(data.lo, newdata=dframe)
  dframe$smResid <- with(dframe, y - smPred)
  dframe
}

#' Process x3p file 
#' 
#' x3p file of a 3d topological bullet surface is processed at surface crosscut x, 
#' the bullet grooves in the crosscuts are identified and removed, and a loess smooth 
#' is used (see \code{?loess} for details) to remove the big structure. 
#' @param bullet file as returned from read.x3p
#' @param name name of the bullet
#' @param x (vector) of surface crosscuts to process. 
#' @return data frame
#' @importFrom dplyr bind_rows %>%
#' @export
processBullets <- function(bullet, name = "", x = 100) {
  crosscuts <- unique(fortify_x3p(bullet)$x)
  crosscuts <- crosscuts[crosscuts >= min(x)]
  crosscuts <- crosscuts[crosscuts <= max(x)]
  if (length(x) > 2) crosscuts <- crosscuts[crosscuts %in% x]
  
  list_of_fits <- lapply(crosscuts, function(x) {
    br111 <- get_crosscut(path = NULL, x = x, bullet = bullet)
    br111.groove <- get_grooves(br111)
    fit_loess(br111, br111.groove)$resid$data
  })
  lof <- list_of_fits %>% bind_rows
  
  data.frame(lof, bullet = name, stringsAsFactors = FALSE)
}
# processBullets <- function(paths, x = 100, check = FALSE) {
#   if (check) {
#     if (length(x) > 1) {
#       crosscuts <- x
#       LOF <- lapply(1:length(crosscuts), function(i) {
#         path <- paths[i]
#         
#         br111 <- get_crosscut(path, x = crosscuts[i])
#         br111.groove <- get_grooves(br111)
#         br111.groove$plot
#         lof <- fit_loess(br111, br111.groove)$resid$data
#         
#         lof$path <- path
#         path <- gsub("app.*//", "", as.character(path))
#         lof$bullet <- gsub(".x3p", "", path)
#         
#         #      browser()
#         lof
#       })
#       
#     } else {
#     
#     crosscuts <- sapply(paths, function(path)
#       bulletCheckCrossCut(path, distance=25, xlimits = c(x[1], 750))) 
#     
#     LOF <- lapply(1:length(crosscuts), function(i) {
#       path <- paths[i]
# 
#       br111 <- get_crosscut(path, x = crosscuts[i])
#       br111.groove <- get_grooves(br111)
#       br111.groove$plot
#       lof <- fit_loess(br111, br111.groove)$resid$data
# 
#       lof$path <- path
#       path <- gsub("app.*//", "", as.character(path))
#       lof$bullet <- gsub(".x3p", "", path)
#       
# #      browser()
#       lof
#     })
#     }
#   } else {
#     br111 <- read.x3p(paths[1])
#     crosscuts <- unique(fortify_x3p(br111)$x)
#     crosscuts <- crosscuts[crosscuts >= min(x)]
#     crosscuts <- crosscuts[crosscuts <= max(x)]
#     if (length(x) > 2) crosscuts <- crosscuts[crosscuts %in% x]
#   
#     LOF <- lapply(paths, function(path) {
#       list_of_fits <- lapply(crosscuts, function(x) {
#         br111 <- get_crosscut(path, x = x)
#         br111.groove <- get_grooves(br111)
#         br111.groove$plot
#         fit_loess(br111, br111.groove)
#       })
#       lof <- lapply(list_of_fits, function(x) x$resid$data) %>% bind_rows
#       lof$path <- path
#       path <- gsub("app.*//", "", as.character(path))
#       lof$bullet <- gsub(".x3p", "", path)
#       
#       lof
#     })
#   }
#   LOF %>% bind_rows()
# }

#' Predict smooth from a fit
#' 
#' @param x X values to use
#' @param y Y values to use
#' @param span The span of the loess fit
#' @param sub Subsample factor
#' @export
smoothloess <- function(x, y, span, sub = 2) {
  dat <- data.frame(x, y)
  indx <- sub *(1: (nrow(dat) %/% sub))
  subdat <- dat[indx, ]
  lwp <- with(subdat, loess(y~x,span=span))
  predict(lwp, newdata = dat)
}

#' Identifying a reliable cross section 
#' 
#' Should be changed: x should just indicate lower and upper limit. That is cleaner and should speed things up as well.
#' @param path path to an x3p file
#' @param distance positive numeric value indicating the distance between cross sections to use for a comparison
#' @param xlimits vector of values between which to check for cross sections in a stable region
#' @param minccf minimal value of cross correlation to indicate a stable region
#' @param span The span for the loess smooth function
#' @export
bulletCheckCrossCut <- function(path, distance=25, xlimits = c(50, 500), minccf = 0.9, span = 0.03) {
  get_cc <- function(x, mybullet) {
    pickx <- mybullet$x[which.min(abs(x - unique(mybullet$x)))]
    
    br111 <- mybullet[mybullet$x == pickx,]
    br111.groove <- get_grooves(br111)
    #    br111.groove$plot
    #    browser()
    dframe <- fit_loess(br111, br111.groove)$resid$data
    
    path <- gsub(".*//", "", as.character(path))
    dframe$bullet <- paste(gsub(".x3p", "", path), x)
    dframe
  }
  bullet <- read.x3p(path)
  dbr111 <- fortify_x3p(bullet)

  done <- FALSE
  x <- min(xlimits)
  first_cc <- get_cc(x, mybullet = dbr111)
  
  while(!done) {
    x <- x + distance
    second_cc <- get_cc(x, mybullet = dbr111)
    b2 <- rbind(first_cc, second_cc)
    lofX <- bulletSmooth(b2, span = span)
    ccf <- bulletAlign(lofX)$ccf
    if (ccf > minccf) { 
      done <- TRUE
      return (x - distance)
    } 
    first_cc <- second_cc
    if (x + distance > max(xlimits)) done <- TRUE
  } 
  return (NA)
}

# #' keep for backup right now
# bulletCheckCrossCutOld <- function(path, distance=25, x = seq(100, 225, by=distance)) {
#   crosscuts <- x
# #  lof <- processBullets(path, x = x, check=FALSE)
# 
#     
#   list_of_fits <- lapply(crosscuts, function(x) {
#     br111 <- get_crosscut(path, x = x)
#     br111.groove <- get_grooves(br111)
# #    br111.groove$plot
# #    browser()
#     fit_loess(br111, br111.groove)
#   })
#   lof <- lapply(list_of_fits, function(x) x$resid$data) %>% bind_rows
#   lof$path <- path
#   path <- gsub("app.*//", "", as.character(path))
#   lof$bullet <- gsub(".x3p", "", path)
#   
#   lof$bullet <- paste(lof$bullet, lof$x)
#   
#   ccfs <- sapply(1:length(crosscuts[-1]), function(i) {
#     b2 <- subset(lof, x %in% crosscuts[i:(i+1)])
#     lofX <- bulletSmooth(b2)
#     bulletAlign(lofX)$ccf
#   })
#   
#   idx <- which(ccfs > .9)
#   if(length(idx) == 0) {
#     return(bulletCheckCrossCutOld(path=path, x=x+100))
#   }
#   if (!is.null(idx)) 
#     return(crosscuts[idx[1]])
#   return(NULL)
# }


# #' Identify the number of maximum CMS between two bullet lands
# #' 
# #' @param bullet1, bullet2 paths to two lands
# #' @param crosscut integer value specifying which surface crosscut to use for the match
# #' @param thresholds vector of potential thresholds to choose from for optimizing the number of CMS
# #' @return list of matching parameters, data set of the identified striae, and the aligned data sets.
# #' @export
# bulletGetMaxCMS_old <- function(bullet1, bullet2, crosscut = 100, crosscut2 = NA, thresholds = seq(0.3, 1.5, by = 0.05), check = FALSE) {
#   if (!is.na(crosscut2)) lof <- processBullets(paths = c(bullet1, bullet2), x = c(crosscut, crosscut2), check=check)
#   else lof <- processBullets(paths = c(bullet1, bullet2), x = crosscut, check=check)
#   lof <- bulletSmooth(lof)
#   bAlign = bulletAlign(lof)
#   lofX <- bAlign$bullet  
#   threshold <- bulletPickThreshold(lofX, thresholds = thresholds)
#   
#   lines <- striation_identify_old(lofX, threshold = threshold)
#   maxCMS <- maxCMS(lines$match==TRUE)
#   list(maxCMS = maxCMS, threshold=threshold, ccf = bAlign$ccf, lag=bAlign$ccf, lines=lines, bullets=lofX)
# }  

#' Identify the number of maximum CMS between two bullet lands
#' 
#' @param lof1 dataframe of smoothed first signature
#' @param lof2 dataframe of smoothed second signature
#' @param span positive number  for the smoothfactor to use for assessing peaks. 
#' @return list of matching parameters, data set of the identified striae, and the aligned data sets.
#' @export
bulletGetMaxCMS <- function(lof1, lof2, span=35) {
    bullet <- NULL
    
  lof <- rbind(lof1, lof2)
  bAlign = bulletAlign(lof)
  lofX <- bAlign$bullet  
  
  b12 <- unique(lof$bullet)
  peaks1 <- get_peaks(subset(lofX, bullet==b12[1]), smoothfactor = span)
  peaks2 <- get_peaks(subset(lofX, bullet == b12[2]), smoothfactor = span)
  
  #qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
  #    theme_bw() +
  #    geom_rect(data=peaks1$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25)) +
  #    geom_rect(data=peaks2$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25))
  
  peaks1$lines$bullet <- b12[1]
  peaks2$lines$bullet <- b12[2]
  
  lines <- striation_identify(peaks1$lines, peaks2$lines)
  
  #   p <- qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
  #     theme_bw() +
  #     geom_rect(data=lines, aes(xmin=xmin, xmax=xmax, fill = factor(type)),  ymin=-6, ymax=6, inherit.aes = FALSE, alpha=I(0.25)) +
  #     ylim(c(-6,6)) +
  #     geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(lines, !match), inherit.aes = FALSE) +
  #     geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(lines, match), inherit.aes = FALSE) 
  
  maxCMS <- maxCMS(lines$match==TRUE)
  list(maxCMS = maxCMS, ccf = bAlign$ccf, lag=bAlign$lag, lines=lines, bullets=lofX)
}  

# bulletGetMaxCMSXXX <- function(bullet1, bullet2, crosscut1, crosscut2, span=35) {
#   bullet <- NULL
#   y <- NULL
#   xmin <- NULL
#   xmax <- NULL
#   type <- NULL
#   
#   lof1 <- processBullets(paths = bullet1, x = crosscut1, check=FALSE)
#   lof2 <- processBullets(paths = bullet2, x = crosscut2, check=FALSE)
#   
#   lof <- rbind(lof1, lof2)
#   lof <- bulletSmooth(lof)
#   bAlign = bulletAlign(lof)
#   lofX <- bAlign$bullet  
# 
#   b12 <- unique(lof$bullet)
#   peaks1 <- get_peaks(subset(lofX, bullet==b12[1]), smoothfactor = span)
#   peaks2 <- get_peaks(subset(lofX, bullet == b12[2]), smoothfactor = span)
# 
#   qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
#     theme_bw() +
#     geom_rect(data=peaks1$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25)) +
#     geom_rect(data=peaks2$lines, aes(xmin=xmin, xmax=xmax, fill=factor(type)), ymin=-5, ymax=5, inherit.aes = FALSE, alpha=I(0.25))
#   
#   peaks1$lines$bullet <- bullet1
#   peaks2$lines$bullet <- bullet2
#   
#   lines <- striation_identify(peaks1$lines, peaks2$lines)
#   
# #   p <- qplot(x=y, y=resid, geom="line", colour=bullet, data=lofX, group=bullet) +
# #     theme_bw() +
# #     geom_rect(data=lines, aes(xmin=xmin, xmax=xmax, fill = factor(type)),  ymin=-6, ymax=6, inherit.aes = FALSE, alpha=I(0.25)) +
# #     ylim(c(-6,6)) +
# #     geom_text(aes(x = meany), y= -5.5, label= "x", data = subset(lines, !match), inherit.aes = FALSE) +
# #     geom_text(aes(x = meany), y= -5.5, label= "o", data = subset(lines, match), inherit.aes = FALSE) 
#     
#   maxCMS <- maxCMS(lines$match==TRUE)
#   list(maxCMS = maxCMS, ccf = bAlign$ccf, lag=bAlign$lag, lines=lines, bullets=lofX)
# }  

#' Number of maximum consecutively matching striae
#' 
#' @param match is a Boolean vector of matches/non-matches
#' @return an integer value of the maximum number of consecutive matches
#' @export
#' @examples 
#' x <- rbinom(100, size = 1, prob = 1/3) 
#' CMS(x == 1) # expected value for longest match is 3
#' maxCMS(x==1)
maxCMS <- function(match) {
  cmsTable <- CMS(match)
  as.numeric(rev(names(cmsTable)))[1]
}

#' Table of the number of consecutive matches
#' 
#' @param match is a Boolean vector of matches/non-matches
#' @return a table of the number of the CMS and their frequencies
#' @export
#' @examples 
#' x <- rbinom(100, size = 1, prob = 1/3) 
#' CMS(x == 1) # expected value for longest match is 3
CMS <- function(match) {
  # number of consecutive matching striae
  
  y <- diff(match)
  # y is -1 if change from 1 to 0, 
  #       0 if unchanged
  #       1 if change from 0 to 1
  w <- c(0, y)[match][-1]
  
  z <- which(w == 1)
  z <- c(0,z,length(match[match]))
  
  return(table(diff(z)))
}

#' Smooth the surface of a bullet 
#' 
#' @param data data frame as returned by the function \code{processBullets}
#' @param span width of the smoother, defaults to 0.03
#' @param limits vector of the form c(min, max). Results will be limited to be between these values.
#' @return data frame of the same form as the input extended by the vector l30 for the smooth.
#' @importFrom dplyr mutate
#' @export
bulletSmooth <- function(data, span = 0.03, limits = c(-5,5)) {
  bullet <- NULL
  y <- NULL
  
  lof <- data %>% group_by(bullet) %>% mutate(
    l30 = smoothloess(y, resid, span = span)
  )
  lof$l30 <- pmin(max(limits), lof$l30)
  lof$l30 <- pmax(min(limits), lof$l30)
  lof
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

  ccf <- ccf(subLOFx1$val, subLOFx2$val, plot = FALSE, lag.max=150, 
             na.action = na.omit)
  lag <- ccf$lag[which.max(ccf$acf)]
  incr <- min(diff(sort(unique(subLOFx1$y))))
  
  subLOFx2$y <- subLOFx2$y +  lag * incr # amount of shifting 
  bullets <- rbind(data.frame(subLOFx1), data.frame(subLOFx2))
#  bullets$y <- bullets$y + miny # we can, but we don't have to shift the series back. This is rather cosmetic.
  list(ccf=max(ccf$acf), lag = lag * incr, bullets=bullets)
}

# 
# #' Pick threshold based on maximum number of CMS
# #' 
# #' @param data data frame consisting of at least two surface crosscuts as given by function \code{bulletAlign}.
# #' @param thresholds vector of thresholds under consideration. Typically in the range between 0.3 and 1.5 (with 5 being the maximum). 
# #' @return threshold value that maximizes CMS
# #' @export
# bulletPickThreshold <- function(data, thresholds) {
#   #  browser()  
#   CMS <- thresholds %>% lapply(function(threshold) {
#     lines <- striation_identify_old(data, threshold = threshold)
#     data.frame(threshold=threshold, maxCMS = maxCMS(lines$match))
#   }) %>% bind_rows()
#   
#   CMS$threshold[which.max(CMS$maxCMS)]
# }
# 

#' Match striation marks across two cross sections based on previously identified peaks and valleys
#' @param lines1 data frame as returned from get_peaks function. data frames are expected to have 
#' the following variables: xmin, xmax, group, type, bullet, heights
#' @param lines2 data frame as returned from get_peaks function. data frames are expected to have 
#' the following variables: xmin, xmax, group, type, bullet, heights
#' @return data frame of the same form as lines1 and lines2, but consisting of an additional variable of whether the striation marks are matches
#' @importFrom dplyr group_by %>% summarise
#' @importFrom reshape2 melt
#' @export
striation_identify <- function(lines1, lines2) {
  group <- NULL
  type <- NULL
  bullet <- NULL
  heights <- NULL
  n <- NULL
  
  lines <- rbind(lines1, lines2)
  lines <- lines[order(lines$xmin),]
  
  ml <- melt(lines, measure.vars=c("xmin", "xmax"))
  ml <- ml[order(ml$value),]
  ml$overlap <- c(1,-1)[as.numeric(ml$variable)]
  ml$gap <- cumsum(ml$overlap)
  
  idx <- which(ml$gap == 0)
  lines <- data.frame(xmin = ml$value[c(1,idx[-length(idx)]+1)], 
                      xmax = ml$value[idx])   
  ml$group <- 0
  ml$group[c(1, idx[-length(idx)]+1)] <- 1
  ml$group <- cumsum(ml$group)
  isMatch <- function(type, bullet) {
    if (length(unique(bullet)) != 2) return(FALSE)
    return (length(unique(type)) == 1) 
  }
  groups <- ml %>% group_by(group) %>% summarise(
    match = isMatch(type, bullet),
    size = n(),
    type = type[1],
    sdheights = sd(heights),
    heights = mean(heights))
  lines$match <- as.vector(groups$match)
  lines$type <- as.vector(groups$type)
  lines$type[!lines$match] <- NA
  lines$meany <- with(lines, (xmin+xmax)/2)
  lines$heights <- as.vector(groups$heights)
  lines$sdheights <- as.vector(groups$sdheights)
  lines
}

# #' Identify striation marks across two bullets
# #' 
# #' @param data dataset containing crosscuts of (exactly?) two bullets as given by \code{processBullets}.
# #' @param threshold where should the smoothed values be cut? Typically, residuals from the smooth have values in (-5,5). A default value of 0.75 is taken.
# #' @param limits vector of the form c(min, max) to indicate cut off values. Any values in the individual characteristics outside will be set to those limits.
# #' @return a data frame with information on all of the identified striation marks, and whether they match across the two bullets.
# #' @importFrom dplyr group_by %>% n summarise
# #' @export
# striation_identify_old <- function(data, threshold = 0.75, limits = c(-5,5)) {
#   # smooth
#   lofX <- bulletSmooth(data, span = 0.03, limits = limits)
#     
#   # cut at .75
# #  threshold <- .75
#   lofX$r05 <- threshold* sign(lofX$l30) * as.numeric(abs(lofX$l30) > threshold)
#   lofX$type <- factor(lofX$r05)
#   levels(lofX$type) <- c("groove", NA, "peak")
#   
#   matches <- lofX %>% group_by(y) %>% summarise(
#     potential = (length(unique(type)) == 1),
#     allnas = sum(is.na(type))/n(),
#     type1 = na.omit(type)[1],
#     type = paste(type, sep="|", collapse="|"),
#     n = n()
#   )
#   
#   matches$id <- cumsum(matches$allnas == 1) + 1
#   matches$lineid <- as.numeric(matches$allnas != 1) * matches$id
#   
#   isMatch <- function(id, type) {
#     if (id[1] == 0) return(FALSE)
#     #  browser()
#     types <- strsplit(type, split = "|", fixed=TRUE) 
#     t1 <- sapply(types, function(x) x[1])
#     t2 <- sapply(types, function(x) x[2])
#     if (all(t1 == "NA")) return(FALSE)
#     if (all(is.na(t2))) return(FALSE)
#     t2 <- na.omit(t2)
#     if (all(t2 == "NA")) return(FALSE)
#     
#     peak <- length(grep("peak", c(t1, t2))) > 0
#     groove <- length(grep("groove", c(t1, t2))) > 0
#     if (peak & groove) return(FALSE)
#     
#     return(TRUE)
#   }
#   
#   lines <- matches %>% group_by(lineid) %>% summarise(
#     meany = mean(y, na.rm=T),
#     miny = min(y, na.rm=T),
#     maxy = max(y, na.rm=T) + 1.5625,
#     match = isMatch(lineid, type),
#     type = type1[1]
#   )
#   subset(lines, lineid != 0)
# }
