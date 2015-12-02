#' Convert a list of x3d file into a data frame
#' 
#' x3d format consists of a list with header info and a 2d matrix of scan depths. 
#' fortify_x3p turn the matrix into a variable within a data frame, using the parameters of the header as necessary.
#' @param x3d a file in x3d format as return by function read.x3d
#' @return data frame with variables x, y, and value
#' @export
fortify_x3p <- function(x3d) {
  require(x3pr)
  info <- x3d[[1]]
  
  df <- data.frame(expand.grid(x=1:info$num.pts.line, y=1:info$num.lines), 
                   value=as.vector(t(x3d[[2]])))
  df$x <- (df$x-1) * info$x.inc
  df$y <- (df$y-1) * info$y.inc
  df
}

#' @export
get_bullet <- function(path, x = 243.75) {
    br111 <- read.x3p(path)
    dbr111 <- fortify_x3p(br111)
    
    dbr111.fixx <- dbr111[dbr111$x == x,]
    
    return(dbr111.fixx)
}

#' @export
#' @importFrom zoo rollapply
get_grooves <- function(bullet, smoothfactor = 21) {
    smoothed <- c(rep(NA, floor(smoothfactor / 2)), rollapply(bullet$value, smoothfactor, function(x) mean(x, na.rm = TRUE)), rep(NA, floor(smoothfactor / 2)))

    smoothed_truefalse <- c(rep(NA, floor(smoothfactor / 2)), rollapply(smoothed, smoothfactor, function(x) mean(x, na.rm = TRUE)), rep(NA, floor(smoothfactor / 2)))
    
    peak_ind <- head(which(diff(smoothed_truefalse) < 0), n = 1)
    groove_ind <- head(which(diff(tail(smoothed_truefalse, n = -(peak_ind + 10))) > 0), n = 1) + peak_ind + 10
    
    peak_ind2 <- tail(which(diff(smoothed_truefalse) > 0), n = 1)
    groove_ind2 <- tail(which(diff(head(smoothed_truefalse, n = -(length(smoothed) - peak_ind2 + 10))) < 0), n = 1)
    
    ## Check that it actually FOUND a groove...
    if (groove_ind > 300) groove_ind <- 0
    if (groove_ind2 < length(smoothed) - 300) groove_ind2 <- 0
    
    p <- qplot(bullet$y, bullet$value) +
        theme_bw() +
        geom_vline(xintercept = bullet$y[peak_ind], colour = "red") +
        geom_vline(xintercept = bullet$y[groove_ind], colour = "blue") +
        geom_vline(xintercept = bullet$y[peak_ind2], colour = "red") +
        geom_vline(xintercept = bullet$y[groove_ind2], colour = "blue")
    
    return(list(groove = c(bullet$y[groove_ind + 5], 
                           bullet$y[groove_ind2 - 5]), plot = p))
}

#' @export
fit_loess <- function(bullet, groove) {
    bullet_filter <- subset(bullet, !is.na(value) & y > groove$groove[1] & y < groove$groove[2])
    my.loess <- loess(value ~ y, data = bullet_filter)
    bullet_filter$fitted <- fitted(my.loess)
    bullet_filter$resid <- resid(my.loess)
    
    # filter out most extreme residuals
    bullet_filter$abs_resid <-  abs(bullet_filter$resid)
    cutoff <- quantile(bullet_filter$abs_resid, probs = c(0.995))
    bullet_filter$chop <- bullet_filter$abs_resid > cutoff
    
    bullet_filter <- subset(bullet_filter, !chop)
    
    p2 <- qplot(data = bullet_filter, y, resid) +
        theme_bw() 

    p1 <- qplot(data = bullet_filter, y, value) +
        theme_bw() +
        geom_smooth()
    
    #p2 <- qplot(data = bullet_filter, y, resid, geom="line") +
    #    theme_bw()
    
    return(list(data = bullet_filter, fitted = p1, resid = p2))
}

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
#' @param paths file paths to the x3p files
#' @param x (vector) of surface crosscuts to process. 
#' @param check boolean indicating whether the crosscut should be checked for stability. Set to FALSE by default.
#' @return data frame
#' @export
processBullets <- function(paths, x = 100, check = FALSE) {
  if (check) {
    if (length(x) > 1) {
      crosscuts <- x
      LOF <- lapply(1:length(crosscuts), function(i) {
        path <- paths[i]
        
        br111 <- get_bullet(path, x = crosscuts[i])
        br111.groove <- get_grooves(br111)
        br111.groove$plot
        lof <- fit_loess(br111, br111.groove)$resid$data
        
        lof$path <- path
        path <- gsub("app.*//", "", as.character(path))
        lof$bullet <- gsub(".x3p", "", path)
        
        #      browser()
        lof
      })
      
    } else {
    
    crosscuts <- sapply(paths, function(path)
      bulletCheckCrossCut(path, distance=25, x = seq(x[1], x[1]+100, by=25))) 
    
    LOF <- lapply(1:length(crosscuts), function(i) {
      path <- paths[i]

      br111 <- get_bullet(path, x = crosscuts[i])
      br111.groove <- get_grooves(br111)
      br111.groove$plot
      lof <- fit_loess(br111, br111.groove)$resid$data

      lof$path <- path
      path <- gsub("app.*//", "", as.character(path))
      lof$bullet <- gsub(".x3p", "", path)
      
#      browser()
      lof
    })
    }
  } else {
    br111 <- read.x3p(paths[1])
    crosscuts <- unique(fortify_x3p(br111)$x)
    crosscuts <- crosscuts[crosscuts >= min(x)]
    crosscuts <- crosscuts[crosscuts <= max(x)]
    if (length(x) > 2) crosscuts <- crosscuts[crosscuts %in% x]
  
    LOF <- lapply(paths, function(path) {
      list_of_fits <- lapply(crosscuts, function(x) {
        br111 <- get_bullet(path, x = x)
        br111.groove <- get_grooves(br111)
        br111.groove$plot
        fit_loess(br111, br111.groove)
      })
      lof <- lapply(list_of_fits, function(x) x$resid$data) %>% bind_rows
      lof$path <- path
      path <- gsub("app.*//", "", as.character(path))
      lof$bullet <- gsub(".x3p", "", path)
      
      lof
    })
  }
  LOF %>% bind_rows()
}

#' @export
smoothloess <- function(x, y, span, sub = 2) {
  dat <- data.frame(x, y)
  indx <- sub *(1: (nrow(dat) %/% sub))
  subdat <- dat[indx, ]
  lwp <- with(subdat, loess(y~x,span=span))
  predict(lwp, newdata = dat)
}

#' @export
bulletCheckCrossCut <- function(path, distance=25, x = seq(100, 225, by=distance)) {
  crosscuts <- x
  lof <- processBullets(path, x = x, check=FALSE)
  lof$bullet <- paste(lof$bullet, lof$x)
  
  ccfs <- sapply(1:length(crosscuts[-1]), function(i) {
    b2 <- subset(lof, x %in% crosscuts[i:(i+1)])
    lofX <- bulletSmooth(b2)
    bulletAlign(lofX)$ccf
  })
  
  idx <- which(ccfs > .875)
  if(length(idx) == 0) {
    return(bulletCheckCrossCut(path=path, x=x+100))
  }
  if (!is.null(idx)) 
    return(crosscuts[idx[1]])
  return(NULL)
}


#' Identify the number of maximum CMS between two bullet lands
#' 
#' @param bullet1, bullet2 paths to two lands
#' @param crosscut integer value specifying which surface crosscut to use for the match
#' @param thresholds vector of potential thresholds to choose from for optimizing the number of CMS
#' @return list of matching parameters, data set of the identified striae, and the aligned data sets.
#' @export
bulletGetMaxCMS <- function(bullet1, bullet2, crosscut = 100, crosscut2 = NA, thresholds = seq(0.3, 1.5, by = 0.05), check = FALSE) {
  if (!is.na(crosscut2)) lof <- processBullets(paths = c(bullet1, bullet2), x = c(crosscut, crosscut2), check=check)
  else lof <- processBullets(paths = c(bullet1, bullet2), x = crosscut, check=check)
  lof <- bulletSmooth(lof)
  bAlign = bulletAlign(lof)
  lofX <- bAlign$bullet  
  threshold <- bulletPickThreshold(lofX, thresholds = thresholds)
  
  lines <- striation_identify(lofX, threshold = threshold)
  maxCMS <- maxCMS(lines$match==TRUE)
  list(maxCMS = maxCMS, threshold=threshold, ccf = bAlign$ccf, lag=bAlign$ccf, lines=lines, bullets=lofX)
}  

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
#' @export
bulletSmooth <- function(data, span = 0.03, limits = c(-5,5)) {
  
  lof <- data %>% group_by(bullet) %>% mutate(
    l30 = smoothloess(y, resid, span = span)
  )
  lof$l30 <- pmin(max(limits), lof$l30)
  lof$l30 <- pmax(min(limits), lof$l30)
  lof
}

#' Align two surface cross cuts according to maximal correlation
#' 
#' @param data data frame consisting of at least two surface crosscuts as given by function \code{bulletSmooth}.
#' @param value string of the variable to match. Defaults to l30, the variable returned from function \code{bulletSmooth}.
#' @return list consisting of a) the maximal cross correlation, b) the lag resulting in the highest cross correlation, and c) same data frame as input, but y vectors are aligned for maximal correlation between the 
#' @export
bulletAlign <- function(data, value = "l30") {
  b12 <- unique(data$bullet)

  if (length(b12) != 2) stop("Two surfaces should be compared\n\n")
  
  data$val <- data.frame(data)[, value]
  
  subLOFx1 <- subset(data, bullet==b12[1])
  subLOFx2 <- subset(data, bullet==b12[2]) 
  
  # ccf assumes that both time series start at the same y
  # shift series into the same origin
  subLOFx1$y <- subLOFx1$y - min(subLOFx1$y)
  subLOFx2$y <- subLOFx2$y - min(subLOFx2$y)

  ccf <- ccf(subLOFx1$val, subLOFx2$val, plot = FALSE, lag.max=150, na.action = na.omit)
  lag <- ccf$lag[which.max(ccf$acf)]
  incr <- min(diff(sort(unique(subLOFx1$y))))
  
  subLOFx1$y <- subLOFx1$y -  lag * incr # amount of shifting should just be lag * y.inc
  bullets <- rbind(data.frame(subLOFx1), data.frame(subLOFx2))
  list(ccf=max(ccf$acf), lag = lag * incr, bullets=bullets)
}


#' Pick threshold based on maximum number of CMS
#' 
#' @param data data frame consisting of at least two surface crosscuts as given by function \code{bulletAlign}.
#' @param thresholds vector of thresholds under consideration. Typically in the range between 0.3 and 1.5 (with 5 being the maximum). 
#' @return threshold value that maximizes CMS
#' @export
bulletPickThreshold <- function(data, thresholds) {
  #  browser()  
  CMS <- thresholds %>% lapply(function(threshold) {
    lines <- striation_identify(data, threshold = threshold)
    data.frame(threshold=threshold, maxCMS = maxCMS(lines$match))
  }) %>% bind_rows()
  
  CMS$threshold[which.max(CMS$maxCMS)]
}

#' Identify striation marks across two bullets
#' 
#' @param data dataset containing crosscuts of (exactly?) two bullets as given by \code{processBullets}.
#' @param threshold where should the smoothed values be cut? Typically, residuals from the smooth have values in (-5,5). A default value of 0.75 is taken.
#' @param limits vector of the form c(min, max) to indicate cut off values. Any values in the individual characteristics outside will be set to those limits.
#' @return a data frame with information on all of the identified striation marks, and whether they match across the two bullets.
#' @export
striation_identify <- function(data, threshold = 0.75, limits = c(-5,5)) {
  # smooth
#  lofX <- data %>% group_by(bullet) %>% mutate(
#    l30 = smoothloess(y, resid, span = 0.03)
#  )
  lofX <- bulletSmooth(data, span = 0.03, limits = limits)
    
  # cut at .75
#  threshold <- .75
  lofX$r05 <- threshold* sign(lofX$l30) * as.numeric(abs(lofX$l30) > threshold)
  lofX$type <- factor(lofX$r05)
  levels(lofX$type) <- c("groove", NA, "peak")
  
  matches <- lofX %>% group_by(y) %>% summarise(
    potential = (length(unique(type)) == 1),
    allnas = sum(is.na(type))/n(),
    type1 = na.omit(type)[1],
    type = paste(type, sep="|", collapse="|"),
    n = n()
  )
  
  matches$id <- cumsum(matches$allnas == 1) + 1
  matches$lineid <- as.numeric(matches$allnas != 1) * matches$id
  
  isMatch <- function(id, type) {
    if (id[1] == 0) return(FALSE)
    #  browser()
    types <- strsplit(type, split = "|", fixed=TRUE) 
    t1 <- sapply(types, function(x) x[1])
    t2 <- sapply(types, function(x) x[2])
    if (all(t1 == "NA")) return(FALSE)
    if (all(is.na(t2))) return(FALSE)
    t2 <- na.omit(t2)
    if (all(t2 == "NA")) return(FALSE)
    
    peak <- length(grep("peak", c(t1, t2))) > 0
    groove <- length(grep("groove", c(t1, t2))) > 0
    if (peak & groove) return(FALSE)
    
    return(TRUE)
  }
  
  lines <- matches %>% group_by(lineid) %>% summarise(
    meany = mean(y, na.rm=T),
    miny = min(y, na.rm=T),
    maxy = max(y, na.rm=T) + 1.5625,
    match = isMatch(lineid, type),
    type = type1[1]
  )
  subset(lines, lineid != 0)
}
